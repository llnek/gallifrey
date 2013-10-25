;;
;; Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
;;
;; This library is distributed in the hope that it will be useful
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.tardis.io.triggers )

(import '(org.jboss.netty.handler.codec.http HttpResponseStatus))
(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))
(import '(org.jboss.netty.channel Channel ChannelFuture ChannelFutureListener))
(import '(org.jboss.netty.handler.codec.http
  HttpResponse HttpHeaders HttpHeaders$Names HttpVersion
  DefaultCookie
  CookieEncoder DefaultHttpResponse))
(import '(java.nio.channels ClosedChannelException))
(import '(java.io OutputStream IOException))
(import '(org.jboss.netty.handler.stream ChunkedStream))
(import '(java.util List Timer TimerTask))
(import '(java.net HttpCookie))
(import '(javax.servlet.http Cookie HttpServletRequest HttpServletResponse))
(import '(org.jboss.netty.buffer ChannelBuffer ChannelBuffers))
(import '(java.nio ByteBuffer))

(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.util NCMap))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.frwk.core Identifiable))
(import '(com.zotoh.gallifrey.io WebSockEvent WebSockResult HTTPEvent))
(import '(org.jboss.netty.handler.codec.http.websocketx
  WebSocketFrame
  BinaryWebSocketFrame
  TextWebSocketFrame))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.netty.comms :only [HTTP-CODES makeHttpReply closeCF] ])
(use '[comzotohcljc.util.core :only [make-mmap stringify notnil? Try!] ])
(use '[comzotohcljc.util.str :only [nsb] ])
(use '[comzotohcljc.tardis.io.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(defn isServletKeepAlive [^HttpServletRequest req]
  (let [ v (.getHeader req "connection") ]
    (= "keep-alive" (.toLowerCase (nsb v)))))

(defn- cookieToServlet ^Cookie [^HttpCookie c]
  (doto (Cookie. (.getName c) (.getValue c))
        (.setDomain (.getDomain c))
        (.setHttpOnly (.isHttpOnly c))
        (.setMaxAge (.getMaxAge c))
        (.setPath (.getPath c))
        (.setSecure (.getSecure c))
        (.setVersion (.getVersion c))) )

(defn- replyServlet [^comzotohcljc.util.core.MuObj res
                     ^HttpServletRequest req
                     ^HttpServletResponse rsp
                     src]
  (let [ ^OutputStream os (.getOutputStream rsp)
         ^NCMap hds (.getf res :hds)
         ^List cks (.getf res :cookies)
         ^URL url (.getf res :redirect)
         code (.getf res :code)
         data (.getf res :data)
         ^HttpResponseStatus
         status (get HTTP-CODES code) ]

    (when (nil? status) (throw (IOException. (str "Bad HTTP Status code: " code))))
    (try
      (.setStatus rsp code)
      (doseq [[^String nm vs] (seq hds)]
        (when-not (= "content-length" (.toLowerCase nm))
          (doseq [vv (seq vs) ]
            (.addHeader rsp nm vv))))
      (doseq [ c (seq cks) ]
        (.addCookie rsp (cookieToServlet c)))
      (cond
        (and (>= code 300)(< code 400))
        (.sendRedirect rsp (.encodeRedirectURL rsp (nsb url)))
        :else
        (let [ ^XData dd (cond
                            (instance? XData data) data
                            (notnil? data) (XData. data)
                            :else nil)
               clen (if (and (notnil? dd) (.hasContent dd))
                        (.size dd)
                        0) ]
            (.setContentLength rsp clen)
            (.flushBuffer rsp)
            (when (> clen 0)
                (IOUtils/copyLarge (.stream dd) os 0 clen)
                (.flush os) )))
      (catch Throwable e#
        (error e# ""))
      (finally
        (Try! (when-not (isServletKeepAlive req) (.close os)))
        (-> (ContinuationSupport/getContinuation req)
          (.complete))) ) ) )


(defn make-servlet-trigger ""

  [^HttpServletRequest req ^HttpServletResponse rsp src]

  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (replyServlet res req rsp src) )

    (resumeWithError [_]
        (try
            (.sendError rsp 500)
          (catch Throwable e#
            (error e# ""))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete)))) )) )

(defn- maybeClose [^HTTPEvent evt ^ChannelFuture cf]
  (when-not (.isKeepAlive evt)
    (when-not (nil? cf)
      (.addListener cf ChannelFutureListener/CLOSE ))))

(defn- cookieToNetty ^DefaultCookie [^HttpCookie c]
  (doto (DefaultCookie. (.getName c) (.getValue c))
        (.setDomain (.getDomain c))
        (.setHttpOnly (.isHttpOnly c))
        (.setMaxAge (.getMaxAge c))
        (.setPath (.getPath c))
        (.setSecure (.getSecure c))
        (.setVersion (.getVersion c))) )

(defn- cookiesToNetty ^String [^List cookies]
  (persistent! (reduce (fn [sum ^HttpCookie c]
                         (conj! sum
                                (-> (doto (CookieEncoder. true)
                                          (.addCookie (cookieToNetty c)))
                                    (.encode))))
                       (transient [])
                       (seq cookies)) ))

(defn- netty-ws-reply [^WebSockResult res ^Channel ch ^WebSockEvent evt src]
  (let [ ^XData xs (.getData res)
         bits (.javaBytes xs)
         ^WebSocketFrame
         f (cond
              (.isBinary res)
              (BinaryWebSocketFrame. (ChannelBuffers/wrappedBuffer bits))

              :else
              (TextWebSocketFrame. (nsb (stringify bits)))) ]
    (.write ch f)))


(defn- netty-reply "" [^comzotohcljc.util.core.MuObj res
                       ^Channel ch
                       ^HTTPEvent evt
                       src]
  (let [ cks (cookiesToNetty (.getf res :cookies))
         code (.getf res :code)
         rsp (makeHttpReply code)
         data (.getf res :data)
         hdrs (.getf res :hds) ]
    (with-local-vars [ clen 0 xd nil wf nil]
      (doseq [[^String nm vs] (seq hdrs)]
        (when-not (= "content-length" (.toLowerCase nm))
          (doseq [vv (seq vs)]
            (HttpHeaders/addHeader rsp nm vv))))
      (doseq [s cks]
        (HttpHeaders/addHeader rsp HttpHeaders$Names/SET_COOKIE s) )
      (cond
        (and (>= code 300)(< code 400))
        (HttpHeaders/setHeader rsp "Location" (nsb (.getf res :redirect)))
        :else
        (let [ ^XData dd (if (nil? data)
                            (XData.)
                            (cond
                              (instance? XData data) data
                              :else (XData. data))) ]
          (var-set clen (if (.hasContent dd) (.size dd) 0))
          (var-set xd dd)
          ))

      (when (.isKeepAlive evt)
        (HttpHeaders/setHeader rsp "Connection" "keep-alive"))

      (HttpHeaders/setContentLength rsp @clen)
      (var-set wf (.write ch rsp))
      (debug "wrote response headers out to client")

      (when (> @clen 0)
        (var-set wf (.write ch (ChunkedStream. (.stream ^XData @xd))))
        (debug "wrote response body out to client"))

      (debug "flushed last response content out to client")
      (closeCF (.isKeepAlive evt) wf)

      )))

(defn make-netty-trigger [^Channel ch evt src]
  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (cond
        (instance? WebSockEvent evt)
        (Try! (netty-ws-reply res ch evt src) )
        :else
        (Try! (netty-reply res ch evt src) ) ))

    (resumeWithError [_]
      (let [ rsp (makeHttpReply 500) ]
        (try
          (maybeClose evt (.write ch rsp))
          (catch ClosedChannelException e#
            (warn "ClosedChannelException thrown while flushing headers"))
          (catch Throwable t# (error t# "") )) ))

    ))


(defn make-async-wait-holder

  [ ^comzotohcljc.tardis.io.core.AsyncWaitTrigger trigger
    ^HTTPEvent event ]

  (let [ impl (make-mmap) ]
    (reify

      Identifiable
      (id [_] (.getId event))

      WaitEventHolder

      (resumeOnResult [this res]
        (let [ ^Timer tm (.mm-g impl :timer)
               ^comzotohcljc.tardis.io.core.EmitterAPI  src (.emitter event) ]
          (when-not (nil? tm) (.cancel tm))
          (.release src this)
          ;;(.mm-s impl :result res)
          (.resumeWithResult trigger res)
          ))

      (timeoutMillis [me millis]
        (let [ tm (Timer. true) ]
          (.mm-s impl :timer tm)
          (.schedule tm (proxy [TimerTask][]
            (run [] (.onExpiry me))) ^long millis)))

      (timeoutSecs [this secs]
        (timeoutMillis this (* 1000 secs)))

      (onExpiry [this]
        (let [ ^comzotohcljc.tardis.io.core.EmitterAPI
               src (.emitter event) ]
          (.release src this)
          (.mm-s impl :timer nil)
          (.resumeWithError trigger) ))

      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private triggers-eof nil)

