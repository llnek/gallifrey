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

  comzotohcljc.tardis.io.netty)

(import '(java.net HttpCookie URI URL InetSocketAddress))
(import '(java.net SocketAddress InetAddress))
(import '(java.util ArrayList List))
(import '(java.io IOException))

(import '(com.zotoh.gallifrey.io HTTPEvent HTTPResult))
(import '(javax.net.ssl SSLContext))
(import '(org.jboss.netty.handler.codec.http
  HttpRequest HttpResponse CookieDecoder CookieEncoder
  DefaultHttpResponse HttpVersion HttpHeaders
  HttpHeaders Cookie QueryStringDecoder))
(import '(org.jboss.netty.channel Channel))
(import '(org.jboss.netty.channel.group
  ChannelGroup))
(import '(org.jboss.netty.bootstrap ServerBootstrap))
(import '(com.zotoh.frwk.net NetUtils))

(import '(com.zotoh.frwk.core Hierarchial Identifiable))
(import '(com.zotoh.gallifrey.io IOSession WebSockEvent WebSockResult))
(import '(com.zotoh.frwk.io XData))
(import '(org.jboss.netty.handler.codec.http.websocketx
  WebSocketFrame
  WebSocketServerHandshaker
  WebSocketServerHandshakerFactory
  ContinuationWebSocketFrame
  CloseWebSocketFrame
  BinaryWebSocketFrame
  TextWebSocketFrame
  PingWebSocketFrame
  PongWebSocketFrame))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.io.core])
(use '[comzotohcljc.tardis.io.http])
(use '[comzotohcljc.tardis.io.triggers])
(use '[comzotohcljc.util.core :only [MuObj make-mmap notnil? conv-long] ])
(use '[comzotohcljc.netty.comms :only [ makeServerNetty finzNetty addListener
                                        makeRouteCracker] ])
(use '[comzotohcljc.tardis.io.ios :only [ make-session]])
(use '[comzotohcljc.util.seqnum :only [next-long] ])
(use '[comzotohcljc.util.mime :only [get-charset] ])
(use '[comzotohcljc.util.str :only [hgl? nsb strim nichts?] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- make-wsock-result []
  (let [ impl (make-mmap) ]
    (.mm-s impl :binary false)
    (.mm-s impl :text false)
    (.mm-s impl :data nil)
    (reify
      MuObj
      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl))

      WebSockResult
      (isBinary [_] (.mm-g impl :binary))
      (isText [_] (.mm-g impl :text))
      (getData [_] (XData. (.mm-g impl :data)))
      )))

(defn- cookieToJava [^Cookie c]
  (doto (HttpCookie. (.getName c)(.getValue c))
        (.setComment (.getComment c))
        (.setDomain (.getDomain c))
        (.setMaxAge (.getMaxAge c))
        (.setPath (.getPath c))
        (.setVersion (.getVersion c))
        (.setHttpOnly (.isHttpOnly c)) ))

(defn- make-wsock-event
  [^comzotohcljc.tardis.io.core.EmitterAPI co
                         ^Channel ch
                         ^XData rdata]
  (let [ ssl (notnil? (.get (NetUtils/getPipeline ch) "ssl"))
         ^InetSocketAddress laddr (.getLocalAddress ch)
         ^WebSockResult res (make-wsock-result)
         impl (make-mmap)
         eeid (next-long) ]
    (.mm-s impl :ios (make-session co ssl))
    (with-meta
      (reify
        MuObj

        (setf! [_ k v] (.mm-s impl k v) )
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k) )
        (clrf! [_ k] (.mm-r impl k) )
        (clear! [_] (.mm-c impl))

        Identifiable
        (id [_] eeid)

        WebSockEvent
        (bindSession [_ s] (.mm-s impl :ios s))
        (getSession [_] (.mm-g impl :ios))
        (getId [_] eeid)
        (isSSL [_] ssl)
        (isText [_] (instance? String (.content rdata)))
        (isBinary [this] (not (.isText this)))
        (getData [_] rdata)
        (getResultObj [_] res)
        (replyResult [this]
          (let [ ^comzotohcljc.tardis.io.core.WaitEventHolder
                 wevt (.release co this) ]
            (when-not (nil? wevt)
              (.resumeOnResult wevt res))))
        (emitter [_] co))

      { :typeid :czc.tardis.io/WebSockEvent } )))

(defmethod ioes-reify-event :czc.tardis.io/NettyIO
  [^comzotohcljc.tardis.io.core.EmitterAPI co & args]
  (let [ ^HTTPResult res (make-http-result co)
         ^HttpRequest req (nth args 1)
         rdata (nth args 2)
         ^Channel ch (nth args 0)
         ssl (notnil? (.get (NetUtils/getPipeline ch) "ssl"))
         ^InetSocketAddress laddr (.getLocalAddress ch)
         impl (make-mmap)
         eeid (next-long) ]
    (.mm-s impl :ios (make-session co ssl))
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        HTTPEvent
        (bindSession [_ s] (.mm-s impl :ios s))
        (getSession [_] (.mm-g impl :ios))
        (getId [_] eeid)
        (emitter [_] co)
        (getCookies [_]
          (let [ v (nsb (HttpHeaders/getHeader req "Cookie"))
                 rc (ArrayList.)
                 cks (if (hgl? v) (-> (CookieDecoder.) (.decode v)) []) ]
            (doseq [ ^Cookie c (seq cks) ]
              (.add rc (cookieToJava c)))
            rc))
        (getCookie [_ nm]
          (let [ v (nsb (HttpHeaders/getHeader req "Cookie"))
                 lnm (.toLowerCase nm)
                 cks (if (hgl? v) (-> (CookieDecoder.)(.decode v)) []) ]
            (some (fn [^Cookie c]
                    (if (= (.toLowerCase (.getName c)) lnm)
                      (cookieToJava c)
                      nil))
                    (seq cks))))

        (isKeepAlive [_] (HttpHeaders/isKeepAlive req))

        (hasData [_] (notnil? rdata))
        (data [_] rdata)

        (contentType [_] (HttpHeaders/getHeader req "content-type"))
        (contentLength [_] (HttpHeaders/getContentLength req 0))

        (encoding [this]  (get-charset (.contentType this)))
        (contextPath [_] "")

        (getHeaderValues [_ nm] (.getHeaders req nm))
        (getHeaders [_] (.getHeaderNames req))
        (getHeaderValue [_ nm] (HttpHeaders/getHeader req nm))
        (hasHeader [_ nm] (.containsHeader req nm))

        (getParameterValues [_ nm]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 rc (.get (.getParameters dc) nm) ]
            (if (nil? rc) (ArrayList.) rc)))
        (getParameters [_]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 m (.getParameters dc) ]
            (.keySet m)))
        (hasParameter [this nm]
          (.contains (.getParameters this) nm))

        (getParameterValue [_ nm]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 ^List rc (.get (.getParameters dc) nm) ]
            (if (and (notnil? rc) (> (.size rc) 0))
              (.get rc 0)
              nil)))

        (localAddr [_] (.getHostAddress (.getAddress laddr)))
        (localHost [_] (.getHostName laddr))
        (localPort [_] (.getPort laddr))

        (protocol [_] (.toString (.getProtocolVersion req)))
        (method [_] (.toString (.getMethod req)))

        (host [_] (HttpHeaders/getHost req))

        (queryString [_]
          (let [ s (nsb (.getUri req))
                 pos (.indexOf s "?") ]
            (if (>= pos 0)
              (.substring s pos)
              "")))

        (remotePort [_] (conv-long (HttpHeaders/getHeader req "REMOTE_PORT") 0))
        (remoteAddr [_] (nsb (HttpHeaders/getHeader req "REMOTE_ADDR")))
        (remoteHost [_] (nsb (HttpHeaders/getHeader req "")))

        (scheme [_] (if ssl "https" "http"))

        (serverPort [_] (conv-long (HttpHeaders/getHeader req "SERVER_PORT") 0))
        (serverName [_] (nsb (HttpHeaders/getHeader req "SERVER_NAME")))

        (isSSL [_] ssl)

        (getUri [_]
          (let [ dc (QueryStringDecoder. (.getUri req)) ]
            (.getPath dc)))

        (getRequestURL [_] (throw (IOException. "not implemented")))

        (getResultObj [_] res)
        (replyResult [this]
          (let [ ^IOSession mvs (.getSession this)
                 code (.getStatus res)
                 ^comzotohcljc.tardis.io.core.WaitEventHolder
                 wevt (.release co this) ]
            (cond
              (and (>= code 200) (< code 400)) (.handleResult mvs this res)
              :else nil)
            (when-not (nil? wevt)
              (.resumeOnResult wevt res))))

      )

      { :typeid :czc.tardis.io/HTTPEvent } )) )

(defmethod comp-configure :czc.tardis.io/NettyIO
  [^comzotohcljc.tardis.core.sys.Element co cfg]
  (let [ c (nsb (:context cfg)) ]
    (.setAttr! co :contextPath (strim c))
    (http-basic-config co cfg) ))

(defn- make-service-io [^comzotohcljc.tardis.io.core.EmitterAPI co]
  (reify comzotohcljc.netty.comms.NettyServiceIO
    (onReply [_ ch rsp msginfo rdata] nil)
    (onError [_ ch msginfo exp]  nil)
    (preSend [_ ch msg] nil)
    (onRequest [_ ch req msginfo rdata]
      (let [ mtd (:method msginfo)
             evt (cond
                   (= "WS" mtd)
                   (if (instance? XData rdata)
                     (make-wsock-event co ch ^XData rdata)
                     (throw (IOException. "Unexpected websocket data.")))
                   :else
                   (ioes-reify-event co ch req rdata))
             ^comzotohcljc.tardis.io.core.WaitEventHolder
             w (make-async-wait-holder (make-netty-trigger ch evt co) evt) ]
        (.timeoutMillis w
                        (.getAttr
                          ^comzotohcljc.tardis.core.sys.Element co
                          :waitMillis))
        (.hold co w)
        (.dispatch co evt {}))) ))

(defn- init-netty
  [^comzotohcljc.tardis.core.sys.Element co reqcb]
  (let [ ^comzotohcljc.tardis.core.sys.Element
         ctr (.parent ^Hierarchial co)
         options { :serverkey (.getAttr co :serverKey)
                   :passwd (.getAttr co :pwd)
                   :usercb reqcb }
         nes (makeServerNetty options) ]
    (debug "server-netty - made - success.")
    (.setAttr! co :netty nes)
    co))

(defmethod ioes-start :czc.tardis.io/NettyIO
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ host (nsb (.getAttr co :host))
         ^long port (.getAttr co :port)
         nes (.getAttr co :netty)
         ^ServerBootstrap bs (:bootstrap nes)
         ^InetAddress ip (if (nichts? host)
              (InetAddress/getLocalHost)
              (InetAddress/getByName host))
         cf (.bindAsync bs (InetSocketAddress. ip port))
         ^ChannelGroup cg (:cgroup nes) ]
;;    c.getConfig().setConnectTimeoutMillis(millis)
    (addListener cf { :ok (fn [^Channel c] (.add cg c)) })
    (debug "netty-io running on port: " port ", host: " host ", ip: " ip)
    (ioes-started co)))

(defmethod ioes-stop :czc.tardis.io/NettyIO
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ nes (.getAttr co :netty) ]
    (finzNetty nes)
    (ioes-stopped co)))

(defmethod comp-initialize :czc.tardis.io/NettyIO
  [^comzotohcljc.tardis.core.sys.Element co]
  (init-netty co (make-service-io co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private netty-eof nil)

