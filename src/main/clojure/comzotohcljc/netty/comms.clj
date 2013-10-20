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

  comzotohcljc.netty.comms )

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(java.nio.charset Charset))
(import '(java.lang.reflect Field))
(import '(org.apache.commons.io IOUtils FileUtils))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io IOException ByteArrayOutputStream
  File OutputStream InputStream))
(import '(java.util Map$Entry HashMap Properties ArrayList))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(javax.net.ssl X509TrustManager TrustManager))
(import '( com.zotoh.frwk.net ULFormItems ULFileItem))

(import '(org.jboss.netty.channel.socket.nio
  NioClientSocketChannelFactory NioServerSocketChannelFactory))
(import '(org.jboss.netty.bootstrap
  Bootstrap ClientBootstrap ServerBootstrap))
(import '(org.jboss.netty.buffer ChannelBuffers ChannelBuffer))
(import '(org.jboss.netty.handler.codec.http.multipart
  DefaultHttpDataFactory FileUpload HttpPostRequestDecoder
  HttpPostRequestDecoder$EndOfDataDecoderException HttpData
  InterfaceHttpData InterfaceHttpData$HttpDataType))
(import '(org.jboss.netty.handler.stream
  ChunkedWriteHandler ChunkedStream))
(import '(org.jboss.netty.channel.socket.nio
  NioServerSocketChannel NioSocketChannel))
(import '(org.jboss.netty.channel
  ChannelHandlerContext Channel SimpleChannelUpstreamHandler
  ChannelFutureListener ChannelFuture Channels
  MessageEvent ExceptionEvent ChannelPipelineFactory
  ChannelPipeline ChannelHandler))
(import '(org.jboss.netty.channel.socket SocketChannel))
(import '(org.jboss.netty.channel.group
  ChannelGroupFuture ChannelGroup ChannelGroupFutureListener
  DefaultChannelGroup))
(import '(org.jboss.netty.handler.codec.http
  HttpHeaders HttpVersion HttpChunk HttpMethod
  HttpHeaders$Values HttpHeaders$Names
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder
  DefaultHttpRequest HttpServerCodec HttpClientCodec
  HttpResponseEncoder))
(import '(org.jboss.netty.handler.ssl SslHandler))
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
(import '(com.zotoh.frwk.net NetUtils))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.frwk.util TFac))

(use '[comzotohcljc.crypto.ssl :only [make-sslContext make-sslClientCtx] ])
(use '[comzotohcljc.net.rts])
(use '[comzotohcljc.util.files :only [save-file get-file] ])
(use '[comzotohcljc.util.core :only [_MB uid notnil? Try! TryC] ])
(use '[comzotohcljc.util.str :only [strim nsb hgl?] ])
(use '[comzotohcljc.util.io :only [make-baos] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(comment
(def HTTP-CODES
  (let [ to-key (comp int (fn [^Field f] (.code ^HttpResponseStatus (.get f nil))))
         fields (:fields (bean HttpResponseStatus))
         kkeys (map to-key fields)
         vvals (map (comp str (fn [^Field f] (.get f nil))) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))
)

;; map of { int (code) -> HttpResponseStatus }
(def HTTP-CODES
  (let [ to-key (fn [^Field f] (.getCode ^HttpResponseStatus (.get f nil)))
         fields (:fields (bean HttpResponseStatus))
         kkeys (map to-key fields)
         vvals (map (fn [^Field f] (.get f nil)) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord NettyIO  [^Bootstrap bootstrap ^ChannelGroup cgroup ] )

(defmulti addListener (fn [a & more ] (class a)))

(defprotocol NettyServiceIO
  ""
  (onRequest [_ ch req msginfo rdata] )
  (preSend [_ ch req] )
  (onError [_ ch msginfo err] )
  (onReply [_ ch rsp msginfo rdata] ))

(defprotocol RouteCracker
  ""
  (routable? [_ msgInfo] )
  (hasRoutes? [_])
  (crack [_ msgInfo] ))

(defn sa-map! "Set attachment object from the channel."
  [^Channel ch obj]
  (.setAttachment ch obj))

(defn ga-map "Get attachment object from the channel."
  [^Channel ch]
  (let [ rc (.getAttachment ch) ]
    (if (nil? rc) {} rc)))

(defn kill9 "Clean up resources used by netty."
  [^Bootstrap bs]
  (.releaseExternalResources bs))

(defn finzNetty "Bring down netty."
  [ { bs :bootstrap cg :cgroup } ]
  (if (nil? cg)
    (kill9 bs)
    (-> (.close ^ChannelGroup cg)
        (addListener { :done (fn [_] (kill9 bs)) }))))

(defn makeHttpReply "Make a netty http-response object."
  (^HttpResponse [] (makeHttpReply 200))
  (^HttpResponse [status]
    (DefaultHttpResponse. HttpVersion/HTTP_1_1
                          (get HTTP-CODES status))))

(defn closeCF "Maybe close the channel."
  [keepAlive? ^ChannelFuture cf]
  (if (and (not keepAlive?) (notnil? cf))
    (.addListener cf ChannelFutureListener/CLOSE)))

(defn sendRedirect "Redirect a request."
  [^Channel ch perm ^String targetUrl]
  (let [ rsp (makeHttpReply (if perm 301 307)) ]
    (debug "redirecting to -> " targetUrl)
    (HttpHeaders/setHeader rsp  "location" targetUrl)
    (closeCF false (.write ch rsp))))

(defn makeNilServiceIO "Reify a no-op service-io object."
  ^comzotohcljc.netty.comms.NettyServiceIO
  []
  (reify NettyServiceIO
    (preSend [_ ch req] (debug "empty pre-send." ))
    (onError [_ ch msginfo err] (debug "empty onerror." ))
    (onRequest [_ ch req msginfo rdata] (debug "empty onreq." ))
    (onReply [_ ch rsp msginfo rdata] (debug "empty onres." ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server side netty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare nioPRequest)
(declare nioPResult)
(declare nioWSFrame)
(declare nioChunk)

(defn- genericNettyHandler "Make a generic Netty 4.x Pipeline Handler."
  ^ChannelHandler
  [options]
  (proxy [SimpleChannelUpstreamHandler] []

    (exceptionCaught [ctx ev]
      (let [ ch (.getChannel ^ChannelHandlerContext ctx)
             err (.getCause ^ExceptionEvent ev)
             attObj (ga-map ch)
             msginfo (:info attObj)
             ucb (:usercb options)
             keepAlive (if (nil? msginfo)
                         false
                         (:keep-alive msginfo)) ]
        (when-not (nil? err) (error err ""))
        (when-not (nil? ucb)
          (.onError ^comzotohcljc.netty.comms.NettyServiceIO
                    ucb ch msginfo err))
        (when-not keepAlive (Try! (NetUtils/closeChannel ch)))))

    (messageReceived [ctx ev]
      (let [ ch (.getChannel ^ChannelHandlerContext ctx)
             msg (.getMessage ^MessageEvent ev) ]
      (debug "pipeline-entering with msg: " (type msg))
      (cond
        (instance? HttpRequest msg)
        (nioPRequest ctx msg options)

        (instance? HttpResponse msg)
        (nioPResult ctx msg options)

        (instance? HttpChunk msg)
        (nioChunk ctx msg)

        (instance? WebSocketFrame msg)
        (nioWSFrame ctx msg)

        (nil? msg)
        (throw (IOException. "Got null object."))

        :else
        (throw (IOException. (str "Got object: " (type msg))) ))))

    ))

(defn- inizServer "" ^ChannelPipelineFactory [options]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline)
             kf (:serverkey options)
             pw (:passwd options)
             ssl (if (nil? kf)
                     nil
                     (make-sslContext kf pw))
             eg (if (nil? ssl)
                    nil
                    (doto (.createSSLEngine ssl)
                          (.setUseClientMode false))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpServerCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (genericNettyHandler options))
        pl))))

(defn makeServerNetty ""
  ([] (makeServerNetty {} ))
  ([options]
    (let [ boss (Executors/newCachedThreadPool (TFac. "boss"))
           bees (Executors/newCachedThreadPool (TFac. "bees"))
           bs (doto (ServerBootstrap. (NioServerSocketChannelFactory. boss bees))
                    (.setOption "reuseAddress" true)
                    (.setOption "child.receiveBufferSize" (int (* 2 _MB)))
                    (.setOption "child.tcpNoDelay" true))
           opts (:netty options)
           cg (DefaultChannelGroup. (uid)) ]
      (doseq [ [k v] (seq opts) ]
        (.setOption bs (name k) v))
      (.setPipelineFactory bs (inizServer options))
      (comzotohcljc.netty.comms.NettyIO. bs cg)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; client side netty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- inizClient ^ChannelPipelineFactory [options]
  (reify ChannelPipelineFactory 
    (getPipeline [_]
      (let [ ssl (= (.getProtocol ^URL (:targetUrl options)) "https")
             pl (org.jboss.netty.channel.Channels/pipeline)
             ^SSLContext ctx (make-sslClientCtx ssl)
             eg (if (notnil? ctx)
                    (doto (.createSSLEngine ctx)
                          (.setUseClientMode true))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpClientCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (genericNettyHandler options))
        pl))))

(defn makeClientNetty ""
  ([] (makeClientNetty {}))
  ([options]
   (let [ boss (Executors/newCachedThreadPool (TFac. "boss"))
          bees (Executors/newCachedThreadPool (TFac. "bees"))
          opts (:netty options)
          bs (doto (ClientBootstrap. (NioClientSocketChannelFactory. boss bees))
                   (.setOption "tcpNoDelay" true)
                   (.setOption "receiveBufferSize" (int (* 2 1024 1024)))
                   (.setOption "keepAlive" true))
          cg (DefaultChannelGroup. (uid)) ]
     (doseq [ [k v] (seq opts) ]
       (.setOption bs (name k) v))
     (.setPipeline bs (inizClient options))
     (comzotohcljc.netty.comms.NettyIO. bs cg)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contWith100 "Send back 100-continue."
  [^ChannelHandlerContext ctx]
  (.write (.getChannel ctx) (makeHttpReply 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- opDone "" [chOrGroup success error options]
  (cond
    (and (fn? (:nok options)) (not success))
    ((:nok options) chOrGroup error)

    (and (fn? (:ok options)) success)
    ((:ok options) chOrGroup)

    (fn? (:done options))
    ((:done options) chOrGroup)

    :else nil)
  nil)

(defmethod addListener ChannelGroupFuture
  [^ChannelGroupFuture cf options]
  (-> cf (.addListener
           (reify ChannelGroupFutureListener
             (operationComplete [_ cff]
               (opDone (.getGroup cf)
                       (.isCompleteSuccess ^ChannelGroupFuture cff)
                       nil
                       options ))))))

(defmethod addListener ChannelFuture
  [^ChannelFuture cf options]
  (-> cf (.addListener
           (reify ChannelFutureListener
             (operationComplete [_ cff]
               (opDone (.getChannel cf)
                       (.isSuccess ^ChannelFuture cff)
                       (.getCause ^ChannelFuture cff)
                       options ))))))

(defn- reply-xxx "" [^Channel ch status]
  (let [ rsp (makeHttpReply status)
         attObj (ga-map ch)
         ^HttpPostRequestDecoder dc (:decoder attObj)
         info (:info attObj)
         kalive (if (nil? info) false (:keep-alive info)) ]
    (when-not (nil? dc)
      (Try! (.cleanFiles dc)))
    ;;(HttpHeaders/setTransferEncodingChunked res)
    (HttpHeaders/setContentLength rsp 0)
    (->> (.write ch rsp) (closeCF kalive))))

(defn- nioMapHeaders "Turn headers into a clj-map."
  [^HttpMessage msg]
  (persistent! (reduce (fn [sum ^String n]
                           (assoc! sum (.toLowerCase n) (vec (.getHeaders msg n))))
                       (transient {}) (.getHeaderNames msg))) )

(defn- nioExtractMsg "Get info from http message."
  [^HttpMessage msg]
    { :is-chunked (.isChunked msg)
      :protocol (nsb (.getProtocolVersion msg))
      :keep-alive (HttpHeaders/isKeepAlive msg)
      :clen (HttpHeaders/getContentLength msg 0)
      :headers (nioMapHeaders msg) } )

(defn- nioExtractReq "Extract info from request."
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpRequest req]
  (let [ ws (.toLowerCase (strim (HttpHeaders/getHeader req "upgrade")))
         decr (QueryStringDecoder. (.getUri req))
         ^String md (-> req (.getMethod) (.getName))
         uri (.getPath decr)
         m1 (nioExtractMsg req)
         ^String mo (strim (HttpHeaders/getHeader req "X-HTTP-Method-Override"))
         params (persistent! (reduce (fn [sum ^Map$Entry en]
                                       (assoc! sum (nsb (.getKey en)) (vec (.getValue en))))
                                     (transient {})
                                     (.getParameters decr)))
         mi (comzotohcljc.net.comms.HTTPMsgInfo.
              (:protocol m1)
              (-> (if (= "websocket" ws) "WS" (if (hgl? mo) mo md))
                (.toUpperCase))
              uri
              (:is-chunked m1)
              (:keep-alive m1)
              (:clen m1) (:headers m1) params) ]
    (debug "request message info: " mi)
    mi))

(defn- nioExtractRes "Extract info from response."
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpResponse res]
  (let [ m1 (nioExtractMsg res) ]
    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- nioFinz "Close any output stream created during the message handling."
  [^ChannelHandlerContext ctx]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         os (:os attObj) ]
    (IOUtils/closeQuietly ^OutputStream os) ))

(defn- process-multi-part "" [^Channel ch
                              ^InterfaceHttpData data
                              ^ULFormItems items
                              ^ArrayList out]
  (debug "multi-part data = " (type data))
  (cond
    (= (.getHttpDataType data) InterfaceHttpData$HttpDataType/Attribute)
    (let [^org.jboss.netty.handler.codec.http.multipart.Attribute attr data
           nm (nsb (.getName attr))
           nv (.get attr) ]
      (debug "multi-part attribute value-string = " (String. nv "utf-8"))
      (.add items (ULFileItem. nm nv)))
    (= (.getHttpDataType data) InterfaceHttpData$HttpDataType/FileUpload)
    (let [ ^FileUpload fu data
           nm (nsb (.getName fu)) ]
      (when-not (.isCompleted fu)
        (throw (IOException. "checking uploaded file - incomplete.")))
      (when-not (.isInMemory fu)(.add out fu))
      (.add items (ULFileItem. nm (.getContentType fu)
                                     (.getFilename fu)
                                     (if (.isInMemory fu)
                                       (XData. (.get fu))
                                       (XData. (.getFile fu))))))
    :else nil))

(defn- finz-decoder [^ChannelHandlerContext ctx]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         ^XData rdata (:xs attObj)
         info (:info attObj)
         dir (:dir attObj)

         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj)

         ^HttpPostRequestDecoder
         dc (:decoder attObj)

         out (:formtrash attObj)
         items (:formitems attObj) ]
    (doseq [ f (seq out) ]
      (.removeHttpDataFromClean dc f))
    (Try! (.cleanFiles dc))
    ;;(.resetContent rdata items)
    (.onRequest usercb ch dir info items) ))


(defn- read-formdata [^ChannelHandlerContext ctx ^HttpPostRequestDecoder dc]
  (let [ ch (.getChannel ctx)
         attObj (ga-map ch)
         items (:formitems attObj)
         out (:formtrash attObj) ]
    (try
      (while (.hasNext dc)
        (let [ ^HttpData data (.next dc) ]
          (try
            (process-multi-part ch data items out)
            (finally
              (when-not (instance? FileUpload data)(.delete data))))))
      (catch HttpPostRequestDecoder$EndOfDataDecoderException _
        (warn "read-formdata: EndOfDataDecoderException thrown.")))
    ))

(defn- nioComplete "" [^ChannelHandlerContext ctx msg]
  (nioFinz ctx)
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         ^XData rdata (:xs attObj)
         info (:info attObj)
         dir (:dir attObj)

         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj)

         ^HttpPostRequestDecoder
         dc (:decoder attObj)

         os (:os attObj)
         clen (:clen attObj) ]

    (when (and (instance? ByteArrayOutputStream os)
               (> clen 0))
      (.resetContent rdata os))
    (cond
      (instance? HttpResponse dir)
      (do
        (TryC (NetUtils/closeChannel ch))
        (.onReply usercb ch dir info rdata))

      (instance? HttpRequest dir)
      (if (nil? dc)
        (.onRequest usercb ch dir info rdata)
        (finz-decoder ctx))

      (instance? WebSocketFrame msg)
      (do (.onRequest usercb ch msg info rdata))

      :else nil)
    ))

(defn- nioSockitDown [^ChannelHandlerContext ctx ^ChannelBuffer cbuf]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         ^OutputStream cout (:os attObj)
         ^XData rdata (:xs attObj)
         csum (:clen attObj)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile rdata)
                  cout
                  (NetUtils/swapFileBacked rdata cout nsum)) ]
    (sa-map! ch (-> attObj
                    (assoc :clen nsum)
                    (assoc :os nout))) ))

(defn- nioCfgCtx [^ChannelHandlerContext ctx usercb]
  (let [ att { :os (make-baos)
               :xs (XData.)
               :formitems (ULFormItems.)
               :formtrash (ArrayList.)
               :clen 0
               :cb usercb
               :dir nil
               :info nil } ]
    (sa-map! (.getChannel ctx) att)
    att))

(defn- nioPError [^ChannelHandlerContext ctx err]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         info (:info attObj)
         usercb (:cb attObj) ]
    (.onError ^comzotohcljc.netty.comms.NettyServiceIO usercb ch info err)))

(defn- nioFrameChunk [^ChannelHandlerContext ctx
                      ^ContinuationWebSocketFrame frame]
  (let [ cbuf (.getBinaryData frame)
         ch (.getChannel ctx) ]
    (when-not (nil? cbuf) (nioSockitDown ctx cbuf))
    (when (.isFinalFragment frame)
      (do
        (nioFinz ctx)
        (when (nil? cbuf)
          (let [ s (nsb (.getAggregatedText frame))
                 attObj (ga-map ch)
                 xs (:xs attObj) ]
            (sa-map! ch (assoc attObj :clen (.length s)))
            (.resetContent ^XData xs s)))
        (nioComplete ctx frame) )) ))

(defn- getBits [^ChannelHandlerContext ctx ^WebSocketFrame frame]
  (when-let [ buf (.getBinaryData frame) ]
    (nioSockitDown ctx buf)))

(defn- nioWSFrame [^ChannelHandlerContext ctx ^WebSocketFrame frame ]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         ^XData xs (:xs attObj)
         ^WebSocketServerHandshaker hs (:hs attObj) ]
    (debug "nio-wsframe: received a " (type frame) )
    (cond
      (instance? CloseWebSocketFrame frame)
      (.close hs ch ^CloseWebSocketFrame frame)

      (instance? PingWebSocketFrame frame)
      (.write ch (PongWebSocketFrame. (.getBinaryData frame)))

      (instance? BinaryWebSocketFrame frame)
      (do
        (getBits ctx frame)
        (nioComplete ctx frame))

      (instance? TextWebSocketFrame frame)
      (let [ s (nsb (.getText ^TextWebSocketFrame frame)) ]
        (.resetContent xs s)
        (sa-map! ch (assoc attObj :clen (.length s)))
        (nioComplete ctx frame))

      (instance? ContinuationWebSocketFrame frame)
      (nioFrameChunk ctx frame)

      :else ;; what else can this be ????
      nil) ))

(defn- maybeSSL "" [^ChannelHandlerContext ctx]
  (notnil? (-> (NetUtils/getPipeline ctx)
               (.get (class SslHandler)))))

(defn- wsSSL "" [^ChannelHandlerContext ctx]
  (let [ ssl (-> (NetUtils/getPipeline ctx)
                 (.get (class SslHandler))) ]
    (when-not (nil? ssl)
      (addListener (.handshake ^SslHandler ssl)
                   { :nok (fn [c] (NetUtils/closeChannel ^Channel c)) } ))))

(defn- nioWSock "" [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.getChannel ctx) attObj (ga-map ch)
         px (if (maybeSSL ctx) "wss://" "ws://")
         us (str px (HttpHeaders/getHeader req "host") (.getUri req))
         wf (WebSocketServerHandshakerFactory. us nil false)
         hs (.newHandshaker wf req) ]
    (if (nil? hs)
      (do
        (.sendUnsupportedWebSocketVersionResponse wf ch)
        (Try! (NetUtils/closeChannel ch)))
      (do
        (sa-map! ch (assoc attObj :hs hs))
        (addListener (.handshake hs ch req)
                     { :nok (fn [^Channel c ^Throwable e]
                              (-> (Channels/fireExceptionCaught ch e)))
                       :ok (fn [_] (wsSSL ctx)) })))
    ))

(defn- new-data-fac "" ^DefaultHttpDataFactory []
  (DefaultHttpDataFactory. (com.zotoh.frwk.io.IOUtils/streamLimit)))

(defn- maybe-formdata "" ^HttpPostRequestDecoder [^Channel ch ^HttpRequest req msginfo]
  (let [ ct (-> (nsb (HttpHeaders/getHeader req "content-type"))
                (.toLowerCase))
         attObj (ga-map ch)
         md (:method msginfo) ]
    ;; multipart form
    (if (and (or (= "POST" md) (= "PUT" md) (= "PATCH" md))
             (or (>= (.indexOf ct "multipart/form-data") 0)
                 (>= (.indexOf ct "application/x-www-form-urlencoded") 0)))
      (let [ rc (HttpPostRequestDecoder. (new-data-fac) req) ]
        (debug "form-data as input, required post-decoder")
        (sa-map! ch (assoc attObj :decoder rc))
        rc)
      nil)
    ))

(defn- maybe-last-req [ ^ChannelHandlerContext ctx msg]
  (cond
    (instance? HttpRequest msg)
    (nioComplete ctx msg)

    (and (instance? HttpChunk msg)
         (.isLast ^HttpChunk msg))
    (nioComplete ctx msg)

    :else
    nil))

(defn- nio-req-decode "" [^ChannelHandlerContext ctx ^HttpMessage req]
  (let [ ch (.getChannel ctx)
         attObj (ga-map ch)
         ^HttpPostRequestDecoder c (:decoder attObj) ]
    (when (notnil? c)
      (with-local-vars [err false]
        (try
          (when (instance? HttpChunk req)
            (.offer c ^HttpChunk req))
          (read-formdata ctx c)
          (catch Throwable e#
            (var-set err true)
            (error e# "")
            (reply-xxx 400)))
        (if-not @err (maybe-last-req ctx req))))))

(defn- nio-request "" [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.getChannel ctx)
         attObj (ga-map ch)
         msginfo (:info attObj) ]
    (try
      (let [ rc (maybe-formdata ch req msginfo) ]
        (when-not (:is-chunked msginfo)
          (if (nil? rc)
            (do
              (nioSockitDown ctx (.getContent req))
              (nioComplete ctx req))
            (nio-req-decode ctx req))))
      (catch Throwable e#
        (error e# "")
        (reply-xxx 400)))
    ))

(defn- nioPRequest "Handle a request."
  [^ChannelHandlerContext ctx ^HttpRequest req options]
  (nioCfgCtx ctx (:usercb options))
  (let [ msginfo (nioExtractReq req)
         ch (.getChannel ctx)
         attObj (ga-map ch)
         ^comzotohcljc.netty.comms.RouteCracker
         rtcObj (:rtcObj options)
         rts (if (and (notnil? rtcObj)
                      (.hasRoutes? rtcObj))
                 (.routable? rtcObj msginfo)
                 true) ]
    (debug "received a " (:method msginfo) " request from " (:uri msginfo))
    (sa-map! ch (-> attObj
                    (assoc :info msginfo)
                    (assoc :rts rts)
                    (assoc :dir req)))
    (if rts
      (if (= "WS" (:method msginfo))
        (nioWSock ctx req)
        (do
          (when (HttpHeaders/is100ContinueExpected req) (contWith100 ctx))
          (nio-request ctx req)))
      (do
        (warn "route not matched - ignoring request.")
        (if (true? (:forwardBadRoutes options))
          (nioComplete ctx req)
          (reply-xxx 403))))
    ))

(defn- nioRedirect "" [^ChannelHandlerContext ctx msg]
  ;; TODO: handle redirect properly, for now, same as error
  (nioPError ctx (IOException. "Unsupported redirect.")))

(defn- nioPResult "Handle a response."
  [^ChannelHandlerContext ctx ^HttpResponse res options]
  (nioCfgCtx ctx (:usercb options))
  (let [ msginfo (nioExtractRes res)
         ch (.getChannel ctx)
         attObj (ga-map ch)
         s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "got a http-response: code " c " reason: " r)
    (sa-map! ch (-> attObj
                    (assoc :info msginfo)
                    (assoc :dir res)
                    (assoc :rts false)))
    (cond
      (and (>= c 200) (< c 300))
      (when-not (.isChunked res)
        (nioSockitDown ctx (.getContent res)))

      (and (>= c 300) (< c 400))
      (nioRedirect ctx)

      :else
      (nioPError ctx (IOException. (str "error code: " c))))))

(defn- nio-basic-chunk "handle a chunk - part of a message"
  [^ChannelHandlerContext ctx ^HttpChunk msg]
  (let [ done (try
                  (nioSockitDown ctx (.getContent msg))
                  (.isLast msg)
                (catch Throwable e#
                    (do (nioFinz ctx) (throw e#)))) ]
    (if done (nioComplete ctx msg) nil)))

(defn- nioChunk "handle a chunk - part of a message"
  [^ChannelHandlerContext ctx ^HttpChunk msg]
  (let [ ch (.getChannel ctx)
         attObj (ga-map ch)
         dc (:decoder attObj) ]
    (if (nil? dc)
      (nio-basic-chunk ctx msg)
      (nio-req-decode ctx msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make some servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nettyXXXServer "Make a Netty server."

  ^NettyIO
  [^String host port options]

  (let [ rc (makeServerNetty options)
         ^ServerBootstrap bs (:bootstrap rc)
         ^ChannelGroup cg (:cgroup rc) ]
    (addListener (.bindAsync bs (InetSocketAddress. host (int port)))
                 { :ok (fn [^Channel c] (.add cg c)) })
    (debug "netty-xxx-server: running on host " host ", port " port)
    rc))

(defn makeMemHttpd "Make an in-memory http server."

  ^NettyIO
  [^String host port options]

  (nettyXXXServer host port options))

(defn- reply-get-vfile [^Channel ch ^XData rdata]
  (let [ res (makeHttpReply 200)
         clen (.size rdata)
         attObj (ga-map ch)
         info (:info attObj)
         kalive (if (nil? info) false (:keep-alive info)) ]
    (HttpHeaders/setHeader res "content-type" "application/octet-stream")
    (HttpHeaders/setContentLength res clen)
    (.setChunked res true)
    (.write ch res)
    (closeCF kalive (.write ch (ChunkedStream. (.stream rdata)))) ))

(defn- filer-handler [^File vdir]
  (let [ putter (fn [^Channel ch ^String fname rdata]
                  (try
                    (if (instance? XData rdata)
                      (do
                        (save-file vdir fname ^XData rdata)
                        (reply-xxx ch 200))
                      (reply-xxx ch 400))
                    (catch Throwable e#
                      (reply-xxx ch 500))))
         getter (fn [^Channel ch ^String fname]
                  (let [ rdata (get-file vdir fname) ]
                    (if (.hasContent rdata)
                      (reply-get-vfile ch rdata)
                      (reply-xxx ch 204)))) ]
    (reify NettyServiceIO
      (preSend [_ ch msg] nil)
      (onError [_ ch msginfo err]
        (do
          (when-not (nil? err) (error err ""))
          (reply-xxx ch 500)))
      (onReply [_ ch rsp msginfo rdata] nil)
      (onRequest [_ ch req msginfo rdata]
        (let [ mtd (nsb (:method msginfo))
               uri (nsb (:uri msginfo))
               pos (.lastIndexOf uri (int \/))
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter ^Channel ch p rdata)
            (or (= mtd "GET")(= mtd "HEAD")) (getter ^Channel ch p)
            :else (reply-xxx ch 405)))))
      ))

(defn makeMemFileSvr "A file server which can get/put files."

  ^NettyIO
  [^String host port options]

  (let [ fh (filer-handler (:vdir options)) ]
    (nettyXXXServer host port (merge options { :usercb fh } ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http client functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- connectClient "Make a Netty 4.x client."

  ([^URL targetUrl] (connectClient {}))
  ([^URL targetUrl options]
   (let [ opts (merge { :usercb (makeNilServiceIO)
                        :netty {}
                        :targetUrl targetUrl }
                      options)
          ssl (= "https" (.getProtocol targetUrl))
          pnum (.getPort targetUrl)
          port (if (< pnum 0) (if ssl 443 80) pnum)
          host (.getHost targetUrl)
          sock (InetSocketAddress. host (int port))
          ucb (:usercb opts)
          nc (makeClientNetty opts)
          ^ClientBootstrap bs (:bootstrap nc)
          ^ChannelFuture cf (-> bs
                                (.connect sock)
                                (.awaitUninterruptibly))
          ok (.isSuccess cf)
          ch (if ok (.getChannel cf))
          e (if (not ok) (.getCause cf)) ]
     (when-not ok
       (if (nil? e)
           (throw (IOException. "Failed to connect to URL: " targetUrl))
           (throw e)))
     (.add ^ChannelGroup (:cgroup nc) (.getChannel cf))
     (debug "Netty client connected to " host ":" port " - OK.")
     [nc ch opts] )))

(defn- send-httpClient "" [^URL targetUrl ^XData rdata options]
  (let [ clen (if (nil? rdata) 0 (.size rdata))
         mo (:override options)
         md (if (> clen 0)
              (if (hgl? mo) mo "POST")
              (if (hgl? mo) mo "GET"))
         req (DefaultHttpRequest. HttpVersion/HTTP_1_1
                                  (HttpMethod/valueOf md)
                                  (nsb targetUrl))
         [nc ^Channel ch opts] (connectClient targetUrl options)
         ka (:keepAlive opts)
         ^comzotohcljc.netty.comms.NettyServiceIO
         ucb (:usercb opts) ]
    (HttpHeaders/setHeader req "Connection" (if ka "keep-alive" "close"))
    (HttpHeaders/setHeader req "host" (.getHost targetUrl))
    (.preSend ucb ch req)
    (let [ ct (HttpHeaders/getHeader req "content-type") ]
      (when (and (StringUtils/isEmpty ct)
                 (> clen 0))
        (HttpHeaders/setHeader req "content-type" "application/octet-stream")) )
    (HttpHeaders/setContentLength req clen)
    (debug "Netty client: about to flush out request (headers)")
    (debug "Netty client: content has length " clen)
    (with-local-vars [wf nil]
      (var-set wf (.write ch req))
      (when (> clen 0)
        (var-set wf (if (> clen (com.zotoh.frwk.io.IOUtils/streamLimit))
                      (.write ch (ChunkedStream. (.stream rdata)))
                      (.write ch (ChannelBuffers/wrappedBuffer (.javaBytes rdata))))))
      (closeCF ka @wf))
    ))

(defn asyncPost "Async HTTP Post"
  ([^URL targetUrl ^XData rdata options]
   (send-httpClient targetUrl rdata options))
  ([^URL targetUrl ^XData rdata]
   (asyncPost targetUrl rdata {})) )

(defn asyncGet "Async HTTP GET"
  ([^URL targetUrl] (asyncGet targetUrl {}))
  ([^URL targetUrl options]
   (send-httpClient targetUrl nil options)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; router cracker
;;

;; [ri mc] routeinfo matcher
(defn- seek-route [mtd uri rts]
  (if-not (nil? rts)
    (some (fn [^comzotohcljc.net.rts.RouteInfo ri]
            (let [ m (.resemble? ri mtd uri) ]
              (if (nil? m) nil [ri m])))
          (seq rts)) ))

(defn makeRouteCracker "Create a url route cracker."
  ^comzotohcljc.netty.comms.RouteCracker
  [routes]
  (reify RouteCracker
    (hasRoutes? [_] (> (count routes) 0))
    (routable? [this msgInfo]
      (first (crack this msgInfo)))
    (crack [_ msgInfo]
      (let [ ^String mtd (:method msgInfo)
             ^String uri (:uri msgInfo)
             rc (seek-route mtd uri routes)
             rt (if (nil? rc)
                  [false nil nil ""]
                  [true (first rc)(last rc) ""] ) ]
        (if (and (false? (nth rt 0))
                 (not (.endsWith uri "/"))
                 (seek-route mtd (str uri "/") routes))
          [true nil nil (str uri "/")]
          rt)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private comms-eof nil)


