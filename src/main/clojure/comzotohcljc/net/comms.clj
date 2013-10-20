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

  comzotohcljc.net.comms )

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(java.security.cert
  X509Certificate
  CertificateException))
(import '(java.security
  KeyStoreException
  KeyStore InvalidAlgorithmParameterException))
(import '(javax.net.ssl
  SSLContext SSLEngine X509TrustManager
  TrustManagerFactorySpi TrustManager
  ManagerFactoryParameters))
(import '(com.zotoh.frwk.net NetUtils SSLTrustMgrFactory))
(import '(com.zotoh.frwk.io XData))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.http.client HttpClient))
(import '(org.apache.http.client.methods HttpGet HttpPost))
(import '(org.apache.http.impl.client DefaultHttpClient))
(import '(org.apache.http
  Header
  StatusLine HttpEntity HttpResponse))
(import '(java.io File IOException))
(import '(org.apache.http.util EntityUtils))
(import '(java.net URL URI))
(import '(org.apache.http.params HttpConnectionParams))
(import '(org.apache.http.entity InputStreamEntity))
(import '(com.zotoh.frwk.io XData))


(use '[comzotohcljc.util.core :only [MutableMapAPI Try!] ])
(use '[comzotohcljc.util.mime :only [get-charset] ])
(use '[comzotohcljc.util.str :only [strim embeds? has-nocase?] ])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(def ^:dynamic *socket-timeout* 5000)
(def LOOPBACK "127.0.0.1")
(def LHOST "localhost")


(defrecord HTTPMsgInfo
  [^String protocol
   ^String method
   ^String uri
   is-chunked
   keep-alive
   clen
   headers
   params] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions to support apache http client.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkApacheClientHandle ""
  ^HttpClient
  []
  (let [ cli (DefaultHttpClient.)
         pms (.getParams cli) ]
    (HttpConnectionParams/setConnectionTimeout pms *socket-timeout*)
    (HttpConnectionParams/setSoTimeout pms *socket-timeout*)
    (NetUtils/cfgForRedirect cli)
    cli))

(defn- get-bits ""
  ^bytes
  [^HttpEntity ent]
  (if (nil? ent)
    nil
    (EntityUtils/toByteArray ent)) )

(defn- get-str ""
  ^String
  [^HttpEntity ent]
  (EntityUtils/toString ent "utf-8"))

(defn- processOK "" [^HttpResponse rsp]
  (let [ ent (.getEntity rsp)
         ct (if (nil? ent) nil (.getContentType ent))
         cv (if (nil? ct) "" (strim (.getValue ct)))
         cl (.toLowerCase ^String cv) ]
    (Try!
      (debug "http-response: " "content-encoding: "(.getContentEncoding ent) "\n"
             "content-type: " cv))
    (let [ bits (get-bits ent)
           clen (if (nil? bits) 0 (alength bits)) ]
      { :encoding (get-charset cv)
        :content-type cv
        :data (if (== clen 0) nil (XData. bits)) } )))
    ;;(cond
      ;;(or (.startsWith cl "text/")
          ;;(.startsWith cl "application/xml")
          ;;(.startsWith cl "application/json")) (get-bits ent) ;;(get-str ent)
      ;;:else (get-bits ent))) )

(defn- processError "" [^HttpResponse rsp ^Throwable exp]
  (do
    (Try! (EntityUtils/consumeQuietly (.getEntity rsp)))
    (throw exp)) )

(defn- processRedirect "" [^HttpResponse rsp]
  ;;TODO - handle redirect
  (processError rsp (IOException. "Redirect not supported.")) )

(defn- processReply "" [^HttpResponse rsp]
  (let [ st (.getStatusLine rsp)
         msg (if (nil? st) "" (.getReasonPhrase st))
         rc (if (nil? st) 0 (.getStatusCode st)) ]
    (cond
      (and (>= rc 200) (< rc 300))
      (processOK rsp)

      (and (>= rc 300) (< rc 400))
      (processRedirect rsp)

      :else
      (processError rsp (IOException. (str "Service Error: code = " rc ": " msg))))) )


(defn- doPOST "" [^URL targetUrl
                  ^String contentType
                  ^XData rdata
                  beforeSendFunc]
  (let [ ^HttpClient cli (mkApacheClientHandle) ]
    (try
      (let [ ent (InputStreamEntity. (.stream rdata) (.size rdata))
             p (HttpPost. (.toURI targetUrl)) ]
        (.setEntity p (doto ent
                            (.setContentType contentType)
                            (.setChunked true)))
        (when (fn? beforeSendFunc) (beforeSendFunc p))
        (processReply (.execute cli p)))
      (finally
          (.. cli getConnectionManager shutdown))) ))

(defn- doGET "" [^URL targetUrl beforeSendFunc]
  (let [ ^HttpClient cli (mkApacheClientHandle) ]
    (try
      (let [ g (HttpGet. (.toURI targetUrl)) ]
        (when (fn? beforeSendFunc) (beforeSendFunc g))
        (processReply (.execute cli g)))
      (finally
        (.. cli getConnectionManager shutdown))) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn syncPost "Perform a http-post on the target url."
  ([^URL targetUrl contentType ^XData rdata] (syncPost targetUrl contentType rdata nil))
  ([^URL targetUrl contentType ^XData rdata beforeSendFunc]
    (doPOST targetUrl contentType rdata beforeSendFunc)))

(defn syncGet "Perform a http-get on the target url."
  ([^URL targetUrl] (syncGet targetUrl nil))
  ([^URL targetUrl beforeSendFunc]
    (doGET targetUrl beforeSendFunc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-simpleClientSSLEngine "Simple minded, trusts everyone."
  []
  (let [ c (SSLContext/getInstance "TLS") ]
    (.init c nil (SSLTrustMgrFactory/getTrustManagers) nil)) )

(defn- clean-str "" [^String s]
  (StringUtils/stripStart (StringUtils/stripEnd s ";,") ";,"))

(defn parse-ie "" [^String line]
  (let [ p1 #".*(MSIE\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         p2 #".*(Windows\s*Phone\s*(\S+)\s*).*"
         m2 (re-matches p2 line)
         bw "IE"
         dt (if (has-nocase? "iemobile") :mobile :pc) ]
    (let [ bv (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "")
           dev (if (and (not (empty? m2)) (> (count m2) 2))
                 { :device-version (clean-str (nth m1 2))
                   :device-moniker "windows phone"
                   :device-type :phone }
                 {} ) ]
      (merge {:browser :ie :browser-version bv :device-type dt}
             dev))))

(defn parse-chrome "" [^String line]
  (let [ p1 #".*(Chrome/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    {:browser :chrome :browser-version bv :device-type :pc }))

(defn parse-kindle "" [^String line]
  (let [ p1 #".*(Silk/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :silk :browser-version bv :device-type :mobile :device-moniker "kindle" } ))

(defn parse-android "" [^String line]
  (let [ p1 #".*(Android\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :chrome :browser-version bv :device-type :mobile :device-moniker "android" } ))

(defn parse-ffox "" [^String line]
  (let [ p1 #".*(Firefox/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :firefox :browser-version bv :device-type :pc } ))

(defn parse-safari "" [^String line]
  (let [ p1 #".*(Version/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "")
         rc { :browser :safari :browser-version bv :device-type :pc } ]
    (cond
      (has-nocase? line "mobile/") (merge rc { :device-type :mobile })
      (has-nocase? line "iphone") (merge rc { :device-type :phone :device-moniker "iphone" } )
      (has-nocase? line "ipad") (merge rc { :device-type :mobile :device-moniker "ipad" } )
      (has-nocase? line "ipod") (merge rc { :device-type :mobile :device-moniker "ipod" } )
      :else rc )))


(defn parse-userAgentLine "Retuns a map of browser/device attributes."
  [^String agentLine]
  (let [ line (strim agentLine) ]
    (cond
      (and (embeds? line "Windows") (embeds? line "Trident/"))
      (parse-ie line)

      (and (embeds? line "AppleWebKit/")(embeds? line "Safari/")
           (embeds? line "Chrome/"))
      (parse-chrome line)

      (and (embeds? line "AppleWebKit/") (embeds? line "Safari/")
           (embeds? line "Android"))
      (parse-android line)

      (and (embeds? line "AppleWebKit/")(embeds? line "Safari/")
           (embeds? line "Silk/"))
      (parse-kindle line)

      (and (embeds? line "Safari/")(embeds? line "Mac OS X"))
      (parse-safari)

      (and (embeds? line "Gecko/")(embeds? line "Firefox/"))
      (parse-ffox)

      :else
      {} )))



(def ^:private comms-eof nil)

