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

  comzotohcljc.crypto.ssl )

(import '(javax.net.ssl X509TrustManager TrustManager))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(java.net URL))
(import '(javax.net.ssl KeyManagerFactory TrustManagerFactory))

(use '[comzotohcljc.crypto.stores :only [make-crypto-store] ])
(use '[comzotohcljc.crypto.core :only [pkcs-file? get-jksStore get-pkcsStore get-srand make-simpleTrustMgr] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(defn make-sslContext "Make a server-side SSLContext."

  (^SSLContext [^URL keyUrl ^comzotohcljc.crypto.codec.Password pwdObj]
   (make-sslContext keyUrl pwdObj "TLS"))

  (^SSLContext [^URL keyUrl ^comzotohcljc.crypto.codec.Password pwdObj ^String flavor]
    (let [ ctx (SSLContext/getInstance flavor)
           ks (with-open [ inp (.openStream keyUrl) ]
                (if (pkcs-file? keyUrl)
                    (get-pkcsStore inp pwdObj)
                    (get-jksStore inp pwdObj)))
           cs (make-crypto-store ks pwdObj)
           ^TrustManagerFactory tmf   (.trustManagerFactory cs)
           ^KeyManagerFactory kmf   (.keyManagerFactory cs) ]

      (.init ctx (.getKeyManagers kmf) (.getTrustManagers tmf) (get-srand))
      ctx)) )


(defn make-sslClientCtx "Make a client-side SSLContext."

  ^SSLContext
  [ssl]

  (if (not ssl)
    nil
    (let [ ctx (SSLContext/getInstance "TLS") ]
      (.init ctx nil (into-array TrustManager [(make-simpleTrustMgr)]) nil)
      ctx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ssl-eof nil)

