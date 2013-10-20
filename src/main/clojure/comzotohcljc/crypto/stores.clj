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

  comzotohcljc.crypto.stores)

(use '[clojure.tools.logging :only [info warn error debug] ])
(require '[clojure.math.numeric-tower :as math])

(import '(java.io File FileInputStream IOException InputStream))
(import '(java.security.cert CertificateFactory X509Certificate Certificate))
(import '(java.security KeyStore PrivateKey
  KeyStore$TrustedCertificateEntry
  KeyStore$ProtectionParameter
  KeyStore$PasswordProtection
  KeyStore$PrivateKeyEntry))
(import '(javax.net.ssl KeyManagerFactory TrustManagerFactory))
(import '(javax.security.auth.x500 X500Principal))


(use '[comzotohcljc.crypto.core 
       :only [new-alias cert-aliases pkey-aliases get-pkcsStore get-jksStore] ])
(use '[comzotohcljc.util.str :only [hgl?] ])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(defn- onNewKey [^KeyStore keystore ^String nm
                 ^KeyStore$PrivateKeyEntry pkey
                 ^KeyStore$ProtectionParameter pm]

  (let [ cc (.getCertificateChain pkey) ]
    (doseq [ ^Certificate c (seq cc) ]
      (.setCertificateEntry keystore (new-alias) c))
    (.setEntry keystore nm pkey pm)))

(defn- getCAs [^KeyStore keystore tca root]
  (let [ en (.aliases keystore) ]
    (loop [ rc (transient []) ]
        (if (not (.hasMoreElements en))
          (persistent! rc)
          (let [ ^String a (.nextElement en) ]
            (if (.isCertificateEntry keystore a)
              (let [ ^KeyStore$TrustedCertificateEntry ce (.getEntry keystore a nil)
                     ^X509Certificate cert (.getTrustedCertificate ce)
                     issuer (.getIssuerX500Principal cert)
                     subj (.getSubjectX500Principal cert)
                     matched (and (not (nil? issuer)) (= issuer subj)) ]
                (if (or (and root (not matched)) (and tca matched))
                  (recur rc)
                  (recur (conj! rc cert))))
              (recur rc)))))))

(defprotocol CryptoStore
  ""
  (addKeyEntity [_ ^bytes bits ^comzotohcljc.crypto.codec.Password pwdObj] )
  (addCertEntity [_ ^bytes bits] )
  (trustManagerFactory [_] )
  (keyManagerFactory [_] )
  (certAliases [_] )
  (keyAliases [_] )
  (keyEntity [_ ^String nm ^comzotohcljc.crypto.codec.Password pwdObj] )
  (certEntity [_ ^String nm] )
  (removeEntity [_ ^String nm] )
  (intermediateCAs [_] )
  (rootCAs [_] )
  (trustedCerts [_] )
  (addPKCS7Entity [_ ^bytes bits] ))

(defn- mkStore ^KeyStore [^KeyStore keystore]
  (case (.getType keystore)
    "PKCS12" (get-pkcsStore)
    "JKS" (get-jksStore)
    (throw (IllegalArgumentException. "wrong keystore type."))))

(defn make-crypto-store ""

  ^comzotohcljc.crypto.stores.CryptoStore
  [^KeyStore keystore
   ^comzotohcljc.crypto.codec.Password passwdObj]

  (reify CryptoStore

    (addKeyEntity [this bits pwdObj]
      ;; we load the p12 content into an empty keystore, then extract the entry
      ;; and insert it into the current one.
      (let [ ch (.toCharArray ^comzotohcljc.crypto.codec.Password pwdObj)
             tmp (doto (mkStore keystore) (.load bits ch))
             pp (KeyStore$PasswordProtection. ch)
             ^KeyStore$PrivateKeyEntry pkey (.getEntry tmp (.nextElement (.aliases tmp)) pp) ]
        (onNewKey this (new-alias) pkey pp)))

    (addCertEntity [_ bits]
      (let [ fac (CertificateFactory/getInstance "X.509")
             ^X509Certificate c (.generateCertificate fac bits) ]
        (.setCertificateEntry keystore (new-alias) c)))

    (trustManagerFactory [_]
      (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
        (.init keystore)))

    (keyManagerFactory [_]
      (doto (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
        (.init keystore  (.toCharArray passwdObj))))

    (certAliases [_] (cert-aliases keystore))
    (keyAliases [_] (pkey-aliases keystore))

    (keyEntity [_ nm pwdObj]
      (let [ ca (.toCharArray ^comzotohcljc.crypto.codec.Password pwdObj) ]
        (.getEntry keystore nm (KeyStore$PasswordProtection. ca))))

    (certEntity [_ nm]
      (if (hgl? nm)
        (.getEntry keystore ^String nm nil) ))

    (removeEntity [_ nm]
      (if (hgl? nm)
        (when (.containsAlias keystore ^String nm) (.deleteEntry keystore ^String nm))))

    (intermediateCAs [_] (getCAs keystore true false))
    (rootCAs [_] (getCAs keystore false true))

    (trustedCerts [me]
      (map (fn [^String nm]
             (let [ ^KeyStore$TrustedCertificateEntry tc (.certEntity me nm) ]
                (.getTrustedCertificate tc)))
           (.certAliases me)))

    (addPKCS7Entity [_ bits]
      (let [ fac (CertificateFactory/getInstance "X.509")
             certs (.generateCertificates fac bits) ]
        (doseq [ ^X509Certificate c (seq certs) ]
          (.setCertificateEntry keystore (new-alias) c))))

  ))




(def ^:private stores-eof nil)


