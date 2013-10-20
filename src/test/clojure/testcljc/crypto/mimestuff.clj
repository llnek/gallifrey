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


(ns testcljc.crypto.mimestuff)

(use '[clojure.test])
(import '(org.apache.commons.io FileUtils IOUtils))
(import '(java.security Policy KeyStore KeyStore$PrivateKeyEntry
  KeyStore$TrustedCertificateEntry SecureRandom))
(import '(java.util Date GregorianCalendar))
(import '(java.io File InputStream ByteArrayOutputStream))
(import '(javax.mail Multipart BodyPart))
(import '(javax.mail.internet MimeBodyPart MimeMessage MimeMultipart))
(import '(javax.activation DataHandler DataSource))
(import '(com.zotoh.frwk.crypto SDataSource))
(import '(com.zotoh.frwk.io XData))
(require '[comzotohcljc.crypto.codec :as RT])
(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.crypto.core :as RU])


(def ^:private ROOTPFX (CU/rc-bytes "com/zotoh/frwk/crypto/test.pfx"))
(def ^:private HELPME (RT/pwdify "helpme"))
(def ^:private ROOTCS
  (ST/make-crypto-store (RU/init-store! (RU/get-pkcsStore) ROOTPFX HELPME) HELPME))

(deftest testcrypto-mimestuff

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
        (let [ msg (RU/new-mimeMsg "" "" inp) ^Multipart mp (.getContent msg) ]
               (and (>= (.indexOf (.getContentType msg) "multipart/mixed") 0)
                    (== (.getCount mp) 2)
                    (not (RU/is-signed? mp))
                    (not (RU/is-compressed? mp))
                    (not (RU/is-encrypted? mp)) ))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS 
                             ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) 
                             HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/SHA512 msg) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               mp (.getContent msg)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/SHA512 mp) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               ^Multipart mp (.getContent msg)
               bp (.getBodyPart mp 1)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               rc (RU/smime-digsig  pk cs RU/SHA512 bp) ]
        (RU/is-signed? rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               mp (RU/smime-digsig  pk cs RU/SHA512 msg)
               baos (IO/make-baos)
               msg2 (doto (RU/new-mimeMsg "" "")
                      (.setContent (cast Multipart mp))
                      (.saveChanges)
                      (.writeTo baos))
               msg3 (RU/new-mimeMsg "" "" (IO/streamify (.toByteArray baos)))
               mp3 (.getContent msg3)
               rc (RU/peeksmime-signedContent mp3) ]
        (instance? Multipart rc))))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
               msg (RU/new-mimeMsg "" "" inp)
               cs (.getCertificateChain pke)
               pk (.getPrivateKey pke)
               mp (RU/smime-digsig  pk cs RU/SHA512 msg)
               baos (IO/make-baos)
               msg2 (doto (RU/new-mimeMsg "" "")
                      (.setContent (cast Multipart mp))
                      (.saveChanges)
                      (.writeTo baos))
               msg3 (RU/new-mimeMsg "" "" (IO/streamify (.toByteArray baos)))
               mp3 (.getContent msg3)
               rc (RU/test-smimeDigSig mp3 cs) ]
        (if (and (not (nil? rc)) (== (count rc) 2))
          (and (instance? Multipart (nth rc 0)) (instance? (MU/bytes-class) (nth rc 1)))
          false))))


(is (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
                s (SDataSource. (CU/bytesify "hello world") "text/plain")
                cs (.getCertificateChain pke)
                pk (.getPrivateKey pke)
                bp (doto (MimeBodyPart.)
                    (.setDataHandler (DataHandler. s)))
                ^BodyPart bp2 (RU/smime-encrypt (nth cs 0) RU/DES_EDE3_CBC bp)
                baos (IO/make-baos)
                msg (doto (RU/new-mimeMsg)
                        (.setContent (.getContent bp2) (.getContentType bp2))
                        (.saveChanges)
                        (.writeTo baos))
                msg2 (RU/new-mimeMsg (IO/streamify (.toByteArray baos)))
                enc (RU/is-encrypted? (.getContentType msg2))
                rc (RU/smime-decrypt [pk] msg2) ]
      ;; rc is a bodypart
           (and (not (nil? rc))
              (> (.indexOf (CU/stringify rc) "hello world") 0))))

(is (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
                s2 (SDataSource. (CU/bytesify "what's up dawg") "text/plain")
                s1 (SDataSource. (CU/bytesify "hello world") "text/plain")
                cs (.getCertificateChain pke)
                pk (.getPrivateKey pke)
                bp2 (doto (MimeBodyPart.)
                      (.setDataHandler (DataHandler. s2)))
                bp1 (doto (MimeBodyPart.)
                      (.setDataHandler (DataHandler. s1)))
                mp (doto (MimeMultipart.)
                     (.addBodyPart bp1)
                     (.addBodyPart bp2))
                msg (doto (RU/new-mimeMsg) (.setContent  mp))
                ^BodyPart bp3 (RU/smime-encrypt (nth cs 0) RU/DES_EDE3_CBC msg)
                baos (IO/make-baos)
                msg2 (doto (RU/new-mimeMsg)
                        (.setContent (.getContent bp3) (.getContentType bp3))
                        (.saveChanges)
                        (.writeTo baos))
                msg3 (RU/new-mimeMsg (IO/streamify (.toByteArray baos)))
                enc (RU/is-encrypted? (.getContentType msg3))
                rc (RU/smime-decrypt [pk] msg3) ]
      ;; rc is a multipart
           (and (not (nil? rc))
              (> (.indexOf (CU/stringify rc) "what's up dawg") 0)
              (> (.indexOf (CU/stringify rc) "hello world") 0))))


(is (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) HELPME)
             cs (.getCertificateChain pke)
             pk (.getPrivateKey pke)
             data (XData. "heeloo world")
             sig (RU/pkcs-digsig pk cs RU/SHA512 data)
             dg (RU/test-pkcsDigSig (nth cs 0) data sig) ]
        (if (and (not (nil? dg)) (instance? (MU/bytes-class) dg))
          true
          false)))

(is (with-open [ inp (CU/rc-stream "com/zotoh/frwk/mime/mime.eml") ]
        (let [ msg (RU/new-mimeMsg "" "" inp)
               bp (RU/smime-compress msg) 
               ^XData x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength ^bytes (.javaBytes x)) 0) )
            true
            false))))

(is (let [ bp (RU/smime-compress "text/plain" (XData. "heeloo world"))
           baos (IO/make-baos)
           ^XData x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength ^bytes (.javaBytes x)) 0) )
            true
            false)))

(is (let [ bp (RU/smime-compress "text/plain" "blah-blah" "some-id" (XData. "heeloo world"))
           baos (IO/make-baos)
           ^XData x (RU/smime-decompress bp) ]
          (if (and (not (nil? x))
                    (> (alength ^bytes (.javaBytes x)) 0) )
            true
            false)))

(is (let [ f (RU/fingerprint-sha1 (CU/bytesify "heeloo world")) ]
  (if (and (not (nil? f)) (> (.length f) 0))
    true
    false)) )

(is (let [ f (RU/fingerprint-md5 (CU/bytesify "heeloo world")) ]
  (if (and (not (nil? f)) (> (.length f) 0))
    true
    false)) )

(is (let [f (RU/fingerprint-sha1 (CU/bytesify "heeloo world"))
          g (RU/fingerprint-md5 (CU/bytesify "heeloo world")) ]
  (if (= f g) false true)))





)

(def ^:private mimestuff-eof nil)

;;(clojure.test/run-tests 'testcljc.crypto.mimestuff)

