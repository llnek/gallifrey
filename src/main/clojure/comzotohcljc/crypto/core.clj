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

  comzotohcljc.crypto.core)

(use '[clojure.tools.logging :only [info warn error debug] ])
(require '[clojure.math.numeric-tower :as math])

(import '(java.io PrintStream File InputStream IOException
  ByteArrayOutputStream ByteArrayInputStream
  FileInputStream InputStreamReader))
(import '(java.security GeneralSecurityException))
(import '(java.math BigInteger))
(import '(java.net URL))
(import '(java.util Random Date))
(import '(javax.activation DataHandler CommandMap MailcapCommandMap))
(import '(javax.mail BodyPart MessagingException Multipart Session ))
(import '(javax.mail.internet ContentType
  MimeBodyPart MimeMessage MimeMultipart MimeUtility ))
(import '(org.bouncycastle.asn1 ASN1ObjectIdentifier))
(import '(org.bouncycastle.cms CMSAlgorithm))
(import '(org.bouncycastle.cert X509CertificateHolder))
(import '(java.security KeyStore$PasswordProtection))
(import '(java.security KeyStore$PrivateKeyEntry KeyStore$TrustedCertificateEntry))
(import '(java.security Policy PermissionCollection CodeSource
  Permissions KeyPair KeyPairGenerator KeyStore
  MessageDigest PrivateKey Provider PublicKey
  AllPermission SecureRandom Security))
(import '(java.security.cert CertificateFactory
  Certificate X509Certificate))
(import '(org.bouncycastle.jce.provider BouncyCastleProvider))
(import '(org.bouncycastle.asn1.x509 X509Extension))
(import '(org.bouncycastle.asn1 ASN1EncodableVector))
(import '(org.bouncycastle.asn1.cms AttributeTable IssuerAndSerialNumber))
(import '(org.bouncycastle.asn1.smime SMIMECapabilitiesAttribute
  SMIMECapability
  SMIMECapabilityVector
  SMIMEEncryptionKeyPreferenceAttribute))
(import '(org.bouncycastle.asn1.x500 X500Name))
(import '(org.bouncycastle.cms CMSCompressedDataParser
  CMSException
  CMSProcessable
  CMSProcessableByteArray
  CMSProcessableFile
  CMSSignedData
  CMSSignedDataGenerator
  CMSTypedData
  CMSTypedStream
  DefaultSignedAttributeTableGenerator
  Recipient
  RecipientInfoGenerator
  RecipientInformation
  SignerInformation))
(import '(org.bouncycastle.cms.jcajce JcaSignerInfoGeneratorBuilder
  JcaSimpleSignerInfoVerifierBuilder
  JceCMSContentEncryptorBuilder
  JceKeyTransEnvelopedRecipient
  JceKeyTransRecipientInfoGenerator
  ZlibExpanderProvider))
(import '(org.bouncycastle.mail.smime SMIMECompressedGenerator
  SMIMEEnveloped
  SMIMEEnvelopedGenerator
  SMIMEException
  SMIMESigned
  SMIMESignedGenerator
  SMIMESignedParser))
(import '(org.bouncycastle.operator OperatorCreationException ContentSigner))
(import '(org.bouncycastle.operator.jcajce JcaDigestCalculatorProviderBuilder JcaContentSignerBuilder))
(import '(org.bouncycastle.util Store))
(import '(org.bouncycastle.operator.bc BcDigestCalculatorProvider))
(import '(javax.security.auth.x500 X500Principal))
(import '(org.bouncycastle.cms.jcajce JceKeyTransRecipientId))
(import '(org.bouncycastle.mail.smime SMIMEEnvelopedParser))
(import '(org.apache.commons.mail DefaultAuthenticator))
(import '(org.bouncycastle.cert.jcajce JcaCertStore
  JcaX509CertificateConverter
  JcaX509ExtensionUtils
  JcaX509v1CertificateBuilder
  JcaX509v3CertificateBuilder))
(import '(org.bouncycastle.cms CMSProcessableByteArray
  CMSSignedDataGenerator CMSSignedGenerator))
(import '(org.bouncycastle.cms.jcajce JcaSignerInfoGeneratorBuilder))
(import '(org.bouncycastle.openssl PEMParser))
(import '(org.bouncycastle.operator
  DigestCalculatorProvider ContentSigner))
(import '(org.bouncycastle.operator.jcajce
  JcaDigestCalculatorProviderBuilder JcaContentSignerBuilder))
(import '(org.bouncycastle.pkcs
  PKCS10CertificationRequestBuilder PKCS10CertificationRequest))
(import '(org.bouncycastle.pkcs.jcajce
  JcaPKCS10CertificationRequestBuilder))
(import '(javax.crypto Cipher
  KeyGenerator Mac SecretKey))
(import '(javax.crypto.spec SecretKeySpec))
(import '(javax.net.ssl X509TrustManager TrustManager))
(import '(javax.security.auth.x500 X500Principal))
(import '(org.apache.commons.codec.binary Hex Base64))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.io FileUtils IOUtils))
(import '(com.zotoh.frwk.crypto SDataSource))
(import '(com.zotoh.frwk.io XData))
(import '(java.lang Math))

(use '[comzotohcljc.util.seqnum :only [next-int] ])
(use '[comzotohcljc.util.core
       :only [bytesify Try! uid get-classname] ])
(use '[comzotohcljc.util.dates :only [plus-months] ])
(use '[comzotohcljc.util.mime
       :only [maybe-stream is-compressed? is-encrypted? is-signed? ]
       :rename { is-compressed? iscmpz?
                 is-encrypted? isencr?
                 is-signed? issigned?
               }
       ])
(use '[comzotohcljc.util.io :only [streamify make-baos reset-stream!] ])
(use '[comzotohcljc.util.str :only [nsb hgl?] ])


(def DES_EDE3_CBC CMSAlgorithm/DES_EDE3_CBC)
(def RC2_CBC CMSAlgorithm/RC2_CBC)
(def IDEA_CBC CMSAlgorithm/IDEA_CBC)
(def CAST5_CBC CMSAlgorithm/CAST5_CBC)
(def AES128_CBC CMSAlgorithm/AES128_CBC)
(def AES192_CBC CMSAlgorithm/AES192_CBC)
(def AES256_CBC CMSAlgorithm/AES256_CBC)
(def CAMELLIA128_CBC CMSAlgorithm/CAMELLIA128_CBC)
(def CAMELLIA192_CBC CMSAlgorithm/CAMELLIA192_CBC)
(def CAMELLIA256_CBC CMSAlgorithm/CAMELLIA256_CBC)
(def SEED_CBC CMSAlgorithm/SEED_CBC)
(def DES_EDE3_WRAP CMSAlgorithm/DES_EDE3_WRAP)
(def AES128_WRAP CMSAlgorithm/AES128_WRAP)
(def AES256_WRAP CMSAlgorithm/AES256_WRAP)
(def CAMELLIA128_WRAP CMSAlgorithm/CAMELLIA128_WRAP)
(def CAMELLIA192_WRAP CMSAlgorithm/CAMELLIA192_WRAP)
(def CAMELLIA256_WRAP CMSAlgorithm/CAMELLIA256_WRAP)
(def SEED_WRAP CMSAlgorithm/SEED_WRAP)
(def ECDH_SHA1KDF CMSAlgorithm/ECDH_SHA1KDF)

(def EXPLICIT_SIGNING :EXPLICIT)
(def IMPLICIT_SIGNING :IMPLICIT)
(def DER_CERT :DER)
(def PEM_CERT :PEM)

(def SHA512 "SHA512withRSA")
(def SHA256 "SHA256withRSA")
(def SHA1 "SHA1withRSA")
(def SHA_512 "SHA-512")
(def SHA_1 "SHA-1")
(def SHA_256 "SHA-256")
(def MD_5 "MD5")
(def MD5 "MD5withRSA")

(def AES256_CBC  "AES256_CBC")
(def BFISH "BlowFish")
(def PKCS12 "PKCS12")
(def JKS "JKS")
(def SHA1 "SHA1")
(def MD5 "MD5")
(def RAS  "RAS")
(def DES  "DES")
(def RSA  "RSA")
(def DSA  "DSA")

(def ^:private DEF_ALGO "SHA1WithRSAEncryption")
(def ^:private DEF_MAC "HmacSHA512")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn assert-jce "This function should fail if the non-restricted (unlimited-strength) jce files are not placed in jre-home"
  []
  (let [ kgen (doto (KeyGenerator/getInstance BFISH) (.init 256))
         cipher (doto (Cipher/getInstance BFISH)
                      (.init (Cipher/ENCRYPT_MODE)
                             (SecretKeySpec. (.. kgen generateKey getEncoded) BFISH))) ]
    (.doFinal cipher (bytesify "This is just an example"))))

(def ^Provider _BCProvider (let [ bcp (BouncyCastleProvider.) ] (Security/addProvider bcp) bcp))

(assert-jce)

(let [ ^MailcapCommandMap mcm (CommandMap/getDefaultCommandMap) ]
  (doto mcm
    (.addMailcap (str "application/pkcs7-signature;; "
            "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_signature"))
  (.addMailcap (str "application/pkcs7-mime;; " "x-java-content-handler=org.bouncycastle.mail.smime.handlers.pkcs7_mime"))
  (.addMailcap (str "application/x-pkcs7-signature;; "
          "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_signature") )
  (.addMailcap (str "application/x-pkcs7-mime;; "
            "x-java-content-handler=org.bouncycastle.mail.smime.handlers.x_pkcs7_mime"))
  (.addMailcap (str "multipart/signed;; "
          "x-java-content-handler=org.bouncycastle.mail.smime.handlers.multipart_signed") )))


(defn pkcs-file? "" [^URL keyUrl]
  (not (-> keyUrl (.getFile) (.toLowerCase) (.endsWith ".jks"))))

(defn make-MsgDigest "Get a message digest instance."
  ^MessageDigest [^String algo]
  (MessageDigest/getInstance (nsb algo) _BCProvider))

(defn next-serial "Get a random Big Integer."
  ^BigInteger []
  (let [ r (Random. (.getTime (Date.))) ]
    (BigInteger/valueOf (Math/abs (.nextLong r)))) )

(defn dbg-provider "" [^PrintStream os]
  (Try!
    (.list _BCProvider os)))

(defn get-srand "Get a secure random."
  ^SecureRandom
  []
  (SecureRandom/getInstance "SHA1PRNG" ))

(defn new-alias "Generate a new name."
  ^String []
  (str "" (System/currentTimeMillis) (next-int)))

(defn- find-aliases "" [^KeyStore keystore pred]
  (let [ en (.aliases keystore) ]
    (loop [ rc (transient []) ]
      (if (not (.hasMoreElements en))
        (persistent! rc)
        (let [ ^String n (.nextElement en) ]
          (if (pred keystore n)
            (recur (conj! rc n))
            (recur rc)))))))

(defn cert-aliases "Enumerate all cert aliases in the key-store."
  [^KeyStore keystore]
  (find-aliases keystore (fn [^KeyStore ks ^String n] (.isCertificateEntry ks n))))

(defn pkey-aliases "Enumerate all key aliases in the key-store."
  [^KeyStore keystore]
  (find-aliases keystore (fn [^KeyStore ks ^String n] (.isKeyEntry ks n))))

(defn- rego-certs ""
  [^KeyStore ks 
   ^comzotohcljc.crypto.codec.Password pwdObj]
  (let [ ^chars ca (if-not (nil? pwdObj)(.toCharArray pwdObj)) ]
    (doseq [ ^String a (pkey-aliases ks) ]
      (let [ ^KeyStore$PrivateKeyEntry pke (.getEntry ks a (KeyStore$PasswordProtection. ca))
             cs (.getCertificateChain pke) ]
        (doseq [ c (seq cs) ]
          (.setCertificateEntry ks (new-alias) c))))))

(defn get-pkcsStore "Create a PKCS12 key-store."
  (^KeyStore [^InputStream inp ^comzotohcljc.crypto.codec.Password pwdObj]
    (let [ ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
           ks (doto (KeyStore/getInstance "PKCS12" _BCProvider)
                (.load inp ca)) ]
      (when-not (nil? inp) (rego-certs ks pwdObj))
      ks))

  (^KeyStore [] (get-pkcsStore nil nil)))

(defn get-jksStore "Create a JKS key-store."

  (^KeyStore [^InputStream inp ^comzotohcljc.crypto.codec.Password pwdObj]
    (let [ ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
           ks (doto (KeyStore/getInstance "JKS" (Security/getProvider "SUN"))
                (.load inp ca)) ]
      (when-not (nil? inp) (rego-certs ks pwdObj))
      ks))

  (^KeyStore [] (get-jksStore nil nil)))

(defmulti init-store! "Initialize the key-store."
  (fn [a b c]
    (cond
      (instance? InputStream b)
      :stream

      (instance? File b)
      :file

      :else
      :bytes)))

(defmethod init-store! :stream
  ^KeyStore [^KeyStore store
             ^InputStream inp
             ^comzotohcljc.crypto.codec.Password pwdObj]

  (let [ ca (if-not (nil? pwdObj) (.toCharArray pwdObj)) ]
    (doto store (.load inp ca))))

(defmethod init-store! :bytes
  ^KeyStore [^KeyStore store
             ^bytes bits
             ^comzotohcljc.crypto.codec.Password pwdObj]

  (init-store! store (streamify bits) pwdObj))

(defmethod init-store! :file
  ^KeyStore [^KeyStore store
             ^File f
             ^comzotohcljc.crypto.codec.Password pwdObj]

  (with-open [ inp (FileInputStream. f) ]
    (init-store! store inp pwdObj)))

(defn conv-cert "Returns a KeyStore$TrustedCertificateEntry."
  ^KeyStore$TrustedCertificateEntry
  [^bytes bits]
  (let [ ks (get-pkcsStore)
         nm (new-alias)
         c (-> (CertificateFactory/getInstance "X.509")
                      (.generateCertificate  (streamify bits))) ]
    (.setCertificateEntry ks nm c)
    (.getEntry ks nm nil)) )

(defn conv-pkey "Returns a KeyStore$PrivateKeyEntry."
  ^KeyStore$PrivateKeyEntry
  [^bytes bits
   ^comzotohcljc.crypto.codec.Password pwdObj]

  (let [ ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
         ks (get-pkcsStore) ]
    (.load ks (streamify bits) ca)
    (.getEntry ks
                (nsb (first (pkey-aliases ks)))
                (KeyStore$PasswordProtection. ca))))

(defn make-easyPolicy "Make a Policy that enables all permissions."
  ^Policy
  []
  (proxy [Policy] []
    (getPermissions [_ cs]
      (doto (Permissions.) (.add (AllPermission.))))))

(defn gen-mac "Generate a Message Auth Code."
  (^String [^bytes skey ^String data] (gen-mac skey data DEF_MAC))
  (^String [^bytes skey ^String data ^String algo]
    (let [ mac (doto (Mac/getInstance algo _BCProvider)
                     (.init (SecretKeySpec. skey algo))
                     (.update (bytesify data))) ]
      (Hex/encodeHexString (.doFinal mac)))))

(defn gen-hash "Generate a Message Digest."
  (^String [^String data] (gen-hash data SHA_512))
  (^String [^String data ^String algo]
    (let [ dig (MessageDigest/getInstance algo)
           b (.digest dig (bytesify data)) ]
      (Base64/encodeBase64String b))) )

(defn make-keypair "Make a Asymmetric key-pair."
  ^KeyPair
  [^String algo keylen]
  (let [ kpg (doto (KeyPairGenerator/getInstance algo _BCProvider)
                   (.initialize ^long keylen (get-srand))) ]
    (debug "Generating keypair for algo " algo ", length " keylen)
    (.generateKeyPair kpg)) )

(defn- loadPKCS12Key
  ^KeyStore$PrivateKeyEntry
  [^URL p12File
   ^comzotohcljc.crypto.codec.Password pwdObj]

  (with-open [ inp (.openStream p12File) ]
    (let [ ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
           ks (doto (get-pkcsStore)
                    (.load inp ca))
           rc (.getEntry ks (nsb (.nextElement (.aliases ks)))
                         (KeyStore$PasswordProtection. ca)) ]
      rc)) )

(defn- fmtPEM
  ^bytes
  [^String top ^String end ^bytes bits]

  (let [ bs (Base64/encodeBase64 bits)
         nl (bytesify "\n")
         baos (make-baos)
         len (alength bs)
         bb (byte-array 1) ]
    (.write baos (bytesify top))
    (loop [pos 0]
      (if (= pos len)
        (do (.write baos (bytesify end)) (.toByteArray baos))
        (do
          (when (and (> pos 0) (= (mod pos 64) 0)) (.write baos nl))
          (aset bb 0 (aget ^bytes bs pos))
          (.write baos bb)
          (recur (inc pos)))))) )

(defn export-pkey "Export Private Key."
  ^bytes
  [^PrivateKey pkey ^clojure.lang.Keyword fmt]

  (let [ bits (.getEncoded pkey) ]
    (if (= fmt PEM_CERT)
      (fmtPEM "-----BEGIN RSA PRIVATE KEY-----\n"
              "\n-----END RSA PRIVATE KEY-----\n"
              bits)
      bits)) )

(defn export-cert "Export Certificate."
  ^bytes
  [^X509Certificate cert ^clojure.lang.Keyword fmt]

  (let [ bits (.getEncoded cert) ]
    (if (= fmt PEM_CERT)
      (fmtPEM "-----BEGIN CERTIFICATE-----\n"
              "-----END CERTIFICATE-----\n"
              bits)
      bits)) )

(defn make-csrreq "Make a PKCS10 - csr-request."
  [keylen ^String dnStr ^clojure.lang.Keyword fmt]
  (do
    (debug "make-csrreq: dnStr= " dnStr ", key-len= " keylen)
    (let [ ^KeyPair kp (make-keypair (nsb RSA) keylen)
           k (.getPrivate kp)
           u (.getPublic kp)
           csb (JcaContentSignerBuilder. (nsb DEF_ALGO))
           cs (.build (.setProvider csb _BCProvider) k)
           xdn (X500Principal. dnStr)
           rbr (JcaPKCS10CertificationRequestBuilder. xdn u)
           bits (.getEncoded (.build rbr cs))
           rc (if (= fmt PEM_CERT)
                (fmtPEM "-----BEGIN CERTIFICATE REQUEST-----\n"
                        "\n-----END CERTIFICATE REQUEST-----\n"
                        bits)
                bits) ]
      [ rc (export-pkey k fmt) ] )) )

;; generate self-signed cert
;; self signed-> issuer is self

(defn- mkSSV1Cert

  [^Provider pv ^KeyPair kp options]

    (let [ ^String dnStr (:dnStr options)
           ^Date start (:start options)
           ^Date end (:end options)
           ^long keylen (:keylen options)
           ^String algo (:algo options)
           dnName (X500Principal. dnStr)
           prv (.getPrivate kp)
           pub (.getPublic kp)
           bdr (JcaX509v1CertificateBuilder. dnName
                                             (next-serial)
                                             start
                                             end
                                             dnName pub)
           cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) prv)
           cert (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv)
                                 (.build bdr cs)) ]
      (.checkValidity cert (Date.))
      (.verify cert pub)
      (debug "mkSSV1Cert: dn= " dnStr ", algo= " algo ", start=" start ", end=" end )
      [cert prv]) )

(defn- mkSSV1

  ^bytes
  [^KeyStore ks ^KeyPair kp ^comzotohcljc.crypto.codec.Password pwdObj options]

    (let [ pv (.getProvider ks)
           [^Certificate cert ^PrivateKey pkey] (mkSSV1Cert pv kp options)
           ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
           baos (make-baos) ]
      (.setKeyEntry ks (uid) pkey ca (into-array Certificate [cert] ))
      (.store ks baos ca)
      (.toByteArray baos)) )

(defn make-pkcs12 "Make a PKCS12 object from key and cert."

  [^bytes keyPEM ^bytes certPEM
   ^comzotohcljc.crypto.codec.Password pwdObj ^File out]

  (let [ ct (.getTrustedCertificate (conv-cert certPEM))
         rdr (InputStreamReader. (streamify keyPEM))
         ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
         baos (make-baos)
         ss (get-pkcsStore)
         ^KeyPair kp (.readObject (PEMParser. rdr)) ]
    (.setKeyEntry ss (uid) (.getPrivate kp) ca (into-array Certificate [ct]))
    (.store ss baos ca)
    (FileUtils/writeByteArrayToFile out (.toByteArray baos))))

(defn make-ssv1PKCS12 "Make a SSV1 (root level) type PKCS12 object."

  [^String dnStr ^comzotohcljc.crypto.codec.Password pwdObj
   ^File out options]

  (let [ dft { :keylen 1024 :start (Date.) :end (plus-months 12) :algo DEF_ALGO }
         opts (assoc (merge dft options) :dnStr dnStr)
         ^long keylen (:keylen opts)
         kp (make-keypair (nsb RSA) keylen)
         ks (get-pkcsStore)
         rc (mkSSV1 ks kp pwdObj opts) ]
    (FileUtils/writeByteArrayToFile out rc)))

(defn make-ssv1JKS "Make a SSV1 (root level) type JKS object."

  [^String dnStr ^comzotohcljc.crypto.codec.Password pwdObj
   ^File out options]

  (let [ dft { :keylen 1024 :start (Date.) :end (plus-months 12) :algo "SHA1withDSA" }
         opts (assoc (merge dft options) :dnStr dnStr)
         ^long keylen (:keylen opts)
         kp (make-keypair (nsb DSA) keylen)
         ks (get-jksStore)
         rc (mkSSV1 ks kp pwdObj opts) ]
    (FileUtils/writeByteArrayToFile out rc)))

(defn- mkSSV3Cert

  [^Provider pv ^KeyPair kp  issuerObjs options ]

  (let [ ^String dnStr (:dnStr options)
         ^Date start (:start options)
         ^Date end (:end options)
         ^String algo (:algo options)
         ^long keylen (:keylen options)
         ^PrivateKey issuerKey (last issuerObjs)
         ^X509Certificate issuer (first issuerObjs)
         subject (X500Principal. dnStr)
         prv (.getPrivate kp)
         pub (.getPublic kp)
         bdr (JcaX509v3CertificateBuilder. issuer (next-serial)
                                           start end subject pub)
         exUte (JcaX509ExtensionUtils.)
         cs (.build (.setProvider (JcaContentSignerBuilder. algo) pv) issuerKey) ]

    (.addExtension bdr X509Extension/authorityKeyIdentifier false
                   (.createAuthorityKeyIdentifier exUte issuer))
    (.addExtension bdr X509Extension/subjectKeyIdentifier false
                   (.createSubjectKeyIdentifier exUte pub))

    (let [ ct (.getCertificate (.setProvider (JcaX509CertificateConverter.) pv)
                                 (.build bdr cs)) ]
      (.checkValidity ct (Date.))
      (.verify ct (.getPublicKey issuer))
      [ct prv] )))

(defn- mkSSV3

  [ ^KeyStore ks ^comzotohcljc.crypto.codec.Password pwdObj
    issuerObjs options ]

  (let [ ^PrivateKey issuerKey (last issuerObjs)
         ^long keylen (:keylen options)
         ^KeyPair kp (make-keypair (.getAlgorithm issuerKey) keylen)
         issuerCerts (vec (first issuerObjs))
         [^Certificate cert ^PrivateKey pkey] (mkSSV3Cert (.getProvider ks) kp 
                           [ (first issuerCerts) issuerKey ] options)
         ca (if-not (nil? pwdObj)(.toCharArray pwdObj))
         baos (make-baos)
         cs (cons cert issuerCerts) ]
    (.setKeyEntry ks (uid) pkey ca (into-array Certificate cs))
    (.store ks baos ca)
    (.toByteArray baos)))


(defn make-ssv3XXX

  [^String dnStr ^comzotohcljc.crypto.codec.Password pwdObj
   ^File out options]

  (let [ hack (:hack options)
         dft { :keylen 1024 :start (Date.) :end (plus-months 12) :algo (:algo hack) }
         issuerObjs [ (:issuerCerts options) (:issuerKey options) ]
         opts (assoc (merge dft options) :dnStr dnStr)
         ^KeyStore ks (:ks hack)
         opts2 (-> opts (dissoc hack) (dissoc :issuerCerts) (dissoc :issuerKey))
         rc (mkSSV3 ks pwdObj issuerObjs opts2) ]
    (FileUtils/writeByteArrayToFile out rc)))

(defn make-ssv3PKCS12 "Make a SSV3 type PKCS12 object."

  [^String dnStr ^comzotohcljc.crypto.codec.Password pwdObj
   ^File out options]

  (make-ssv3XXX dnStr pwdObj out
                (-> options (assoc :hack { :algo DEF_ALGO :ks (get-pkcsStore) } ))))

;; JKS uses SUN and hence needs to use DSA
(defn make-ssv3JKS "Make a SSV3 JKS object."

  [^String dnStr ^comzotohcljc.crypto.codec.Password pwdObj
   ^File out options]

  (make-ssv3XXX dnStr pwdObj out
                (-> options (assoc :hack { :algo "SHA1withDSA" :ks (get-jksStore) } ))))

(defn export-pkcs7 "Extract and export PKCS7 info from a PKCS12 object."

  [^URL p12File ^comzotohcljc.crypto.codec.Password pwdObj
   ^File fileOut]

  (let [ pkey (loadPKCS12Key p12File pwdObj)
         cc (.getCertificateChain pkey)
         k (.getPrivateKey pkey)
         cl (vec cc)
         cp (.build (.setProvider (JcaDigestCalculatorProviderBuilder.) _BCProvider))
         bdr (JcaSignerInfoGeneratorBuilder. cp)
;;    "SHA1withRSA"
         ;;cs (.build (.setProvider (JcaContentSignerBuilder. (CMSSignedGenerator/DIGEST_SHA512)) _BCProvider) k)
         cs (.build (.setProvider
                      (JcaContentSignerBuilder. (nsb SHA512)) _BCProvider)
                    k)
         gen (CMSSignedDataGenerator.)
         ^X509Certificate x509 (first cl) ]

    (.addSignerInfoGenerator gen (.build bdr cs x509))
    (.addCertificates gen (JcaCertStore. cl))

    (let [ dummy (CMSProcessableByteArray. (bytesify "Hello"))
           bits (.getEncoded (.generate gen CMSSignedGenerator/DATA
                                        dummy false _BCProvider false)) ]
      (FileUtils/writeByteArrayToFile fileOut bits))) )

(defn new-session "Creates a new java-mail session."
  (^Session [] (new-session "" nil))
  (^Session [^String user ^comzotohcljc.crypto.codec.Password pwdObj]
    (Session/getInstance (System/getProperties)
      (if (StringUtils/isEmpty user)
        nil
        (DefaultAuthenticator. user (nsb pwdObj)) ))))

(defn new-mimeMsg "Create a new MIME Message."
  (^MimeMessage [^String user ^comzotohcljc.crypto.codec.Password pwdObj]
     (new-mimeMsg user pwdObj nil))
  (^MimeMessage [] (new-mimeMsg "" nil nil))
  (^MimeMessage [^InputStream inp] (new-mimeMsg "" nil inp))
  (^MimeMessage [^String user ^comzotohcljc.crypto.codec.Password pwdObj ^InputStream inp]
      (let [ s (new-session user pwdObj) ]
        (if (nil? inp) (MimeMessage. s) (MimeMessage. s inp)))) )

(defn is-signed? "Check if this stream-like object/message-part is signed."
  [^Object obj]
  (let [ inp (maybe-stream obj) ]
    (if (nil? inp)
      (if (instance? Multipart obj)
        (let [ ^Multipart mp obj ] (issigned? (.getContentType mp)))
        (throw (IOException. (str "Invalid content: " (get-classname obj)))))
      (try
        (issigned? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (reset-stream! inp))))) )

(defn is-compressed? "Check if this stream-like object/message-part is compressed."
  [^Object obj]
  (let [ inp (maybe-stream obj) ]
    (if (nil? inp)
      (cond
        (instance? Multipart obj)
        (let [ ^Multipart mp obj ]
          (iscmpz? (.getContentType mp)))

        (instance? BodyPart obj)
        (let [ ^BodyPart bp obj ]
          (iscmpz? (.getContentType bp)))

        :else
        (throw (IOException. (str "Invalid content: " (get-classname obj)))))
      (try
        (iscmpz? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (reset-stream! inp))))) )

(defn is-encrypted? "Check if this stream-like object/message-part is encrypted."
  [^Object obj]
  (let [ inp (maybe-stream obj) ]
    (if (nil? inp)
      (cond
        (instance? Multipart obj)
        (let [ ^Multipart mp obj ]
          (isencr? (.getContentType mp)))

        (instance? BodyPart obj)
        (let [ ^BodyPart bp obj ]
          (isencr? (.getContentType bp)))

        :else
        (throw (IOException. (str "Invalid content: " (get-classname obj)))))
      (try
        (isencr? (.getContentType (new-mimeMsg "" "" inp)))
        (finally (reset-stream! inp))))))

(defn get-charset "Deduce the char-set from content-type."
  (^String [^String cType]
    (if (hgl? cType)
      (try
        (nsb (MimeUtility/javaCharset (-> (ContentType. cType) (.getParameter "charset"))))
        (catch Throwable e# (do (warn e# "") "")))
      ""))
  (^String [^String cType ^String dft]
    (let [ cs (get-charset cType) ]
      (nsb (if (hgl? cs) cs (MimeUtility/javaCharset dft))))) )

(defn- make-signerGentor
  ^SMIMESignedGenerator
  [^PrivateKey pkey certs ^String algo]
  (let [ gen (SMIMESignedGenerator. "base64")
         lst (vec certs)
         caps (doto (SMIMECapabilityVector.)
                  (.addCapability SMIMECapability/dES_EDE3_CBC)
                  (.addCapability SMIMECapability/rC2_CBC, 128)
                  (.addCapability SMIMECapability/dES_CBC) )
         signedAttrs (doto (ASN1EncodableVector.)
                        (.add (SMIMECapabilitiesAttribute. caps)))
         ^X509Certificate x0 (first lst)
         ^X509Certificate issuer (if (> (count lst) 1) (nth lst 1) x0)
         issuerDN (.getSubjectX500Principal issuer)
         ;;
         ;; add an encryption key preference for encrypted responses -
         ;; normally this would be different from the signing certificate...
         ;;
         issAndSer (IssuerAndSerialNumber. (X500Name/getInstance (.getEncoded issuerDN)) (.getSerialNumber x0))
         dm1 (.add signedAttrs (SMIMEEncryptionKeyPreferenceAttribute. issAndSer))
         bdr (doto (JcaSignerInfoGeneratorBuilder.
                          (-> (JcaDigestCalculatorProviderBuilder.)
                            (.setProvider _BCProvider)
                            (.build)) )
                (.setDirectSignature true))
         cs (-> (JcaContentSignerBuilder. (nsb algo))
              (.setProvider _BCProvider)
              (.build pkey)) ]

    (.setSignedAttributeGenerator
      bdr
      (DefaultSignedAttributeTableGenerator. (AttributeTable. signedAttrs)))
    (.addSignerInfoGenerator gen (.build bdr cs, x0))
    (.addCertificates gen (JcaCertStore. lst))
    gen))

(defmulti smime-digsig "Generates a MimeMultipart."
  (fn [a b c d]
    (cond
      (instance? MimeMessage d)
      :mimemessage

      (instance? Multipart d)
      :multipart

      (instance? BodyPart d)
      :bodypart

      :else
      (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-digsig :mimemessage

  [^PrivateKey pkey certs ^String algo ^MimeMessage mmsg]
  (do
    ;; force internal processing, just in case
    (.getContent mmsg)
    (-> (make-signerGentor pkey certs algo) (.generate mmsg _BCProvider))) )

(defmethod smime-digsig :multipart

  [^PrivateKey pkey certs ^String algo ^Multipart mp]
  (let [ mm (new-mimeMsg) ]
    (.setContent mm mp)
    (-> (make-signerGentor pkey certs algo) (.generate mm _BCProvider))) )

(defmethod smime-digsig :bodypart

  [^PrivateKey pkey certs ^String algo ^BodyPart bp]

  (let [ ^MimeBodyPart mbp bp ]
    (-> (make-signerGentor pkey certs algo)
      (.generate mbp _BCProvider))) )

(defn- smime-dec
  ^CMSTypedStream
  [^PrivateKey pkey ^SMIMEEnveloped env]
    ;;var  recId = new JceKeyTransRecipientId(cert.asInstanceOf[XCert])
  (let [ rec (-> (JceKeyTransEnvelopedRecipient. pkey)
               (.setProvider _BCProvider))
         it (-> (.getRecipientInfos env) (.getRecipients) (.iterator)) ]
    (loop [ rc nil ]
      (if (or (not (nil? rc)) (not (.hasNext it)))
        rc
        (recur (.getContentStream
                 (let [ ^RecipientInformation ri (.next it) ] ri) rec))))) )

(defmulti smime-decrypt "SMIME decrypt this object."
  (fn [a b]
    (cond
      (instance? MimeMessage b)
      :mimemsg

      (instance? BodyPart b)
      :bodypart

      :else
      (throw (IllegalArgumentException. "wrong type")))))

(defn- smime-loopdec ^bytes [pkeys ^SMIMEEnveloped ev]
  (let [ rc (some (fn [ ^PrivateKey k & args ]
                    (let [ cms (smime-dec k ev) ]
                      (if (nil? cms) false (IOUtils/toByteArray (.getContentStream cms)))))
              pkeys) ]
    (when (nil? rc)
      (throw (GeneralSecurityException. "No matching decryption key")))
    rc))

(defmethod smime-decrypt :mimemsg
  ^bytes
  [pkeys ^MimeMessage mimemsg]
  (smime-loopdec pkeys (SMIMEEnveloped. mimemsg)))

(defmethod smime-decrypt :bodypart
  ^bytes
  [pkeys ^BodyPart part]
  (let [ ^MimeBodyPart bbp part ]
    (smime-loopdec pkeys (SMIMEEnveloped. bbp))) )

(defn peeksmime-signedContent "Get the content ignoring the signing stuff."
  [^Multipart mp]
  (let [ ^MimeMultipart  mmp mp ]
    (-> (SMIMESignedParser. (BcDigestCalculatorProvider.)
        mmp
        (get-charset (.getContentType mp) "binary"))
      (.getContent) (.getContent)) ))

(defn test-smimeDigSig "Verify the signature and return content if ok."
  ([^Multipart mp certs] (test-smimeDigSig mp certs ""))
  ([^Multipart mp certs ^String cte]
    (let [ ^MimeMultipart mmp mp
           sc (if (hgl? cte) (SMIMESigned. mmp cte) (SMIMESigned. mmp))
           s (JcaCertStore. (vec certs))
           sns (-> (.getSignerInfos sc) (.getSigners) )
           rc (some (fn [^SignerInformation si]
                  (let [ c (.getMatches s (.getSID si))
                         it (.iterator c)
                         rc (loop [ ret [] stop false]
                              (if (or stop (not (.hasNext it)))
                                ret
                                (let [ bdr (-> (JcaSimpleSignerInfoVerifierBuilder.)
                                             (.setProvider _BCProvider))
                                       ^X509CertificateHolder hdr (.next it) ]
                                  (if (.verify si (.build bdr hdr))
                                    (let [ digest (.getContentDigest si) ]
                                      (if (nil? digest)
                                        (recur ret false)
                                        (recur [sc digest] true )))
                                    (recur ret false))))) ]
                    (if (empty? rc) nil rc))
                  ) (seq sns)) ]
      (when (empty? rc)
        (throw (GeneralSecurityException. "Failed to verify signature: no matching cert.")) )

      [ (-> (.getContentAsMimeMessage sc (new-session))
          (.getContent))
       (nth rc 1) ] )))


(defmulti smime-decompress "Inflate the compressed content."
  (fn [a]
    (cond
      (instance? InputStream a)
      :stream

      (instance? BodyPart a)
      :bodypart

      :else
      (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-decompress :bodypart
  ^XData
  [^BodyPart bp]
  (smime-decompress (if (nil? bp) nil (.getInputStream bp))))

(defmethod smime-decompress :stream
  ^XData
  [^InputStream inp]
    (if (nil? inp)
      (XData.)
      (let [ cms (-> (CMSCompressedDataParser. inp) (.getContent (ZlibExpanderProvider.))) ]
        (when (nil? cms) (throw (GeneralSecurityException. "Failed to decompress stream: corrupted content")))
        (XData. (IOUtils/toByteArray (.getContentStream cms))))))

(defmulti smime-encrypt "Generates a MimeBodyPart."
  (fn [a b c]
    (cond
      (instance? MimeMessage c)
      :mimemsg

      (instance? Multipart c)
      :multipart

      (instance? BodyPart c)
      :bodypart

      :else
      (throw (IllegalArgumentException. "wrong type")))))

(defmethod smime-encrypt :bodypart

  ^MimeBodyPart
  [^Certificate cert ^String algo ^BodyPart bp]

  (let [ gen (SMIMEEnvelopedGenerator.)
         ^MimeBodyPart mb bp
         ^X509Certificate xc cert ]
    (.addRecipientInfoGenerator gen
        (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider _BCProvider)))
    (.generate gen mb
        (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider _BCProvider) (.build)))))

(defmethod smime-encrypt :mimemsg

  ^MimeBodyPart
  [^Certificate cert ^String algo ^MimeMessage msg]

  (let [ gen (SMIMEEnvelopedGenerator.)
         ^X509Certificate xc cert
         g (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider _BCProvider)) ]
    ;; force message to be processed, just in case.
    (.getContent msg)
    (.addRecipientInfoGenerator gen g)
    (.generate gen msg
      (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider _BCProvider) (.build)))))

(defmethod smime-encrypt :multipart

  ^MimeBodyPart
  [^Certificate cert ^String algo ^Multipart mp]

  (let [ gen (SMIMEEnvelopedGenerator.)
         ^X509Certificate xc cert
         mm (doto (new-mimeMsg) (.setContent mp))
         g (-> (JceKeyTransRecipientInfoGenerator. xc) (.setProvider _BCProvider)) ]
    (.addRecipientInfoGenerator gen g)
    (.generate gen mm
      (-> (JceCMSContentEncryptorBuilder. algo) (.setProvider _BCProvider) (.build)))))

(defn smime-compress "Generates a MimeBodyPart."

  (^MimeBodyPart [^String cType ^XData xdata]
    (let [ gen (SMIMECompressedGenerator.)
           bp (MimeBodyPart.)
           ds (if (.isDiskFile xdata)
                (SDataSource. (.fileRef xdata) cType)
                (SDataSource. (.javaBytes xdata) cType)) ]
      (.setDataHandler bp (DataHandler. ds))
      (.generate gen bp (SMIMECompressedGenerator/ZLIB))))

  (^MimeBodyPart [^MimeMessage msg]
   (do
    (.getContent msg) ;; make sure it's processed, just in case
    (-> (SMIMECompressedGenerator.) (.generate msg (SMIMECompressedGenerator/ZLIB)))) )

  (^MimeBodyPart [^String cType ^String contentLoc ^String cid ^XData xdata]
    (let [ gen (SMIMECompressedGenerator.)
           bp (MimeBodyPart.)
           ds (if (.isDiskFile xdata) (SDataSource. (.fileRef xdata) cType)
                    (SDataSource. (.javaBytes xdata) cType)) ]
      (when (hgl? contentLoc) (.setHeader bp "content-location" contentLoc))
      (when (hgl? cid) (.setHeader bp "content-id" cid))
      (.setDataHandler bp (DataHandler. ds))
      (let [ zbp (.generate gen bp SMIMECompressedGenerator/ZLIB)
             pos (.lastIndexOf cid (int \>))
             cID (if (>= pos 0) (str (.substring cid 0 pos) "--z>") (str cid "--z")) ]
        (when (hgl? contentLoc) (.setHeader zbp "content-location" contentLoc))
        (.setHeader zbp "content-id" cID)
        ;; always base64
        ;;cte="base64"
        (.setHeader zbp "content-transfer-encoding" "base64")
        zbp))) )

(defn pkcs-digsig "SMIME sign some data."
  ^bytes
  [^PrivateKey pkey certs ^String algo ^XData xdata]
    (let [ gen (CMSSignedDataGenerator.)
           cl (vec certs)
           ^X509Certificate cert (first cl)
           cs (-> (JcaContentSignerBuilder. (nsb algo))
                (.setProvider _BCProvider)
                (.build pkey))
           bdr (JcaSignerInfoGeneratorBuilder.
                  (-> (JcaDigestCalculatorProviderBuilder.)
                    (.setProvider _BCProvider)
                    (.build))) ]

      (.setDirectSignature bdr true)
      (.addSignerInfoGenerator gen (.build bdr cs cert))
      (.addCertificates gen (JcaCertStore. cl))

      (let [ ^CMSTypedData cms (if (.isDiskFile xdata) (CMSProcessableFile. (.fileRef xdata))
                      (CMSProcessableByteArray. (.javaBytes xdata))) ]
        (.getEncoded (.generate gen cms false) ))) )

(defn test-pkcsDigSig "Verify the signed object with the signature."
  ^bytes
  [^Certificate cert ^XData xdata ^bytes signature]

    (let [ ^CMSProcessable cproc (if (.isDiskFile xdata)
                   (CMSProcessableFile. (.fileRef xdata))
                   (CMSProcessableByteArray. (.javaBytes xdata)))
           cms (CMSSignedData. cproc signature)
           s (JcaCertStore. [cert])
           sls (-> cms (.getSignerInfos) (.getSigners))
           rc (some (fn [^SignerInformation si]
                       (let [ c (.getMatches s (.getSID si))
                              it (.iterator c) ]
                         (loop [ digest nil stop false ]
                           (if (or stop (not (.hasNext it)))
                             digest
                             (let [ bdr (-> (JcaSimpleSignerInfoVerifierBuilder.)
                                          (.setProvider _BCProvider))
                                    ^X509CertificateHolder hdr (.next it)
                                    ok (.verify si
                                                (.build bdr hdr))
                                    dg (if ok (.getContentDigest si) nil) ]
                               (if (not (nil? dg)) (recur dg true) (recur nil false)))))))
                      (seq sls)) ]
      (when (nil? rc) (throw (GeneralSecurityException. "Failed to decode signature: no matching cert.")))
      rc))

(defn- str-signingAlgo [algo]
  (case algo
    "SHA-512" SMIMESignedGenerator/DIGEST_SHA512
    "SHA-1" SMIMESignedGenerator/DIGEST_SHA1
    "MD5" SMIMESignedGenerator/DIGEST_MD5
    (throw (IllegalArgumentException. (str "Unsupported signing algo: " algo)))))

(defn- finger-print ^String [^bytes data ^String algo]

  (let [ md (MessageDigest/getInstance (nsb algo))
         ret (StringBuilder. (int 256))
         hv (.digest md data)
         tail (dec (alength hv)) ]
    (loop [ i 0 ]
      (if (>= i (alength hv))
        (.toString ret)
        (let [ n (.toUpperCase (Integer/toString (bit-and (aget ^bytes hv i) 0xff) 16)) ]
          (-> ret (.append (if (= (.length n) 1) (str "0" n) n))
                (.append (if (= i tail) "" ":")))
          (recur (inc i)))))))


(defn fingerprint-sha1 "Generate a fingerprint using SHA-1."
  ^String
  [^bytes data]
  (finger-print data SHA_1))

(defn fingerprint-md5 "Generate a fingerprint using MD5."
  ^String
  [^bytes data]
  (finger-print data MD_5))

(defrecord CertDesc
  [ ^X500Principal subj ^X500Principal issuer ^Date notBefore ^Date notAfter ])

(defn desc-certificate "Get some basic info from Certificate."

  ^comzotohcljc.crypto.core.CertDesc
  [^X509Certificate x509]

    (if (nil? x509)
      (->CertDesc nil nil nil nil)
      (->CertDesc (.getSubjectX500Principal x509) (.getIssuerX500Principal x509) (.getNotBefore x509) (.getNotAfter x509))))

(defn desc-cert "Return a object"

  ( ^comzotohcljc.crypto.core.CertDesc
    [^bytes privateKeyBits ^comzotohcljc.crypto.codec.Password pwdObj]
      (let [ pkey (conv-pkey privateKeyBits pwdObj) ]
        (if (nil? pkey)
          (->CertDesc nil nil nil nil)
          (desc-certificate (.getCertificate pkey)) )))

  ( ^comzotohcljc.crypto.core.CertDesc
    [^bytes certBits]
      (let [ cert (conv-cert certBits) ]
        (if (nil? cert)
          (->CertDesc nil nil nil nil)
          (desc-certificate (.getTrustedCertificate cert))))) )

(defn valid-certificate? "Validate this Certificate."
  [^X509Certificate x509]
  (try
    (.checkValidity x509 (Date.))
    (catch Throwable e false)))

(defn valid-pkey? "Validate this Private Key."
  [^bytes keyBits ^comzotohcljc.crypto.codec.Password pwdObj]
    (let [ pkey (conv-pkey keyBits pwdObj) ]
      (if (nil? pkey)
        false
        (valid-certificate? (.getCertificate pkey)))) )

(defn valid-cert? "Validate this Certificate."
  [^bytes certBits]
  (let [ cert (conv-cert certBits) ]
    (if (nil? cert)
      false
      (valid-certificate? (.getTrustedCertificate cert)))) )

(defn intoarray-certs "From a list of TrustedCertificateEntry(s)."
  [certs]
  (if (empty? certs)
    []
    (map (fn [ ^KeyStore$TrustedCertificateEntry x] (.getTrustedCertificate x)) (seq certs))))

(defn intoarray-pkeys "From a list of PrivateKeyEntry(s)."
  [pkeys]
  (if (empty? pkeys)
    []
    (map (fn [^KeyStore$PrivateKeyEntry x] (.getPrivateKey x)) (seq pkeys))))


(defn make-simpleTrustMgr ""
  ^X509TrustManager
  []
    (reify X509TrustManager
      (getAcceptedIssuers [_] (make-array X509Certificate 0))
      (checkClientTrusted [_ x y] nil)
      (checkServerTrusted [_ x y] nil)))























(def ^:private core-eof nil)


