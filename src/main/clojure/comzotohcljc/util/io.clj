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

(ns ^{ :doc "Util functions related to stream/io."
       :author "kenl" }

  comzotohcljc.util.io )

(import '(java.io
  ByteArrayInputStream ByteArrayOutputStream DataInputStream
  FileInputStream FileOutputStream CharArrayWriter OutputStreamWriter
  File InputStream InputStreamReader OutputStream Reader Writer))
(import '(java.util.zip GZIPInputStream GZIPOutputStream))
(import '(com.zotoh.frwk.io XData XStream))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.codec.binary Base64))
(import '(org.apache.commons.io IOUtils))
(import '(org.xml.sax InputSource))
(import '(java.nio.charset Charset))

(use '[ comzotohcljc.util.core :only [Try!] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;(def ^:private HEX_CHS [ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A \B \C \D \E \F ])
(def ^:private HEX_CHS (.toCharArray "0123456789ABCDEF"))
(def ^:private SZ_10MEG (* 1024 1024 10))

(defn make-tmpfile "Create a temp file in the temp dir."
  (^File [] (make-tmpfile "" ""))
  (^File [^String pfx ^String sux]
    (File/createTempFile
      (if (StringUtils/isEmpty pfx) "tmp-" pfx)
      (if (StringUtils/isEmpty sux) ".dat" sux)
      (com.zotoh.frwk.io.IOUtils/workDir)) ))

(defn newly-tmpfile "Create a new temp file, optionally open it for write as stream."
  ([] (newly-tmpfile false))
  ([open]
    (let [ f (make-tmpfile) ]
      (if open [ f (FileOutputStream. f) ] [ f nil ]))) )

(defn streamify "Wrapped these bytes in an input-stream."
  ^InputStream [^bytes bits]
  (if (nil? bits)
    nil
    (ByteArrayInputStream. bits)) )

(defn make-baos "Make a byte array output stream."
  ^ByteArrayOutputStream []
  (ByteArrayOutputStream. (int 4096)))

(defn hexify-chars "Turn bytes into hex chars."
  ^chars [^bytes bits]
  (let [ len (* 2 (if (nil? bits) 0 (alength bits)))
         out (char-array len) ]
    (when (> len 0)
      (loop [ k 0 pos 0 ]
        (if (>= pos len)
          nil
          (let [ n (bit-and (aget ^bytes bits k) 0xff) ]
            (aset-char out pos (aget ^chars HEX_CHS (bit-shift-right n 4))) ;; high 4 bits
            (aset-char out (+ pos 1) (aget ^chars HEX_CHS (bit-and n 0xf))) ;; low 4 bits
            (recur (inc k) (+ 2 pos)) ))))
    out))

(defn hexify-string "Turn bytes into hex string."
  ^String [^bytes bits]
  (if (nil? bits) nil (String. (hexify-chars bits))) )

(defmulti open-file "Open this file path." class)

(defn gzip "Gzip these bytes."
  ^bytes [^bytes bits]
  (if (nil? bits)
    nil
    (let [ baos (make-baos) ]
      (with-open [ g (GZIPOutputStream. baos) ]
                (.write g bits, 0, (alength bits)))
      (.toByteArray baos))))

(defn gunzip "Gunzip these bytes."
  ^bytes [^bytes bits]
  (if (nil? bits)
    nil
    (IOUtils/toByteArray (GZIPInputStream. (streamify bits))) ))

(defn reset-stream! "Call reset on this input stream."
  [^InputStream inp]
  (Try!
    (when-not (nil? inp)  (.reset inp)) ))

(defmethod open-file String
  ^XStream [ ^String fp]
  (if (nil? fp) nil (XStream. (File. fp))))

(defmethod open-file File
  ^XStream [^File f]
  (if (nil? f) nil (XStream. f)))

(defn from-gzb64 "Unzip content which is base64 encoded + gziped."
  ^bytes [^String gzb64]
  (if (nil? gzb64) nil (gunzip (Base64/decodeBase64 gzb64))) )

(defn to-gzb64 "Zip content and then base64 encode it."
  ^String [^bytes bits]
  (if (nil? bits) nil (Base64/encodeBase64String (gzip bits))) )

(defn available "Get the available bytes in this stream."
  [^InputStream inp]
  (if (nil? inp) 0 (.available inp)) )

(defn copy-stream "Copy content from this input-stream to a temp file."
  ^File [^InputStream inp]
  (let [ [^File fp ^OutputStream os] (newly-tmpfile true) ]
    (try
      (IOUtils/copy inp os)
      (finally
        (IOUtils/closeQuietly os)))
    fp))

(defn copy-bytes "Copy x number of bytes from the source input-stream."
  [^InputStream src ^OutputStream out bytesToCopy]
  (when (> bytesToCopy 0)
    (IOUtils/copyLarge src out 0 ^long bytesToCopy)) )

(defn reset-source! "Reset an input source."
  [^InputSource inpsrc]
  (when-not (nil? inpsrc)
    (let [ rdr (.getCharacterStream inpsrc)
           ism (.getByteStream inpsrc) ]
      (Try!
        (when-not (nil? ism) (.reset ism)) )
      (Try!
        (when-not (nil? rdr) (.reset rdr)) ))))

(defn make-xdata "Return a newly created XData."
  (^XData [] (make-xdata false))
  (^XData [usefile] (if usefile (XData. (make-tmpfile)) (XData.)) ))

(defn- swap-bytes "" [^ByteArrayOutputStream baos]
  (let [ [^File fp ^OutputStream os] (newly-tmpfile true) ]
    (doto os (.write (.toByteArray baos)) (.flush))
    (.close baos)
    [fp os]))

(defn- swap-read-bytes "" [^InputStream inp ^ByteArrayOutputStream baos]
  (let [ [^File fp ^OutputStream os] (swap-bytes baos)
         bits (byte-array 4096) ]
    (try
      (loop [ c (.read inp bits) ]
        (if (< c 0)
          (XData. fp)
          (if (= c 0)
            (recur (.read inp bits))
            (do (.write os bits 0 c)
                (recur (.read inp bits))))))
      (finally
        (IOUtils/closeQuietly os)))) )

(defn- slurp-bytes ^XData [^InputStream inp lmt]
  (let [ bits (byte-array 4096)
         baos (make-baos) ]
    (loop [ c (.read inp bits) cnt 0 ]
      (if (< c 0)
        (XData. baos)
        (if (= c 0)
          (recur (.read inp bits) cnt)
          (do ;; some data
            (.write baos bits 0 c)
            (if (> (+ c cnt) lmt)
              (swap-read-bytes inp baos)
              (recur (.read inp bits) (+ c cnt)) )))))) )

(defn- swap-chars ""  [^CharArrayWriter wtr]
  (let [ [^File fp ^OutputStream out] (newly-tmpfile true)
         bits (.toCharArray wtr)
         os (OutputStreamWriter. out "utf-8") ]
    (doto os (.write bits) (.flush))
    (IOUtils/closeQuietly wtr)
    [fp os]))

(defn- swap-read-chars ^XData [^Reader inp ^CharArrayWriter wtr]
  (let [ [^File fp ^Writer os] (swap-chars wtr)
         bits (char-array 4096) ]
    (try
      (loop [ c (.read inp bits) ]
        (if (< c 0)
          (XData. fp)
          (if (= c 0)
            (recur (.read inp bits))
            (do (.write os bits 0 c)
                (recur (.read inp bits))))))
      (finally
        (IOUtils/closeQuietly os)))) )

(defn- slurp-chars ^XData [^Reader inp lmt]
  (let [ wtr (CharArrayWriter. (int 10000))
         bits (char-array 4096) ]
    (loop [ c (.read inp bits) cnt 0 ]
      (if (< c 0)
        (XData. wtr)
        (if (= c 0)
          (recur (.read inp bits) cnt)
          (do
            (.write wtr bits 0 c)
            (if (> (+ c cnt) lmt)
              (swap-read-chars inp wtr)
              (recur (.read inp bits) (+ c cnt)))))))) )

(defn read-bytes "Read bytes and return a XData."
  (^XData [^InputStream inp usefile] (slurp-bytes inp (if usefile 1 (com.zotoh.frwk.io.IOUtils/streamLimit))))
  (^XData [^InputStream inp] (slurp-bytes inp (com.zotoh.frwk.io.IOUtils/streamLimit))) )

(defn read-chars "Read chars and return a XData."
  (^XData [^Reader rdr] (slurp-chars rdr (com.zotoh.frwk.io.IOUtils/streamLimit)))
  (^XData [^Reader rdr usefile] (slurp-chars rdr (if usefile 1 (com.zotoh.frwk.io.IOUtils/streamLimit)))))

(defn morph-chars "Convert these bytes to chars."
  (^chars [^bytes bits] (morph-chars bits (Charset/forName "utf-8")) )
  (^chars [^bytes bits ^Charset charSet]
;;    (1 to min(b.length, count)).foreach { (i) =>
;;      val b1 = b(i-1)
;;      ch(i-1) = (if (b1 < 0) { 256 + b1 } else b1 ).asInstanceOf[Char]
;;    }
    (if (nil? bits)
      nil
      (IOUtils/toCharArray (streamify bits) charSet))) )

(defn diff? "Tests if both streams are the same or different at byte level."
  [^InputStream inp1 ^InputStream inp2]
  (cond
    (and (nil? inp1) (nil? inp2)) false
    (or (nil? inp1) (nil? inp2)) true
    :else (not (IOUtils/contentEquals inp1 inp2))) )


(def ^:private io-eof nil)

