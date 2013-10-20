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

(ns ^{ :doc "Utililties for handling byte[] conversions to/from numbers."
      :author "kenl" }
  comzotohcljc.util.bytes)

(use '[clojure.tools.logging :only [info warn error debug]])

(import '(java.nio ByteBuffer CharBuffer) )
(import '(java.nio.charset Charset) )
(import '(java.io
  ByteArrayOutputStream ByteArrayInputStream
  DataOutputStream DataInputStream) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti write-bytes "Write this long value out as byte[]." class )

(defn to-bytes "Convert char[] to byte[]."
  ^bytes
  [^chars chArray ^String encoding]
  (.array (.encode (Charset/forName encoding)
                   (CharBuffer/wrap chArray)) ) )

(defn to-chars "Convert byte[] to char[]."
  ^chars
  [^bytes byteArray ^String encoding]
  (.array (.decode (Charset/forName encoding)
                   (ByteBuffer/wrap byteArray)) ) )

(defn read-long "Return a long by scanning the byte[]."
  [^bytes byteArray]
  (.readLong (DataInputStream. (ByteArrayInputStream. byteArray)) ))

(defn read-int "Return an int by scanning the byte[]."
  [^bytes byteArray]
  (.readInt (DataInputStream. (ByteArrayInputStream. byteArray)) ))

(defmethod write-bytes Integer
  ^bytes
  [nnum]
  (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
    (let [ ds (DataOutputStream. baos ) ]
      (.writeInt ds (int nnum))
      (.flush ds)
      (.toByteArray baos) )))

(defmethod write-bytes Long
  ^bytes
  [nnum]
  (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
    (let [ ds (DataOutputStream. baos ) ]
      (.writeLong ds ^long nnum)
      (.flush ds)
      (.toByteArray baos) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private bytes-eof nil)

