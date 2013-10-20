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

(ns ^{ :doc "Ways to generate an unique id."
       :author "kenl" }

  comzotohcljc.util.guids)

(import '(java.lang StringBuilder) )
(import '(java.net InetAddress) )
(import '(java.lang Math) )
(import '(java.security SecureRandom))

(use '[ comzotohcljc.util.core :only [now-millis TryC new-random] ])
(use '[ comzotohcljc.util.bytes :only [read-int read-long] ])
(use '[ comzotohcljc.util.seqnum :only [next-int] ])
(use '[ comzotohcljc.util.str :only [left right] ])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;(def ^:private  _CHARS (.toCharArray "0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"))
(def ^:private  _CHARS (.toCharArray "YcQnPuzVAvpi7taGj1XwoJbIK3smye96NlHrR2DZS0CUxkLF5O4g8fBTqMEdhW"))
(def ^:private  _UUIDLEN 36)

(def ^:private LONG_MASK "0000000000000000")
(def ^:private INT_MASK "00000000")

(defn- fmt ""
  ^String [^String pad ^String mask]
  (let [ mlen (.length mask)
         plen (.length pad) ]
    (if (>= mlen plen)
      (.substring mask 0 plen)
      (.toString (.replace (StringBuilder. pad) (- plen mlen) plen mask ) ))) )

(defn- fmt-int ^String [nm] (fmt INT_MASK (Integer/toHexString nm)))
(defn- fmt-long ^String [nm] (fmt LONG_MASK (Long/toHexString nm)))

(defn- splitTime []
  (let [ s (fmt-long (now-millis))
         n (.length s) ]
    [ (left s (/ n 2)) (right s (max 0 (- n (/ n 2 )) )) ] ))

(defn- maybeSetIP ^long []
  (TryC
    (let [ neta (InetAddress/getLocalHost)
           b (.getAddress neta) ]
      (if (.isLoopbackAddress neta )
        (.nextLong (new-random))
        (if (= 4 (alength b)) (long (read-int b)) (read-long b) )
        ))
    ))

(def ^:private _IP (Math/abs (maybeSetIP)) )

(defn new-uuid "RFC4122, version 4 form."
  ^String []
  ;; At i==19 set the high bits of clock sequence as per rfc4122, sec. 4.1.5
  (let [ rc (char-array _UUIDLEN)
         rnd (new-random) ]
    (dotimes [ n (alength rc) ]
      (aset-char rc n (case n
        (8 13 18 23) \-
        (14) \4
        (let [ d (Double. (* (.nextDouble rnd) 16))
               r (bit-or 0 (.intValue d))
               pos (if (= n 19) (bit-or (bit-and r 0x3) 0x8) (bit-and r 0xf) ) ]
          (aget ^chars _CHARS pos))) ))
    (String. rc)))

(defn new-wwid "Return a new guid based on time and ip-address."
  ^String []
  (let [ seed (.nextInt (new-random) (Integer/MAX_VALUE))
         ts (splitTime) ]
      (str (nth ts 0)
           (fmt-long _IP)
           (fmt-int seed)
           (fmt-int (next-int))
           (nth ts 1)) ))


(def ^:private guids-eof  nil)

