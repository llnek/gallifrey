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

(ns ^{ :doc "Generate some sequence numbers." 
       :author "kenl" }

  comzotohcljc.util.seqnum )

(import '(java.util.concurrent.atomic AtomicLong AtomicInteger) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private _numInt (AtomicInteger. 1))
(def ^:private  _numLong (AtomicLong. 1))


(defn next-int "Return a sequence number (integer)."
  ^Integer []
  (.getAndIncrement ^AtomicInteger _numInt))

(defn next-long "Return a sequence number (long)."
  ^long []
  (.getAndIncrement ^AtomicLong _numLong))





(def ^:private  seqnum-eof nil)

