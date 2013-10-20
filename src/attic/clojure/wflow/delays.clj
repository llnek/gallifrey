;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.wflow.delays)



(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.wflow.core FAsyncResumeToken Job))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn async-resume-token [^comzotohcljc.util.core.MuObj fw]
  (reify
    FAsyncResumeToken
      (resume [_ resArg]
        (let [ np (fw-next* fw) ]
          (when-not (nil? np)
            (fw-setattmt! np resArg)
            (fw-rerun np))))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delay

(defn make-delay "Create a Delay Activity."
  [delayMillis]
  (let [ ^comzotohcljc.util.core.MuObj b (make-activity :czc.wflow/Delay) ]
    (.setf! b :delayMillis delayMillis)
    b))

(defmethod ac-reify :czc.wflow/Delay
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/DelayPoint))

(defmethod ac-realize! :czc.wflow/Delay
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj fw]
  (let [ d (.getf ac :delayMillis) ]
    (.setf! fw :delayMillis d)
    fw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AsyncWait

(defn make-asyncwait "Make a AsyncWait Activity."
  []
  (let [ b (make-activity :czc.wflow/AsyncWait) ]
    b))

(defmethod ac-reify :czc.wflow/AsyncWait
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/AsyncWaitPoint))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private delays-eof nil)


