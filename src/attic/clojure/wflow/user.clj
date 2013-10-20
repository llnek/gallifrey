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

  comzotohcljc.wflow.user )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.wflow.core Job))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(defprotocol ^:private Work
  (perform [_ fw job] ))

(defn- make-ptask-work [cb]
  (let [ impl (CU/make-mmap) ]
    (reify Work
      (perform [_ fw job]
        (let [ c (fw-popattmt! fw) ]
          (.mm-s impl :res nil)
          (.mm-s impl :cur fw)
          (cb fw job c))))))

(defn make-ptask "Create a PTask Activity."

  ;; function signature (fw job arg)
  [cb]
  (let [ ^comzotohcljc.util.core.MuObj b (make-activity :czc.wflow/PTask) ]
    (.setf! b :task (make-ptask-work cb))
    b))

(defmethod ac-reify :czc.wflow/PTask
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/PTaskPoint))

(defmethod ac-realize! :czc.wflow/PTask
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj fw]
  (let [ w (.getf ac :task) ]
    (.setf! fw :task w)
    fw))

(defmethod fw-evaluate! :czc.wflow/PTaskPoint
  [^comzotohcljc.util.core.MuObj fw job]
  (do
    (debug "[" (.getf fw :pid) "] about to perform work.")
    (let [ ^comzotohcljc.wflow.core.PipelineAPI pipe (fw-pipe* fw)
           ^comzotohcljc.wflow.user.Work w (.getf fw :task)
           np (fw-next* fw)
           ^comzotohcljc.wflow.activity.Activity na (.perform w fw job) ]
      (with-local-vars [rc np]
        (when-not (nil? na)
          (if (= :czc.wflow/Nihil (.moniker na))
            (var-set rc (ac-reify-nihil pipe))
            (var-set rc (ac-reify na @rc))))
        @rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private user-eof nil)


