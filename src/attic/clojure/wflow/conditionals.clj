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

  comzotohcljc.wflow.conditionals )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.wflow.core ForLoopCountExpr BoolExpr SwitchChoiceExpr Job))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If

(defn make-if "Create a If Activity."
  [expr then else]
  (let [ ^comzotohcljc.util.core.MuObj b (make-activity :czc.wflow/If) ]
    (.setf! b :test expr)
    (.setf! b :then then)
    (.setf! b :else else)
    b))

(defmethod ac-reify :czc.wflow/If
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/IfPoint))

(defmethod ac-realize! :czc.wflow/If
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj fw]
  (let [ np (fw-next* fw)
         t (.getf ac :then)
         c (.getf ac :test)
         e (.getf ac :else) ]
    (.setf! fw :else (if (nil? e) np (ac-reify e np)))
    (.setf! fw :then (ac-reify t np))
    (.setf! fw :test c)
    fw))

(defmethod fw-evaluate! :czc.wflow/IfPoint
  [^comzotohcljc.util.core.MuObj fw job]
  (let [ c (fw-popattmt! fw)
         t (.getf fw :then)
         e (.getf fw :else)
         ^BoolExpr s (.getf fw :test)
         b (.evaluate s job) ]
    (debug "if-(test) = " (if b "OK" "FALSE"))
    (let [ ^comzotohcljc.util.core.MuObj rc (if b t e) ]
      (fw-setattmt! rc c)
      (fw-realize! fw)
      rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; While

(defn make-while "Create a While Activity."
  [expr body]
  (let [ ^comzotohcljc.util.core.MuObj b (make-activity :czc.wflow/While) ]
    (.setf! b :test expr)
    (.setf! b :then body)
    b))

(defmethod ac-reify :czc.wflow/While
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/WhilePoint))

(defmethod ac-realize! :czc.wflow/While
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj fw]
  (let [ b (.getf ac :body)
         ^BoolExpr t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf! fw :body (ac-reify b fw)))
    (.setf! fw :test t)
    fw))

(defn- evalWhilePoint [^comzotohcljc.util.core.MuObj fw job]
  (let [ c (fw-popattmt! fw)
         np (fw-next* fw)
         body (.getf fw :body)
         ^BoolExpr tst (.getf fw :test) ]
    (with-local-vars [ rc fw ]
      (if (not (.evaluate tst job))
        (do
          (debug "test-condition == false")
          (var-set rc np)
          (fw-setattmt! @rc c)
          (fw-realize! fw))
        (do
          (debug "looping - eval body")
          (fw-setattmt! body c)
          (let [ ^comzotohcljc.util.core.MuObj f (fw-evaluate! body job)
                 id (fw-id* f) ]
            (cond
              (or (= id :czc.wflow/AsyncWaitPoint)
                  (= id :czc.wflow/DelayPoint))
              (do (.setf! f :next @rc) (var-set rc f))

              :else
              (when-not (identical? f fw)
                (.setf! fw :body f))))) )
      @rc) ))

(defmethod fw-evaluate! :czc.wflow/WhilePoint
  [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For

;; expr == ForLoopCountExpr
(defn make-for "Create a For Activity."
  [expr body]
  (let [ ^comzotohcljc.util.core.MuObj b (make-activity :czc.wflow/For) ]
    (.setf! b :test expr)
    (.setf! b :body body)
    b))

(defmethod ac-reify :czc.wflow/For
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/ForPoint))

(deftype ^:private ForLoopExpr [ ^:unsynchronized-mutable started
                       ^:unsynchronized-mutable loopCnt
                       ^ForLoopCountExpr loopCountExpr ] BoolExpr
  (evaluate [_ job]
    (try
      (when-not (started)
        (set! loopCnt (.getCount loopCountExpr job))
        (set! started true))
      (debug "current loop " loopCnt)
      (let [ rc (> loopCnt 0) ]
        rc)
      (finally
        (set! loopCnt (dec loopCnt))))))

(defmethod ac-realize! :czc.wflow/For
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf! fw :body (ac-reify b fw)))
    (.setf! fw :test (ForLoopExpr. false 0 t))
    fw))

(defmethod fw-evaluate! :czc.wflow/ForPoint
  [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch
;;

(defn make-switch "Create a Switch Activity."
  [choiceExpr]
  (let [ ^comzotohcljc.util.core.MuObj a (make-activity :czc.wflow/Switch) ]
    (.setf! a :test choiceExpr)
    (.setf! a :default nil)
    (.setf! a :choices {} )
    a))

(defmethod ac-reify :czc.wflow/Switch
  [ac cur]
  (ac-spawnpoint ac cur :czc.wflow/SwitchPoint))

(defmethod ac-realize! :czc.wflow/Switch
  [^comzotohcljc.util.core.MuObj ac ^comzotohcljc.util.core.MuObj cur]
  (let [ df (.getf cur :default)
         cs (.getf ac :choices)
         np (fw-next* cur)
         t  (persistent! (reduce (fn [sum en]
                                    (assoc! sum (first en)
                                            (ac-reify (last en) np)) )
                                 (transient {})
                                 (seq cs))) ]
    (.setf! cur :choices t)
    (when-not (nil? df)
      (.setf! cur :default (ac-reify df np)))
    (.setf! cur :test (.getf ac :test))
    cur))

(defmethod fw-evaluate! :czc.wflow/SwitchPoint
  [^comzotohcljc.util.core.MuObj fw job]
  (let [ cs (.getf fw :choices)
         df (.getf fw :default)
         c (fw-popattmt! fw)
         ^SwitchChoiceExpr e (.getf fw :test) ]
    (let [ h (get cs (.getChoice e job))
           x (if (nil? h) df h) ]
      (fw-setattmt! x c)
      (fw-realize! fw)
      x)))




(def ^:private conditionals-eof nil)

