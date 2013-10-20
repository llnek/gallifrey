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

  comzotohcljc.wflow.core )


(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.frwk.util Schedulable RunnableWithId))
(import '(com.zotoh.wflow.core Job))

(use '[comzotohcljc.util.core :only (MuObj) ])
(require '[comzotohcljc.wflow.activity :as ACT ])
(require '[comzotohcljc.wflow.point :as POI ])

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU ])
(require '[comzotohcljc.util.meta :as MU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol ContainerAPI ""
  (core [_] ))

(defprotocol PipelineDelegateAPI ""
  (getStart [_ pipe] )
  (getStop [_ pipe] )
  (getError [_ pipe error cur] ) )

(defprotocol PipelineAPI ""
  (^ContainerAPI container [_] )
  (isActive [_] )
  (job [_] )
  (onError [_ error curPoint] )
  (start [_] )
  (stop [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for flow points
(defmulti fw-configure! "" (fn [a b c] (:typeid (meta a))))
(defmulti fw-realize! "" (fn [a] (:typeid (meta a))))
(defmulti fw-evaluate! "" (fn [a b] (:typeid (meta a))))


;; for activities
(defmulti ac-realize! "" (fn [a b] (:typeid (meta a))))
(defmulti ac-reify "" (fn [a b] (:typeid (meta a))))

(defmacro fw-pipe* [fw]
  `(.getf ~fw :pipeline))

(defmacro fw-next* [fw]
  `(.getf ~fw :next))

(defmacro fw-id* [fw]
  `(:typeid (meta ~fw)))

(comment
(defmacro fw-gf* [fw p]
  `(.getf ~fw ~p))

(defmacro fw-sf* [fw p v]
  `(.setf ~fw ~p ~v))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fw-runafter

  [^comzotohcljc.util.core.MuObj fw]

  (let [ ^comzotohcljc.wflow.core.PipelineAPI
         pipe (fw-pipe* fw)
         np (fw-next* fw)
         id (fw-id* fw)
         ^comzotohcljc.wflow.core.ContainerAPI 
         ct (.container pipe)
         ^Schedulable c (.core ct) ]

    (cond
      (= id :czc.wflow/DelayPoint)
      (.postpone c np (.getf fw :delayMillis))

      (= id :czc.wflow/AsyncWaitPoint)
      (.hold c np)

      (= id :czc.wflow/NihilPoint)
      (.stop pipe)

      :else
      (.run c fw))
    fw))


(defn fw-rerun "" [^comzotohcljc.util.core.MuObj fw]
  (let [ ^comzotohcljc.wflow.core.PipelineAPI
         pipe (fw-pipe* fw)
         ^comzotohcljc.wflow.core.ContainerAPI
         ct (.container pipe)
         ^Schedulable
         c  (.core ct) ]
    (.reschedule c fw)
    fw))


(defn fw-run "" [^comzotohcljc.util.core.MuObj fw]

  (let [ ^comzotohcljc.wflow.core.PipelineAPI pipe (fw-pipe* fw)
         ^Job job (.job pipe)
         np (fw-next* fw)
         ^comzotohcljc.wflow.core.ContainerAPI
         ct (.container pipe)
         ^Schedulable c (.core ct) ]

    (debug "nested inside fw-run: pipe= " pipe ", job= " job ", fw=" fw ", next=" np)
    (with-local-vars [ rc nil err nil ]
      (try
          (.dequeue c fw)
          (var-set rc (fw-evaluate! fw job))
        (catch Throwable e#
          (var-set err (.onError pipe e# fw))))
      (when-not (nil? @err) (var-set rc @err))
      (if (nil? @rc)
        (do
          ;; indicate skip, happens with joins
          (debug "rc==null => skip."))
        (fw-runafter @rc)))))

;; attmt is data passed back from previous async call, if any
(defn fw-popattmt! "" [^comzotohcljc.util.core.MuObj fw]
  (let [ c (.getf fw :attmt) ]
    (.setf! fw :attmt nil)
    c))

(defn fw-setattmt! "" [^comzotohcljc.util.core.MuObj fw c]
  (do
    (when-not (nil? fw) (.setf! fw :attmt c))
    fw))

(defmethod fw-configure! :default [^comzotohcljc.util.core.MuObj fw
                                   ac 
                                   ^comzotohcljc.util.core.MuObj cur]
  (do
    (.setf! fw :pipeline (fw-pipe* cur))
    (.setf! fw :pid (SN/next-long))
    (.setf! fw :template ac)
    (.setf! fw :next cur)
    fw))

(defmethod fw-realize! :default [^comzotohcljc.util.core.MuObj fw]
  (let [ a (.getf fw :template) ]
    (ac-realize! a fw)
    fw))

(defmethod fw-evaluate! :default [fw job] fw)

(defmethod ac-realize! :default [ac fw] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-flowpoint "" [id]

  (let [ impl (CU/make-mmap)
         nid (SN/next-long) ]
    (with-meta
      (reify

        MuObj

          (seq* [_] (CU/test-cond "not implemented" false))
          (setf! [_ k v] (.mm-s impl k v))
          (getf [_ k] (.mm-g impl k))
          (clear! [_] (.mm-c impl))
          (clrf! [_ k] (.mm-r impl k))

        RunnableWithId
          (getId [_] nid)

        Runnable

          (run [this]
            (CU/TryC
              (fw-run this)))

        comzotohcljc.wflow.point.FlowPoint
          (moniker [this] (:typeid (meta this))) )

      { :typeid  id } )) )

(defn make-activity "" [id]

  (let [ impl (CU/make-mmap) ]
     (with-meta
       (reify

          MuObj

          (seq* [_] (CU/test-cond "not implemented" false))
          (setf! [_ k v] (.mm-s impl k v))
          (getf [_ k] (.mm-g impl k))
          (clear! [_] (.mm-c impl))
          (clrf! [_ k] (.mm-r impl k))

          comzotohcljc.wflow.activity.Activity
            (moniker [this] (:typeid (meta this))) )

       { :typeid  id } )))

(defn ac-spawnpoint "" [ac cur id]
  (let [ f (make-flowpoint id) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-nihil "" [] (make-activity :czc.wflow/Nihil) )

(defn ac-reify-nihil "" [pipe]
  (let [ ^comzotohcljc.util.core.MuObj
         f (make-flowpoint :czc.wflow/NihilPoint) ]
    (.setf! f :pipeline pipe)
    (fw-configure! f (make-nihil) f)
    (fw-realize! f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pipeline [^Job theJob ^String delegateClassName]

  (let [ ^comzotohcljc.wflow.core.PipelineDelegateAPI 
         delegate (MU/make-obj delegateClassName)
         pid (SN/next-long)
         impl (CU/make-mmap) ]
    (CU/test-cond "Delegate does not implement PipelineDelegateAPI"
                  (satisfies? PipelineDelegateAPI delegate))
    (CU/test-nonil "the-job" theJob)
    (debug "Pipeline (" pid ") being created...")
    (with-meta
      (reify

        Object

          (finalize [this]
            (printf (str "=====================>" (.toString this) " finz'ed")) )
          (toString [this]
            (str (-> this (.getClass)(.getSimpleName)) "(" pid ")" ))

        PipelineAPI

          (isActive [_] (true? (.mm-g impl :active)))
          (container [_] (.parent theJob))

          (onError [this err cur]
            (let [ h (.getError delegate this err cur) ]
              (error err "")
              (let [ a (if (fn? h)
                         (CU/TryC (h this err cur))) ]
                (if (nil? a)
                  (ac-reify-nihil this)
                  a))))

          (start [this]
            (with-local-vars [ f9 nil ]
              (let [ f1 (ac-reify-nihil this)
                     h (.getStart delegate this) ]
                ;;(when-not (fn? h) (warn "no start function defined by delegate!"))
                (try
                  (let [ ^comzotohcljc.wflow.activity.Activity a (if (fn? h) (h this))
                         ^comzotohcljc.wflow.core.ContainerAPI
                         ct (.container this)
                         ^Schedulable c (.core ct)
                         f2 (cond
                                (or (nil? a) (= :czc.wflow/Nihil (.moniker a)))
                                (ac-reify-nihil this)
                                :else
                                (ac-reify a f1)) ]
                    (var-set f9 f2)
                    (.run c @f9)
                    (.mm-s impl :active true))
                  (catch Throwable e#
                      (onError this e# @f9))))) )

          (job [_] theJob)

          (stop [this]
            (CU/TryC
              ;;(debug "Pipeline " pid " stopping...")
              (let [ h (.getStop delegate this) ]
                (when (fn? h)
                  (h this)))) ) )

      { :typeid :czc.wflow/Pipeline } )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-noFlow [pipe job]
  (let [ f (fn [_] (make-nihil))
         g (reify PipelineDelegateAPI
             (getStart [_ pipe] f)
             (getStop [_ pipe] nil)
             (getError [_ pipe error cur] nil)) ]
    (make-pipeline job g)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.wflow/Composite  :czc.wflow/Activity)
(derive :czc.wflow/AsyncWait  :czc.wflow/Activity)
(derive :czc.wflow/Nihil  :czc.wflow/Activity)
(derive :czc.wflow/PTask  :czc.wflow/Activity)

(derive :czc.wflow/Block  :czc.wflow/Composite)
(derive :czc.wflow/Split  :czc.wflow/Composite)

(derive :czc.wflow/Switch  :czc.wflow/Activity)
(derive :czc.wflow/Delay  :czc.wflow/Activity)

(derive :czc.wflow/Join  :czc.wflow/Activity)
(derive :czc.wflow/And  :czc.wflow/Join)
(derive :czc.wflow/Or  :czc.wflow/Join)

(derive :czc.wflow/Conditional  :czc.wflow/Activity)
(derive :czc.wflow/If  :czc.wflow/Conditional)
(derive :czc.wflow/While  :czc.wflow/Conditional)
(derive :czc.wflow/For  :czc.wflow/While)




(derive :czc.wflow/CompositePoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/AsyncWaitPoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/NihilPoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/PTaskPoint  :czc.wflow/FlowPoint)

(derive :czc.wflow/BlockPoint  :czc.wflow/CompositePoint)
(derive :czc.wflow/SplitPoint  :czc.wflow/CompositePoint)

(derive :czc.wflow/SwitchPoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/DelayPoint  :czc.wflow/FlowPoint)

(derive :czc.wflow/JoinPoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/AndPoint  :czc.wflow/JoinPoint)
(derive :czc.wflow/OrPoint  :czc.wflow/JoinPoint)

(derive :czc.wflow/ConditionalPoint  :czc.wflow/FlowPoint)
(derive :czc.wflow/IfPoint  :czc.wflow/ConditionalPoint)
(derive :czc.wflow/WhilePoint  :czc.wflow/ConditionalPoint)
(derive :czc.wflow/ForPoint  :czc.wflow/WhilePoint)



(def ^:private core-eof nil)

