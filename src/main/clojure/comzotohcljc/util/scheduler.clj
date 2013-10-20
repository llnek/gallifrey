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

  comzotohcljc.util.scheduler )

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(java.util.concurrent ConcurrentHashMap))
(import '(com.zotoh.frwk.util
  RunnableWithId Schedulable TCore ))
(import '(java.util
  Map Properties
  Timer TimerTask))

(use '[comzotohcljc.util.core :only [uid make-mmap] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- xrefPID [w]
  (if (instance? RunnableWithId w)
    (let [ p (.getId ^RunnableWithId w) ]
      ;;(debug "runnable-with-(pid): " p)
      p)
    nil))

(defprotocol SchedulerAPI
  ""
  (preRun [_ w] )
  (activate [_ options] )
  (deactivate [_] )
  (addTimer [_ task dely] ) )

(defn make-scheduler "Make a Scheduler." [parObj]
  (let [ ;;^comzotohcljc.util.core.MutableMapAPI
         impl (make-mmap) ]
    (with-meta
      (reify

        SchedulerAPI

          (preRun [_ w]
            (let [pid (xrefPID w) ]
              (when-not (nil? pid)
                (let [ ^Map holdQ (.mm-g impl :holdQ)
                       ^Map runQ (.mm-g impl :runQ) ]
                  (.remove holdQ pid)
                  (.put runQ pid w)))) )

          (activate [_ options]
            (let [ ^long t (:threads options)
                   c (TCore. (uid) (if (nil? t) 4 t)) ]
              (.mm-s impl :timer (Timer. (uid) true))
              (.mm-s impl :holdQ (ConcurrentHashMap.))
              (.mm-s impl :runQ (ConcurrentHashMap.))
              (.mm-s impl :core c)
              (.start c)))

          (deactivate [_]
              (let [ ^Timer t (.mm-g impl :timer)
                     ^Map hq (.mm-g impl :holdQ)
                     ^Map rq (.mm-g impl :runQ)
                     ^TCore c (.mm-g impl :core) ]
                (.cancel t)
                (.clear hq)
                (.clear rq)
                (.stop c)))

          (addTimer [_ task dely]
            (let [ t (.mm-g impl :timer) ]
              (.schedule ^Timer t ^TimerTask task ^long dely)))

        Schedulable

          ;; called by a *running* task to remove itself from the running queue
          (dequeue [_ w]
            (let [ ^Map runQ (.mm-g impl :runQ)
                   pid (xrefPID w) ]
              (when-not (nil? pid)
                (.remove runQ pid))) )

          (run [this w]
            (let [ ^TCore c (.mm-g impl :core)
                   ^Runnable r w]
              (preRun this r)
              (.schedule c r)) )

          (postpone [me w delayMillis]
            (cond
              (= delayMillis 0)
              (.run me w)

              (< delayMillis 0)
              (.hold me w)

              :else
              (do
                (addTimer me
                  (proxy [TimerTask] []
                    (run [] (.wakeup me w))) delayMillis)) ))

          (hold [this w]
            (let [ pid (xrefPID w) ]
              (.hold this pid w)))

          (hold [_ pid w]
            (when-not (nil? pid)
              (let [ ^Map holdQ (.mm-g impl :holdQ)
                     ^Map runQ (.mm-g impl :runQ) ]
                (.remove runQ pid)
                (.put holdQ pid w) )))

          (wakeup [this w]
            (let [ pid (xrefPID w) ]
              (.wakeAndRun this pid w)))

          (wakeAndRun [this pid w]
            (when-not (nil? pid)
              (let [ ^Map holdQ (.mm-g impl :holdQ)
                     ^Map runQ (.mm-g impl :runQ) ]
                (.remove holdQ pid)
                (.put runQ pid w)
                (.run this w) )))

          (reschedule [this w]
            (when-not (nil? w)
              (.run this w)))

          (dispose [_]
            (let [ ^TCore c (.mm-g impl :core) ]
              (when-not (nil? c) (.dispose c)))) )

      { :typeid (keyword "czc.frwk.util/Scheduler") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











(def ^:private scheduler-eof nil)


