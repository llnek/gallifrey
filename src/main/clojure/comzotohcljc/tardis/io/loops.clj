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

  comzotohcljc.tardis.io.loops )

(import '(java.util Date Timer TimerTask))
(import '(com.zotoh.gallifrey.io TimerEvent))
(import '(com.zotoh.frwk.core Identifiable Startable))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj TryC] ])
(use '[comzotohcljc.util.process :only [coroutine safe-wait] ])
(use '[comzotohcljc.util.dates :only [parse-date] ])
(use '[comzotohcljc.util.meta :only [get-cldr] ])
(use '[comzotohcljc.util.seqnum :only [next-long] ])
(use '[comzotohcljc.util.str :only [nsb hgl? strim] ])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.io.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmulti loopable-wakeup "" (fn [a & args] (:typeid (meta a)) ))
(defmulti loopable-oneloop "" (fn [a] (:typeid (meta a)) ))
(defmulti loopable-schedule "" (fn [a] (:typeid (meta a)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- config-repeat-timer "" [^Timer tm delays intv func]
  (let [ tt (proxy [TimerTask][]
              (run []
                (TryC
                    (when (fn? func) (func)))))
         [^Date dw ^long ds] delays ]
    (when (instance? Date dw)
      (.schedule tm tt dw ^long intv) )
    (when (number? ds)
      (.schedule tm tt ds ^long intv))) )

(defn- config-timer "" [^Timer tm delays func]
  (let [ tt (proxy [TimerTask][]
              (run []
                (when (fn? func) (func))))
         [^Date dw ^long ds] delays]
    (when (instance? Date dw)
      (.schedule tm tt dw) )
    (when (number? ds)
      (.schedule tm tt ds))) )


(defn- config-timertask "" [^comzotohcljc.tardis.core.sys.Element co]
  (let [ intv (.getAttr co :intervalMillis)
         t (.getAttr co :timer)
         ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         func (fn [] (loopable-wakeup co)) ]
    (if (number? intv)
      (config-repeat-timer t [dw ds] intv func)
      (config-timer t [dw ds] func))
    co))


(defn cfg-loopable "" [^comzotohcljc.tardis.core.sys.Element co cfg]
  (let [ intv (:interval-secs cfg)
         ds (:delay-secs cfg)
         dw (nsb (:delay-when cfg)) ]
    (if (hgl? dw)
      (.setAttr! co :delayWhen (parse-date (strim dw) "yyyy-MM-ddTHH:mm:ss"))
      (do
        (.setAttr! co :delayMillis
                   (* 1000 (Math/min (int 3)
                                     (int (if (number? ds) ds 3)))))))
    (when (number? intv)
      (.setAttr! co :intervalMillis (* 1000 intv)))
    co))

(defn- start-timer "" [^comzotohcljc.tardis.core.sys.Element co]
  (do
    (.setAttr! co :timer (Timer. true))
    (loopable-schedule co)))

(defn- kill-timer "" [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^Timer t (.getAttr co :timer) ]
    (TryC
        (when-not (nil? t) (.cancel t)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeating Timer

(defn makeRepeatingTimer [container]
  (makeEmitter container :czc.tardis.io/RepeatingTimer))

(defmethod ioes-reify-event :czc.tardis.io/RepeatingTimer
  [co & args]
  (let [ eeid (next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        TimerEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (isRepeating [_] true))

      { :typeid :czc.tardis.io/TimerEvent } )))

(defmethod comp-configure :czc.tardis.io/RepeatingTimer
  [co cfg]
  (cfg-loopable co cfg))

(defmethod ioes-start :czc.tardis.io/RepeatingTimer
  [co]
  (start-timer co)
  (ioes-started co))

(defmethod ioes-stop :czc.tardis.io/RepeatingTimer
  [co]
  (kill-timer co)
  (ioes-stopped co))

(defmethod loopable-wakeup :czc.tardis.io/RepeatingTimer
  [^comzotohcljc.tardis.io.core.EmitterAPI co & args]
  (.dispatch co (ioes-reify-event co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod loopable-schedule :default [co]
  (config-timertask co))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Once Timer

(defn makeOnceTimer [container]
  (makeEmitter container :czc.tardis.io/OnceTimer))

(defmethod ioes-reify-event :czc.tardis.io/OnceTimer
  [co & args]
  (let [ eeid (next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        TimerEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (isRepeating [_] false))

      { :typeid :czc.tardis.io/TimerEvent } )))

(defmethod comp-configure :czc.tardis.io/OnceTimer
  [co cfg]
  ;; get rid of interval millis field, if any
  (cfg-loopable co (dissoc cfg :interval-secs)))

(defmethod ioes-start :czc.tardis.io/OnceTimer
  [co]
  (start-timer co)
  (ioes-started co))

(defmethod ioes-stop :czc.tardis.io/OnceTimer
  [co]
  (kill-timer co)
  (ioes-stopped co))

(defmethod loopable-wakeup :czc.tardis.io/OnceTimer
  [^comzotohcljc.tardis.io.core.EmitterAPI co & args]
  (do
    (.dispatch co (ioes-reify-event co))
    (.stop ^Startable co)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threaded Timer

;;(defmethod loopable-oneloop :default [co] nil)

(defmethod loopable-schedule :czc.tardis.io/ThreadedTimer
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ intv (.getAttr co :intervalMillis)
         cl (get-cldr)
         loopy (atom true)
         func (fn []
                (coroutine (fn []
                             (while @loopy (loopable-wakeup co intv)))
                           cl)) ]
    (.setAttr! co :loopy loopy)
    (info "threaded one timer - interval = " intv)
    (func)))

(defmethod loopable-wakeup :czc.tardis.io/ThreadedTimer
  [co & args]
  (do
    (TryC
        (loopable-oneloop co) )
    (safe-wait (first args) )) )

(defmethod ioes-start :czc.tardis.io/ThreadedTimer
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         intv (.getAttr co :intervalMillis)
         loopy (atom true)
         cl (get-cldr)
         func (fn [] (loopable-schedule co)) ]
    (.setAttr! co :loopy loopy)
    (if (or (number? ds) (instance? Date dw))
      (config-timer (Timer.) [dw ds] func)
      (func))
    (ioes-started co)))


(defmethod ioes-stop :czc.tardis.io/ThreadedTimer
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ loopy (.getAttr co :loopy) ]
    (reset! loopy false)
    (ioes-stopped co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private loops-eof nil)


