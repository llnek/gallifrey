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

  comzotohcljc.tardis.io.core )

(import '(java.util.concurrent ConcurrentHashMap))
(import '(com.zotoh.frwk.server Component Service))

(import '(com.zotoh.frwk.core
  Versioned Hierarchial
  Identifiable Disposable Startable))
(import '(com.zotoh.gallifrey.core Container))
(import '(com.zotoh.gallifrey.io ServletEmitter Emitter))
(import '(java.util Map))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.util.core :only [make-mmap TryC] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol EmitterAPI
  ""
  (enabled? [_] )
  (active? [_] )

  (suspend [_] )
  (resume [_] )

  (release [_ wevt] )
  (hold [_ wevt] )
  (dispatch [_ ev options] ))

(defprotocol WaitEventHolder
  ""
  (timeoutMillis [_ millis] )
  (resumeOnResult [_ res] )
  (onExpiry [_])
  (timeoutSecs [_ secs] ) )

(defprotocol AsyncWaitTrigger
  ""
  (resumeWithResult [_ res] )
  (resumeWithError [_] )
  (emitter [_] ))

(defmulti ioes-reify-event "" (fn [a & args] (:typeid (meta a))))
(defmulti ioes-dispatch "" (fn [a & args] (:typeid (meta a))))
(defmulti ioes-dispose "" (fn [a] (:typeid (meta a))))

(defmulti ioes-suspend "" (fn [a] (:typeid (meta a))))
(defmulti ioes-start "" (fn [a] (:typeid (meta a))))
(defmulti ioes-stop "" (fn [a] (:typeid (meta a))))
(defmulti ioes-resume "" (fn [a] (:typeid (meta a))))

(defmulti ioes-stopped "" (fn [a] (:typeid (meta a))))
(defmulti ioes-started "" (fn [a] (:typeid (meta a))))

(defmethod ioes-started :default [co]
  (info "Emitter "
        (:typeid (meta co))
        " started - OK"))

(defmethod ioes-stopped :default [co]
  (info "Emitter "
        (:typeid (meta co))  " stopped - OK"))

(defmethod ioes-dispose :default [co]
  (info "Emitter "
        (:typeid (meta co))  " disposed - OK"))

(defmethod ioes-suspend :default [co]
  (throw (Exception. "Not Implemented")))

(defmethod ioes-resume :default [co]
  (throw (Exception. "Not Implemented")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn makeEmitter "" [^Container parObj emId emAlias]
  (let [ impl (make-mmap)
         eeid emAlias ]
    (.mm-s impl :backlog (ConcurrentHashMap.))
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Component

        (version [_] "1.0")
        (id [_] eeid)

        Hierarchial

        (parent [_] parObj)

        Emitter

        (container [this] (.parent this))

        Disposable

        (dispose [this] (ioes-dispose this))

        Startable

        (start [this] (ioes-start this))
        (stop [this] (ioes-stop this))

        Service

        (getv [_ k] (.mm-g impl (keyword k)))
        (setv [_ k v]
              (.mm-s impl (keyword k) v))

        EmitterAPI

        (enabled? [_] (if (false? (.mm-g impl :enabled)) false true ))
        (active? [_] (if (false? (.mm-g impl :active)) false true))

        (suspend [this] (ioes-suspend this))
        (resume [this] (ioes-resume this))

        (release [_ wevt]
          (when-not (nil? wevt)
            (let [ ^Map b (.mm-g impl :backlog)
                   wid (.id ^Identifiable wevt) ]
              (debug "emitter releasing an event with id: " wid)
              (.remove b wid))))

        (hold [_ wevt]
          (when-not (nil? wevt)
            (let [ ^Map b (.mm-g impl :backlog)
                   wid (.id ^Identifiable wevt) ]
              (debug "emitter holding an event with id: " wid)
              (.put b wid wevt))))

        (dispatch [_ ev options]
          (TryC
              (.notifyObservers parObj ev options) )) )

      { :typeid emId } )))

(defmethod comp-contextualize :czc.tardis.io/Emitter [co ctx]
  (let [ ^comzotohcljc.tardis.core.sys.Element c ctx ]
    (comp-clone-context co (.getCtx c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.tardis.io/HTTP :czc.tardis.io/Emitter)

(derive :czc.tardis.io/JettyIO :czc.tardis.io/HTTP)
(derive :czc.tardis.io/NettyIO :czc.tardis.io/HTTP)

(derive :czc.tardis.io/WebSockIO :czc.tardis.io/NettyIO)
(derive :czc.tardis.io/NettyMVC :czc.tardis.io/NettyIO)

(derive :czc.tardis.io/RepeatingTimer :czc.tardis.io/Emitter)
(derive :czc.tardis.io/OnceTimer :czc.tardis.io/Emitter)
(derive :czc.tardis.io/ThreadedTimer :czc.tardis.io/RepeatingTimer)

(derive :czc.tardis.io/FilePicker :czc.tardis.io/ThreadedTimer)
(derive :czc.tardis.io/IMAP :czc.tardis.io/ThreadedTimer)
(derive :czc.tardis.io/POP3 :czc.tardis.io/ThreadedTimer)

(derive :czc.tardis.io/JMS :czc.tardis.io/Emitter)
(derive :czc.tardis.io/SocketIO :czc.tardis.io/Emitter)



(def ^:private core-eof nil)

