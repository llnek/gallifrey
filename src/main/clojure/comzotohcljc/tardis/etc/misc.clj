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

  comzotohcljc.tardis.etc.misc )

(import '(com.zotoh.wflow.core FlowError))
(import '(com.zotoh.wflow Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.gallifrey.io IOEvent HTTPEvent HTTPResult))
(import '(com.zotoh.frwk.core Startable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal flows

(defn- make-work [s]
  (reify Work
             (perform [_ cur job arg]
               (let [ ^HTTPEvent evt (.event job)
                      ^HTTPResult res (.getResultObj evt) ]
                 (.setStatus res s)
                 (.replyResult evt)))) )

(defn- make-internal-flow [^Pipeline pipe s]
  (let [ ev (.event (.job pipe)) ]
    (cond
      (instance? HTTPEvent ev)
      (PTask. (make-work s))
      :else
      (throw (FlowError.
               (str "Unhandled event-type \"" (:typeid (meta ev))  "\"."))))))

(deftype FatalErrorFlow [] PipelineDelegate
  (getStartActivity [_ pipe]
    (make-internal-flow pipe 500))
  (onStop [_ pipe ] nil)
  (onError [_ error cur] nil))

(deftype OrphanFlow [] PipelineDelegate
  (getStartActivity [_  pipe]
    (make-internal-flow pipe 501))
  (onStop [_  pipe] nil)
  (onError [_  error cur] nil))


(defn make-FatalErrorFlow ^Startable [job]
  (Pipeline. job "comzotohcljc.tardis.etc.misc.FatalErrorFlow"))

(defn make-OrphanFlow ^Startable [job]
  (Pipeline. job "comzotohcljc.tardis.etc.misc.OrphanFlow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private misc-eof nil)

