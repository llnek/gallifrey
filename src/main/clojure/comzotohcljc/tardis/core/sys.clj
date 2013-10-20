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

  comzotohcljc.tardis.core.sys )

(import '(com.zotoh.frwk.core
  Hierarchial Identifiable Versioned))

(use '[comzotohcljc.util.core :only [MuObj make-mmap] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol Element
  ""
  (setCtx! [_ ctx] )
  (getCtx [_] )
  (setAttr! [_ a v] )
  (clrAttr! [_ a] )
  (getAttr [_ a] ) )

(defprotocol Registry
  ""
  (seq* [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti ^comzotohcljc.tardis.core.sys.Element comp-contextualize
  ""
  (fn [a ctx] (:typeid (meta a))))

(defmulti ^comzotohcljc.tardis.core.sys.Element comp-compose
  ""
  (fn [a rego] (:typeid (meta a))))

(defmulti ^comzotohcljc.tardis.core.sys.Element comp-configure
  ""
  (fn [a options] (:typeid (meta a))))

(defmulti ^comzotohcljc.tardis.core.sys.Element comp-initialize
  ""
  (fn [a] (:typeid (meta a))))

(defn synthesize-component ""

  ^comzotohcljc.tardis.core.sys.Element
  [c options]

  (let [ rego (:rego options)
         ctx (:ctx options)
         props (:props options) ]
   (when-not (nil? rego) (comp-compose c rego))
   (when-not (nil? ctx) (comp-contextualize c ctx))
   (when-not (nil? props) (comp-configure c props))
   (comp-initialize c)
   c) )


(defn make-context "" ^comzotohcljc.util.core.MuObj []
  (let [ impl (make-mmap) ]
    (reify MuObj
      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl)))) )

(defn comp-clone-context
  [^comzotohcljc.tardis.core.sys.Element co
   ^comzotohcljc.util.core.MuObj ctx]
  (do
    (when-not (nil? ctx)
      (let [ x (make-context) ]
        (doseq [ [k v] (.seq* ctx) ]
          (.setf! x k v))
        (.setCtx! co x)))
    co))

(defmethod comp-contextualize :default [co ctx]
  (comp-clone-context co ctx))

(defmethod comp-configure :default [co props] co)
(defmethod comp-initialize :default [co] co)
(defmethod comp-compose :default [co rego] co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sys-eof nil)

