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

  comzotohcljc.tardis.core.wfs )

(import '( com.zotoh.wflow
  If BoolExpr
  FlowPoint Activity Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.gallifrey.io HTTPEvent HTTPResult))
(import '(com.zotoh.wflow.core Job))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro DefWFTask [ & exprs ] `(PTask. (reify Work ~@exprs )))
(defmacro DefBoolExpr [ & exprs ] `(reify BoolExpr ~@exprs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private wfs-eof nil)


