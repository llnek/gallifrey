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
  comzotohcljc.dbio.postgresql)

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

(def POSTGRESQL-URL "jdbc:postgresql://{{host}}:{{port}}/{{db}}" )
(def POSTGRESQL-DRIVER "org.postgresql.Driver")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;; Postgresql
(defmethod getTSKeyword :postgresql [db] "TIMESTAMP WITH TIME ZONE")
(defmethod getBlobKeyword :postgresql [db] "BYTEA")
(defmethod getDoubleKeyword :postgresql [db] "DOUBLE PRECISION")
(defmethod getFloatKeyword :postgresql [db] "REAL")

(defmethod genCal :postgresql [db fld] (genTimestamp db fld))

(defmethod genAutoInteger :postgresql [db table fld]
  (genColDef db (genCol fld) "SERIAL" false nil))

(defmethod genAutoLong :postgresql [db table fld]
  (genColDef db (genCol fld) "BIGSERIAL" false nil))

(defmethod genDrop :postgresql [db table]
  (str "DROP TABLE IF EXISTS " table " CASCADE" (genExec db) "\n\n"))

;;(def XXX (.getMetas (make-MetaCache testschema)))
;;(println (getDDL (make-MetaCache testschema) (Postgresql.) ))


(def ^:private postgresql-eof nil)

