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
  comzotohcljc.dbio.sqlserver)

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;; SQLServer
(defmethod getBlobKeyword SQLServer [db] "IMAGE")
(defmethod getTSKeyword SQLServer [db] "DATETIME")
(defmethod getDoubleKeyword SQLServer [db] "FLOAT(53)")
(defmethod getFloatKeyword SQLServer [db] "FLOAT(53)")

(defmethod genAutoInteger SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genAutoLong SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genDrop SQLServer [db table]
  (str "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id=object_id('"
       table "')) DROP TABLE " table (genExec db) "\n\n"))


;;(println (getDDL (make-MetaCache testschema) (SQLServer.) ))


(def ^:private sqlserver-eof nil)

