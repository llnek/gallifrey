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
  comzotohcljc.dbio.mysql)

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

(def MYSQL-DRIVER "com.mysql.jdbc.Driver")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;; MySQL
(defmethod getBlobKeyword MySQL [db] "LONGBLOB")
(defmethod getTSKeyword MySQL [db] "TIMESTAMP")
(defmethod getDoubleKeyword MySQL [db] "DOUBLE")
(defmethod getFloatKeyword MySQL [db]  "DOUBLE")

(defmethod genEnd MySQL [db table]
  (str "\n) Type=InnoDB" (genExec db) "\n\n"))

(defmethod genAutoInteger MySQL [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db) " NOT NULL AUTO_INCREMENT"))

(defmethod genAutoLong MySQL [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db) " NOT NULL AUTO_INCREMENT"))

(defmethod genDrop MySQL [db table]
  (str "DROP TABLE IF EXISTS " table (genExec db) "\n\n"))


;;(println (getDDL (make-MetaCache testschema) (MySQL.) ))

(def ^:private mysql-eof nil)

