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
  comzotohcljc.dbio.oracle)

(import '(java.util Map HashMap))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- create_sequence_trigger [db table col]
  (str "CREATE OR REPLACE TRIGGER TRIG_" table "\n"
        "BEFORE INSERT ON " table "\n"
        "REFERENCING NEW AS NEW\n"
        "FOR EACH ROW\n"
        "BEGIN\n"
        "SELECT SEQ_" table ".NEXTVAL INTO :NEW."
        col " FROM DUAL;\n"
        "END" (genExec db) "\n\n"))

(defn- create_sequence [db table]
  (str "CREATE SEQUENCE SEQ_" table
       " START WITH 1 INCREMENT BY 1"
          (genExec db) "\n\n"))

;; Oracle
(defmethod getStringKeyword Oracle [db] "VARCHAR2")
(defmethod getTSDefault Oracle [db] "DEFAULT SYSTIMESTAMP")
(defmethod getLongKeyword Oracle [db] "NUMBER(38)")
(defmethod getDoubleKeyword Oracle [db] "BINARY_DOUBLE")
(defmethod getFloatKeyword Oracle [db] "BINARY_FLOAT")

(defmethod genAutoInteger Oracle [db table fld]
  (do
    (.put ^Map *DDL_BVS* table (:column fld))
    (genInteger db fld)))

(defmethod genAutoLong Oracle [db table fld]
  (do
    (.put ^Map *DDL_BVS* table (:column fld))
    (genLong db fld)))

(defmethod genEndSQL Oracle [db]
  (let [ m (into {} *DDL_BVS*) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (doto bf
        (.append (create_sequence db (first en)))
        (.append (create_sequence_trigger db (first en) (last en)))))
    (.toString bf)))

(defmethod genDrop Oracle [db table]
  (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec db) "\n\n"))


;;(println (getDDL (make-MetaCache testschema) (Oracle.) ))

(def ^:private oracle-eof nil)

