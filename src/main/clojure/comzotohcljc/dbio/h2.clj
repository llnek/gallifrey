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

  comzotohcljc.dbio.h2)

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.frwk.dbio DBIOError))
(import '(java.io File))
(import '(java.sql DriverManager Connection Statement))

(use '[comzotohcljc.util.core :only [test-nonil test-nestr] ])
(use '[comzotohcljc.util.str :only [nsb] ])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])


(def H2-SERVER-URL "jdbc:h2:tcp://host/path/db" )
(def H2-DRIVER "org.h2.Driver" )

(def H2-MEM-URL "jdbc:h2:mem:{{dbid}};DB_CLOSE_DELAY=-1" )
(def H2-FILE-URL "jdbc:h2:{{path}};MVCC=TRUE" )

(def H2_MVCC ";MVCC=TRUE" )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;; H2
(defmethod getDateKeyword H2 [db] "TIMESTAMP")
(defmethod getDoubleKeyword H2 [db] "DOUBLE")
(defmethod getBlobKeyword H2 [db] "BLOB")
(defmethod getFloatKeyword H2 [db] "FLOAT")

(defmethod genAutoInteger H2 [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))

(defmethod genAutoLong H2 [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))

(defmethod genBegin H2 [db table]
  (str "CREATE CACHED TABLE " table "\n(\n" ))

(defmethod genDrop H2 [db table]
  (str "DROP TABLE " table " IF EXISTS CASCADE" (genExec db) "\n\n"))


(defn make-h2-db [^File dbFileDir ^String dbid ^String user pwdObj]
  (test-nonil "file-dir" dbFileDir)
  (test-nestr "db-id" dbid)
  (test-nestr "user" user)
  (let [ url (File. dbFileDir dbid)
         u (.getCanonicalPath url)
         pwd (nsb pwdObj)
         dbUrl (StringUtils/replace H2-FILE-URL "{{path}}" u) ]
    (debug "Creating H2: " dbUrl)
    (.mkdir dbFileDir)
    (with-open [ c1 (DriverManager/getConnection dbUrl user pwd) ]
      (.setAutoCommit c1 true)
      (with-open [ s (.createStatement c1) ]
        ;;(.execute s (str "CREATE USER " user " PASSWORD \"" pwd "\" ADMIN"))
        (.execute s "SET DEFAULT_TABLE_TYPE CACHED"))
      (with-open [ s (.createStatement c1) ]
        (.execute s "SHUTDOWN"))
      )
    dbUrl))

(defn close-h2-db [^File dbFileDir ^String dbid ^String user pwdObj]
  (test-nonil "file-dir" dbFileDir)
  (test-nestr "db-id" dbid)
  (test-nestr "user" user)
  (let [ url (File. dbFileDir dbid)
         u (.getCanonicalPath url)
         pwd (nsb pwdObj)
         dbUrl (StringUtils/replace H2-FILE-URL "{{path}}" u) ]
    (debug "Closing H2: " dbUrl)
    (with-open [ c1 (DriverManager/getConnection dbUrl user pwd) ]
      (.setAutoCommit c1 true)
      (with-open [ s (.createStatement c1) ]
        (.execute s "SHUTDOWN")) )))

;;(println (getDDL (make-MetaCache testschema) (H2.) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private h2-eof nil)



