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

  comzotohcljc.dbio.simple )

(use '[clojure.tools.logging :only [info warn error debug] ])
(import '(com.zotoh.frwk.dbio DBAPI MetaCache SQLr))
(import '(java.sql Connection))

(use '[comzotohcljc.util.str :only [hgl?] ])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.sql])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- openDB ^Connection [^DBAPI db]
  (doto (.open db)
    (.setAutoCommit true)
    ;;(.setTransactionIsolation Connection/TRANSACTION_READ_COMMITTED)
    (.setTransactionIsolation Connection/TRANSACTION_SERIALIZABLE)))

(defn simpleSQLr ""

  ^SQLr

  [ ^MetaCache metaCache
    ^DBAPI db ]

  (let [ ^comzotohcljc.dbio.sql.SQLProcAPI proc (make-proc metaCache db)
         metas (.getMetas metaCache) ]
    (reify SQLr

      (findAll [this model extra] (.findSome this model {} extra))
      (findAll [this model] (.findAll this model ""))

      (findOne [this model filters]
        (let [ rset (.findSome this model filters "") ]
          (if (empty? rset) nil (first rset))))

      (findSome [this  model filters] (.findSome this model filters ""))

      (findSome [this model filters extraSQL]
        (with-open [ conn (openDB db) ]
          (let [ zm (get metas model)
                 tbl (table-name zm)
                 s (str "SELECT * FROM " (ese tbl))
                 [wc pms] (sql-filter-clause zm filters)
                 extra (if (hgl? extraSQL) extraSQL "") ]
            (if (hgl? wc)
              (.doQuery proc conn (str s " WHERE " wc " " extra) pms model)
              (.doQuery proc conn (str s " " extra) [] model))) ))

      (update [this obj]
        (with-open [ conn (openDB db) ]
          (.doUpdate proc conn obj) ))

      (delete [this obj]
        (with-open [ conn (openDB db) ]
            (.doDelete proc conn obj) ))

      (insert [this obj]
        (with-open [ conn (openDB db) ]
            (.doInsert proc conn obj) ))

      (select [this model sql params]
        (with-open [ conn (openDB db) ]
            (.doQuery proc conn sql params model) ))

      (select [this sql params]
        (with-open [ conn (openDB db) ]
            (.doQuery proc conn sql params) ))

      (executeWithOutput [this sql pms]
        (with-open [ conn (openDB db) ]
            (.doExecuteWithOutput proc conn sql pms { :pkey "DBIO_ROWID" } ) ))

      (execute [this sql pms]
        (with-open [ conn (openDB db) ]
            (doExecute proc conn sql pms) ))

      (countAll [this model]
        (with-open [ conn (openDB db) ]
            (.doCount proc conn model) ))

      (purge [this model]
        (with-open [ conn (openDB db) ]
            (.doPurge proc conn model) )) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private simple-eof nil)

