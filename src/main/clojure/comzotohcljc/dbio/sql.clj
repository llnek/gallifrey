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

  comzotohcljc.dbio.sql)

(use '[clojure.tools.logging :only [info warn error debug] ])

(use '[comzotohcljc.util.core :only [flatten-nil notnil? now-jtstamp nnz] ])
(use '[comzotohcljc.util.meta :only [bytes-class chars-class] ])
(use '[comzotohcljc.util.str :only [add-delim! nsb strim] ])
(use '[comzotohcljc.util.io :only [read-chars read-bytes ] ])
(use '[comzotohcljc.util.dates :only [gmt-cal] ])
(use '[comzotohcljc.dbio.core])

(import '(java.util Calendar GregorianCalendar TimeZone))
(import '(com.zotoh.frwk.dbio MetaCache DBIOError OptLockError))
(import '(java.math BigDecimal BigInteger))
(import '(java.io Reader InputStream))
(import '(com.zotoh.frwk.dbio DBAPI))
(import '(com.zotoh.frwk.io XData))

(import '(java.sql
  ResultSet Types SQLException
  DatabaseMetaData ResultSetMetaData Date Timestamp Blob Clob
  Statement PreparedStatement Connection))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn table-name
  (^String [mdef] (:table mdef))
  (^String [mid cache] (table-name (get cache mid))))

(defn col-name
  (^String [fdef] (:column fdef))
  (^String [fid zm] (col-name (get (:fields (meta zm)) fid))))

(defn- fmtUpdateWhere ^String [lock zm]
  (str (ese (col-name :rowid zm)) "=?"
       (if lock
          (str " AND " (ese (col-name :verid zm)) "=?")
          "")))

(defn- lockError [^String opcode cnt ^String table rowID]
  (when (= cnt 0)
    (throw (OptLockError. opcode table rowID))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sql-filter-clause "" [zm filters]
  (let [ flds (:fields (meta zm))
         wc (reduce (fn [^StringBuilder sum en]
                      (let [ k (first en) fld (get flds k)
                             c (if (nil? fld) k (ese (:column fld))) ]
                      (add-delim! sum " AND "
                        (str c
                             (if (nil? (last en)) " IS NULL " " = ? ")))))
                    (StringBuilder.)
                    (seq filters)) ]
    [ (nsb wc) (flatten-nil (vals filters)) ] ))

(defn- readCol ^Object [sqlType pos ^ResultSet rset]
  (let [ obj (.getObject rset (int pos))
         ^InputStream inp (cond
                  (instance? Blob obj) (.getBinaryStream ^Blob obj)
                  (instance? InputStream obj) obj
                  :else nil)
         ^Reader rdr (cond
                  (instance? Clob obj) (.getCharacterStream ^Clob obj)
                  (instance? Reader obj) obj
                  :else nil) ]
    (cond
      (notnil? rdr) (with-open [r rdr] (read-chars r))
      (notnil? inp) (with-open [p inp] (read-bytes p))
      :else obj)))

(defn- readOneCol [sqlType pos ^ResultSet rset]
  (case sqlType
      Types/TIMESTAMP (.getTimestamp rset (int pos) (gmt-cal))
      Types/DATE (.getDate rset (int pos) (gmt-cal))
      (readCol sqlType pos rset)) )

;; row is a transient object.
(defn- model-injtor [cache zm row cn ct cv]
  (let [ cols (:columns (meta zm))
         fdef (get cols cn) ]
    (if (nil? fdef)
      row
      (assoc! row (:id fdef) cv))))

;; generic resultset, no model defined.
;; row is a transient object.
(defn- std-injtor [row ^String cn ct cv]
  (assoc! row (keyword (.toUpperCase cn)) cv))

(defn- row2obj [finj ^ResultSet rs ^ResultSetMetaData rsmeta]
  (with-local-vars [ row (transient {}) ]
    (doseq [ pos (range 1 (+ (.getColumnCount rsmeta) 1)) ]
      (let [ cn (.getColumnName rsmeta (int pos))
             ct (.getColumnType rsmeta (int pos))
             cv (readOneCol ct (int pos) rs) ]
        (var-set row (finj @row cn ct cv))))
    (persistent! @row) ))

(defn- insert? [^String sql]
  (.startsWith (.toLowerCase (strim sql)) "insert"))

(defn- setBindVar [^PreparedStatement ps pos p]
  (cond
    (instance? String p) (.setString ps pos p)
    (instance? Long p) (.setLong ps pos p)
    (instance? Integer p) (.setInt ps pos p)
    (instance? Short p) (.setShort ps pos p)

    (instance? BigDecimal p) (.setBigDecimal ps pos p)
    (instance? BigInteger p) (.setBigDecimal ps pos (BigDecimal. ^BigInteger p))

    (instance? InputStream p) (.setBinaryStream ps pos p)
    (instance? Reader p) (.setCharacterStream ps pos p)
    (instance? Blob p) (.setBlob ps ^long pos ^Blob p)
    (instance? Clob p) (.setClob ps ^long pos ^Clob p)

    (instance? (chars-class) p) (.setString ps pos (String. ^chars p))
    (instance? (bytes-class) p) (.setBytes ps pos ^bytes p)
    (instance? XData p) (.setBinaryStream ps pos (.stream ^XData p))

    (instance? Boolean p) (.setInt ps pos (if p 1 0))
    (instance? Double p) (.setDouble ps pos p)
    (instance? Float p) (.setFloat ps pos p)

    (instance? Timestamp p) (.setTimestamp ps pos p (gmt-cal))
    (instance? Date p) (.setDate ps pos p (gmt-cal))
    (instance? Calendar p) (.setTimestamp ps pos
                                          (Timestamp. (.getTimeInMillis ^Calendar p))
                                          (gmt-cal))

    :else (dbio-error (str "Unsupported param type: " (type p)))) )

(defn- mssql-tweak-sqlstr [^String sqlstr ^String token ^String cmd]
  (loop [ stop false sql sqlstr ]
    (if stop
      sql
      (let [ lcs (.toLowerCase sql) pos (.indexOf lcs (name token))
             rc (if (< pos 0)
                  []
                  [(.substring sql 0 pos) (.substring sql pos)]) ]
        (if (empty? rc)
          (recur true sql)
          (recur false (str (first rc) " WITH (" cmd ") " (last rc)) ))))))

(defn- jiggleSQL [^DBAPI db ^String sqlstr]
  (let [ sql (strim sqlstr)
         lcs (.toLowerCase sql)
         v (.vendor db)   ]
    (if (= :sqlserver (:id v))
      (cond
        (.startsWith lcs "select") (mssql-tweak-sqlstr sql :where "NOLOCK")
        (.startsWith lcs "delete") (mssql-tweak-sqlstr sql :where "ROWLOCK")
        (.startsWith lcs "update") (mssql-tweak-sqlstr sql :set "ROWLOCK")
        :else sql)
      sql)))

(defn- build-stmt

  ^PreparedStatement
  [db ^Connection conn ^String sqlstr params]
  (let [ sql sqlstr ;; (jiggleSQL db sqlstr)
         ps (if (insert? sql)
              (.prepareStatement conn sql Statement/RETURN_GENERATED_KEYS)
              (.prepareStatement conn sql)) ]
    (debug "building SQLStmt: {}" sql)
    (doseq [n (seq (range 0 (count params))) ]
      (setBindVar ps (inc n) (nth params n)))
    ps))

(defn- handleGKeys [^ResultSet rs cnt options]
  (let [ rc (cond
              (= cnt 1) (.getObject rs 1)
              :else (.getLong rs (nsb (:pkey options)))) ]
    { :1 rc }))

(defprotocol ^:private SQueryAPI
  (sql-select [_ sql pms row-provider-func post-func] [_ sql pms] )
  (sql-executeWithOutput [_  sql pms options] )
  (sql-execute [_  sql pms] ) )

(defn- make-sql

  ^comzotohcljc.dbio.sql.SQueryAPI
  [^MetaCache metaCache db ^Connection conn]

  (reify SQueryAPI

    (sql-executeWithOutput [this sql pms options]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (if (> (.executeUpdate stmt) 0)
          (with-open [ rs (.getGeneratedKeys stmt) ]
            (let [ cnt (if (nil? rs)
                          0
                          (-> (.getMetaData rs) (.getColumnCount))) ]
              (if (and (> cnt 0) (.next rs))
                (handleGKeys rs cnt options)
                {}
                ))))))

    (sql-select [this sql pms ]
      (sql-select this sql pms
                  (partial row2obj std-injtor) (fn [a] a)))

    (sql-select [this sql pms func postFunc]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (with-open [ rs (.executeQuery stmt) ]
          (let [ rsmeta (.getMetaData rs) ]
            (loop [ sum (transient []) ok (.next rs) ]
              (if (not ok)
                (persistent! sum)
                (recur (conj! sum (postFunc (func rs rsmeta))) (.next rs))))))))

    (sql-execute [this sql pms]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (.executeUpdate stmt)))  ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SQLProcAPI
  ""
  (doQuery [_ conn sql params model] [_ conn sql params] )
  (doExecuteWithOutput [_ conn sql params options] )
  (doExecute [_ conn sql params] )
  (doCount [_  conn model] )
  (doPurge [_  conn model] )
  (doDelete [_  conn pojo] )
  (doInsert [_  conn pojo] )
  (doUpdate [_  conn pojo] ) )

(defn- insert-fields [flds obj s1 s2]
  (with-local-vars [ ps (transient []) ]
    (doseq [ [k v] (seq obj) ]
      (let [ fdef (get flds k)
             ^String cn (:column fdef) ]
        (when (and (notnil? fdef)
                 (not (:auto fdef))
                 (not (:system fdef)))
          (add-delim! s1 "," (ese cn))
          (add-delim! s2 "," (if (nil? v) "NULL" "?"))
          (when-not (nil? v)
            (var-set ps (conj! @ps v))))))

    (persistent! @ps) ))

(defn- update-fields [flds obj ^StringBuilder sb1]
  (with-local-vars [ ps (transient []) ]
    (doseq [ [k v] (seq obj) ]
      (let [ fdef (get flds k)
             cn (col-name fdef) ]
        (when (and (notnil? fdef)
                   (:updatable fdef)
                   (not (:auto fdef)) (not (:system fdef)) )
          (doto sb1
            (add-delim! "," (ese cn))
            (.append (if (nil? v) "=NULL" "=?")))
          (when-not (nil? v)
            (var-set ps (conj! @ps v))))))
    (persistent! @ps)) )

(defn- postFmtModelRow [model obj]
  (let [ mm { :typeid model
               :verid (:verid obj)
               :rowid (:rowid obj)
               :last-modify (:last-modify obj)
              }
         rc (with-meta (-> obj
                         (dbio-clr-fld :rowid)
                         (dbio-clr-fld :verid)
                         (dbio-clr-fld :last-modify)) mm) ]
    rc))

(defn make-proc ""

  ^comzotohcljc.dbio.sql.SQLProcAPI

  [ ^MetaCache metaCache
    ^DBAPI db  ]

  (let [ metas (.getMetas metaCache) ]
    (reify SQLProcAPI

      (doQuery [_ conn sql pms model]
        (let [ zm (get metas model) ]
          (when (nil? zm)
            (dbio-error (str "Unknown model " model)))
          (let [ px (partial model-injtor metaCache zm)
                 pf (partial row2obj px)
                 f2 (fn [obj] (postFmtModelRow model obj)) ]
            (-> (make-sql metaCache db conn) (.sql-select sql pms pf f2)))))

      (doQuery [_ conn sql pms]
        (let []
          (-> (make-sql metaCache db conn) (.sql-select sql pms ))) )

      (doCount [this conn model]
        (let [ rc (doQuery this conn
                    (str "SELECT COUNT(*) FROM "
                         (ese (table-name model metas))) [] ) ]
          (if (empty? rc)
            0
            (last (first (seq (first rc)))))))

      (doPurge [_ conn model]
        (let [ sql (str "DELETE FROM " (ese (table-name model metas))) ]
          (do (-> (make-sql metaCache db conn) (.sql-execute sql [])) nil)))

      (doDelete [this conn obj]
        (let [ info (meta obj) model (:typeid info)
               zm (get metas model) ]
          (when (nil? zm) (dbio-error (str "Unknown model " model)))
          (let [ lock (.supportsOptimisticLock db)
                 table (table-name zm)
                 rowid (:rowid info)
                 verid (:verid info)
                 p (if lock [rowid verid] [rowid] )
                 w (fmtUpdateWhere lock zm)
                 cnt (doExecute this conn
                       (str "DELETE FROM " (ese table) " WHERE " w)
                       p) ]
            (when lock (lockError "delete" cnt table rowid))
            cnt)))

      (doInsert [this conn obj]
        (let [ info (meta obj) model (:typeid info)
               zm (get metas model) ]
          (when (nil? zm) (dbio-error (str "Unknown model " model)))
          (let [ lock (.supportsOptimisticLock db)
                 flds (:fields (meta zm))
                 table (table-name zm)
                 now (now-jtstamp)
                 s2 (StringBuilder.)
                 s1 (StringBuilder.)
                 pms (insert-fields flds obj s1 s2) ]
            (if (== (.length s1) 0)
              nil
              (let [ out (doExecuteWithOutput this conn
                            (str "INSERT INTO " (ese table) "(" s1 ") VALUES (" s2 ")" )
                            pms { :pkey (col-name :rowid zm) } ) ]
                (if (empty? out)
                  (dbio-error (str "Insert requires row-id to be returned."))
                  (debug "exec-with-out " out))
                (let [ wm { :rowid (:1 out) :verid 0 } ]
                  (when-not (number? (:rowid wm))
                    (dbio-error (str "RowID data-type must be Long.")))
                  (vary-meta obj merge-meta wm))))
          )))

      (doUpdate [this conn obj]
        (let [ info (meta obj) model (:typeid info)
               zm (get metas model) ]
          (when (nil? zm) (dbio-error (str "Unknown model " model)))
          (let [ lock (.supportsOptimisticLock db)
                 cver (nnz (:verid info))
                 flds (:fields (meta zm))
                 table (table-name zm)
                 rowid (:rowid info)
                 sb1 (StringBuilder.)
                 now (now-jtstamp)
                 nver (inc cver)
                 pms (update-fields flds obj sb1) ]
            (if (= (.length sb1) 0)
              nil
              (with-local-vars [ ps (transient pms) ]
                (-> (add-delim! sb1 "," (ese (col-name :last-modify zm)))
                    (.append "=?"))
                (var-set  ps (conj! @ps now))
                (when lock ;; up the version
                  (-> (add-delim! sb1 "," (ese (col-name :verid zm)))
                      (.append "=?"))
                  (var-set ps (conj! @ps nver)))
                ;; for the where clause
                (var-set ps (conj! @ps rowid))
                (when lock (var-set  ps (conj! @ps cver)))
                (let [ cnt (doExecute this conn
                              (str "UPDATE " (ese table) " SET " sb1 " WHERE "
                                  (fmtUpdateWhere lock zm))
                                      (persistent! @ps)) ]
                  (when lock (lockError "update" cnt table rowid))
                  (vary-meta obj merge-meta
                             { :verid nver :last-modify now }))))
        )))

        (doExecuteWithOutput [this conn sql pms options]
          (-> (make-sql metaCache db conn)
              (.sql-executeWithOutput sql pms options)))

        (doExecute [this conn sql pms]
          (-> (make-sql metaCache db conn) (.sql-execute sql pms)))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sql-eof nil)

