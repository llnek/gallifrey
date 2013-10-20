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

  comzotohcljc.dbio.core )

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[clojure.set])

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.frwk.dbio
  MetaCache Schema SQLr JDBCPool JDBCInfo))
(import '(java.sql
  SQLException DatabaseMetaData
  Connection Driver DriverManager))
(import '(java.util
  GregorianCalendar TimeZone Properties))
(import '(java.lang Math))
(import '(com.zotoh.frwk.dbio DBIOError))
(import '(com.jolbox.bonecp BoneCP BoneCPConfig))
(import '(org.apache.commons.lang3 StringUtils))

(use '[comzotohcljc.crypto.codec])
(use '[comzotohcljc.util.core :only [TryC Try! root-cause stripNSPath notnil? nnz nbf uid] ])
(use '[comzotohcljc.util.meta :only [for-name] ])
(use '[comzotohcljc.util.str :only [strim embeds? nsb has-nocase? hgl?] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *USE_DDL_SEP* true)
(def ^:dynamic *DDL_BVS* nil)
(def ^:dynamic *META-CACHE* nil)
(def ^:dynamic *JDBC-INFO* nil)
(def DDL_SEP "-- :")


(defn uc-ent ^String [ent] (.toUpperCase (name ent)))
(defn lc-ent ^String [ent] (.toLowerCase (name ent)))

(defn ese "Escape string entity for sql."
  (^String [ent] (uc-ent ent))
  (^String [ch ent] (str ch (uc-ent ent) ch))
  (^String [c1 ent c2] (str c1 (uc-ent ent) c2)))

(defn merge-meta [m1 m2] (merge m1 m2))
(defn eseOID [] (ese "dbio_rowid"))
(defn eseVID [] (ese "dbio_verid"))
(defn eseLHS [] (ese "lhs_rowid"))
(defn eseRHS [] (ese "rhs_rowid"))

(defn make-jdbc "Make a JDBCInfo record."
  ^JDBCInfo
  [^String id cfg ^comzotohcljc.crypto.codec.Password pwdObj]
  ;;(debug "JDBC id= " id ", cfg = " cfg)
  (reify JDBCInfo
    (getId [_] id)
    (getDriver [_] (:d cfg))
    (getUrl [_] (:url cfg))
    (getUser [_] (:user cfg))
    (getPwd [_] (nsb pwdObj)) ))

(def SQLServer :sqlserver)
(def Oracle :oracle)
(def MySQL :mysql)
(def H2 :h2)
(def Postgresql :postgresql)

(def DBTYPES {
    :sqlserver { :test-string "select count(*) from sysusers" }
    :postgresql { :test-string "select 1" }
    :mysql { :test-string "select version()" }
    :h2  { :test-string "select 1" }
    :oracle { :test-string "select 1 from DUAL" }
  })

(defn dbio-error [^String msg] (throw (DBIOError. msg)))
(defn dbio-scopeType [t]
  (keyword (str *ns* "/" t)))

(defn- maybeGetVendor [^String product]
  (let [ lp (.toLowerCase product) ]
    (cond
      (has-nocase? lp "microsoft") :sqlserver
      (has-nocase? lp "postgres") :postgresql
      (has-nocase? lp "oracle") :oracle
      (has-nocase? lp "mysql") :mysql
      (has-nocase? lp "h2") :h2
      :else (dbio-error (str "Unknown db product " product)))))

(defn- fmtfkey [p1 p2]
  (keyword (str "fk_" (name p1) "_" (name p2))) )

(defn match-dbtype "" [^String dbtype]
  (let [ kw (keyword (.toLowerCase dbtype)) ]
    (if (nil? (get DBTYPES kw)) nil kw)))

(defn match-jdbc-url "" [^String url]
  (let [ ss (seq (StringUtils/split url \:)) ]
    (if (> (count ss) 1)
      (match-dbtype (nth ss 1))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data modelling
;;
(def JOINED-MODEL-MONIKER :czc.dbio.core/dbio-joined-model)
(def BASEMODEL-MONIKER :czc.dbio.core/dbio-basemodel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dbio-model
  ([^String nm] (dbio-model *ns* nm))
  ([^String nsp ^String nm]
    {
      :id (keyword (str nsp "/" nm))
      :table (.toUpperCase nm)
      :parent nil
      :abstract false
      :system false
      :mxm false
      :indexes {}
      :uniques {}
      :fields {}
      :assocs {} }) )

(defmacro defmodel! [ nsp model-name & body]
  `(def ~model-name
    (-> (dbio-model ~nsp ~(name model-name))
                 ~@body)))

(defmacro defmodel "Define a data model." [model-name & body]
  `(def ~model-name
    (-> (dbio-model ~(name model-name))
                 ~@body)))

(defmacro defjoined! [nsp model-name lhs rhs]
  `(def ~model-name
      (-> (dbio-model ~nsp ~(name model-name))
                (with-db-parent-model JOINED-MODEL-MONIKER)
                (with-db-joined-model ~lhs ~rhs))))

(defmacro defjoined [model-name lhs rhs]
  `(def ~model-name
      (-> (dbio-model ~(name model-name))
                (with-db-parent-model JOINED-MODEL-MONIKER)
                (with-db-joined-model ~lhs ~rhs))))

(defn with-db-parent-model "" [pojo par]
  (assoc pojo :parent par))

(defn with-db-joined-model "" [pojo lhs rhs]
  (let [ a1 { :kind :MXM :rhs lhs :fkey :lhs_rowid }
         a2 { :kind :MXM :rhs rhs :fkey :rhs_rowid }
         am (:assocs pojo)
         m1 (assoc am :lhs a1)
         m2 (assoc m1 :rhs a2) ]
    (-> pojo
      (assoc :assocs m2)
      (assoc :mxm true)) ))

(defn with-db-table-name "" [pojo tablename]
  (assoc pojo :table tablename))

(defn with-db-indexes "" [pojo indices]
  (let [ m (:indexes pojo) ]
    (assoc pojo :indexes (merge m indices))))

(defn with-db-uniques "" [pojo uniqs]
  (let [ m (:uniques pojo) ]
    (assoc pojo :uniques (merge m uniqs))))

(defn- get-fld-template "" [fid]
  { :column (.toUpperCase (name fid))
               :size 255
               :id fid
               :domain :String
               :assoc-key false
               :pkey false
               :null true
               :auto false
               :dft nil
               :updatable true
               :system false
               :index "" } )

(defn with-db-field "" [pojo fid fdef]
  (let [ dft (get-fld-template fid)
         fd (assoc (merge dft fdef) :id (keyword fid))
         fm (:fields pojo)
         mm (assoc fm fid fd) ]
    (assoc pojo :fields mm)))

(defn with-db-fields "" [pojo flddefs]
  (with-local-vars [rcmap pojo]
    (doseq [ [k v] (seq flddefs) ]
      (var-set rcmap (with-db-field @rcmap k v)))
    @rcmap))

(defn with-db-assoc "" [pojo aid adef]
  (let [ dft { :kind nil :rhs nil :fkey "" :cascade false }
         pid (:id pojo)
         ad (merge dft adef)
         a2 (case (:kind ad)
              (:O2O :O2M) (assoc ad :fkey (fmtfkey pid aid))
              (:M2M :MXM) ad
              (throw (DBIOError. (str "Invalid assoc def " adef))))
         am (:assocs pojo)
         mm (assoc am aid a2) ]
    (assoc pojo :assocs mm)))

(defn with-db-assocs "" [pojo assocs]
  (with-local-vars [ rcmap pojo ]
    (doseq [ [k v] (seq assocs) ]
      (var-set rcmap (with-db-assoc @rcmap k v)))
    @rcmap))

(defn with-db-abstract "" [pojo] (assoc pojo :abstract true))

(defn- with-db-system "" [pojo] (assoc pojo :system true))

(defn- nested-merge "" [src des]
  (cond
    (and (map? src)(map? des)) (merge src des)
    (and (set? src)(set? des)) (union src des)
    :else des))

;; Defining the base model here.
(defmodel! "czc.dbio.core" dbio-basemodel
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :rowid {:column "DBIO_ROWID" :pkey true :domain :Long
            :auto true :system true :updatable false}
    :verid {:column "DBIO_VERSION" :domain :Long :system true
            :dft [ 0 ] }
    :last-modify {:column "DBIO_LASTCHANGED" :domain :Timestamp
               :system true :dft [""] }
    :created-on {:column "DBIO_CREATED_ON" :domain :Timestamp
                  :system true :dft [""] :updatable false}
    :created-by {:column "DBIO_CREATED_BY" :system true :domain :String } }))

(defmodel! "czc.dbio.core" dbio-joined-model
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :lhs-typeid {:column "LHS_TYPEID" }
    :lhs-oid {:column "LHS_ROWID" :domain :Long :null false}
    :rhs-typeid {:column "RHS_TYPEID" }
    :rhs-oid {:column "RHS_ROWID" :domain :Long :null false} }) )

(defn make-Schema "" ^Schema [theModels]
  (reify Schema
    (getModels [_] theModels)) )

(defn- resolve-assocs "" [ms]
  (let [ fdef { :domain :Long :assoc-key true } ]
    (with-local-vars [ rc (transient {}) xs (transient {}) ]
      ;; create placeholder maps for each model, to hold new fields from assocs.
      (doseq [ [k m] (seq ms) ]
        (var-set rc (assoc! @rc k {} )))
      ;; as we find new assoc fields, add them to the placeholder maps.
      (doseq [ [k m] (seq ms) ]
        (let [ socs (:assocs m) ]
          (doseq [ [x s] (seq socs) ]
            (case (:kind s)
              (:O2O :O2M)
              (let [ rhs (get @rc (:rhs s))
                     zm (get @rc k)
                     fid (:fkey s)
                     ft (merge (get-fld-template fid) fdef) ]
                (var-set rc (assoc! @rc (:rhs s) (assoc rhs fid ft))))
              nil))))
      ;; now walk through all the placeholder maps and merge those new
      ;; fields to the actual models.
      (let [ tm (persistent! @rc) ]
        (doseq [ [k v] (seq tm) ]
          (let [ zm (get ms k)
                 fs (:fields zm) ]
            (var-set xs (assoc! @xs k (assoc zm :fields (merge fs v))))))
        (persistent! @xs)))))

(defn- resolve-parent "" [ms m]
  (let [ par (:parent m) ]
    (cond
      (keyword? par) (if (nil? (get ms par))
                       (dbio-error (str "Unknown model " par))
                       m)
      (nil? par)
      (assoc m :parent BASEMODEL-MONIKER)

      :else (dbio-error (str "Invalid parent " par)))))

;; idea is walk through all models and ultimately
;; link it's root to base-model.
(defn- resolve-parents "" [ms]
  (persistent! (reduce (fn [sum en]
                          (let [ rc (resolve-parent ms (last en)) ]
                            (assoc! sum (:id rc) rc)))
                       (transient {})
                       (seq ms))) )

(defn- mapize-models "" [ms]
  (persistent! (reduce (fn [sum n]
                         (assoc! sum (:id n) n))
                       (transient {})
                       (seq ms))) )

(defn- collect-db-xxx-filter "" [a b]
  (cond
    (keyword? b) :keyword
    (map? b) :map
    :else (dbio-error (str "Invalid arg " b))))

(defmulti collect-db-fields collect-db-xxx-filter)

(defmethod collect-db-fields :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-fields cache mm)))

(defmethod collect-db-fields :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:fields zm))
      (merge {} (:fields zm) (collect-db-fields cache par)))))

(defmulti collect-db-indexes collect-db-xxx-filter)

(defmethod collect-db-indexes :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-indexes cache mm)))

(defmethod collect-db-indexes :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:indexes zm))
      (merge {} (:indexes zm) (collect-db-indexes cache par)))))

(defmulti collect-db-uniques collect-db-xxx-filter)

(defmethod collect-db-uniques :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-uniques cache mm)))

(defmethod collect-db-uniques :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:uniques zm))
      (merge {} (:uniques zm) (collect-db-uniques cache par)))))

(defn- colmap-fields "" [flds]
  (with-local-vars [ sum (transient {}) ]
    (doseq [ [k v] (seq flds) ]
      (let [ cn (.toUpperCase (nsb (:column v))) ]
        (var-set sum (assoc! @sum cn v))))
    (persistent! @sum)) )

(defn- meta-models [cache]
  (with-local-vars [ sum (transient {}) ]
    (doseq [ [k m] (seq cache) ]
      (let [ flds (collect-db-fields cache m)
             cols (colmap-fields flds) ]
        (var-set sum
                 (assoc! @sum k
                         (with-meta m
                                    { :columns cols :fields flds } ) ))))
    (persistent! @sum)) )

(defn make-MetaCache ""

  ^MetaCache [^Schema schema]

  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m2 (if (empty? ms)
              {}
              (-> (assoc ms JOINED-MODEL-MONIKER dbio-joined-model)
                  (resolve-parents)
                  (resolve-assocs)
                  (assoc BASEMODEL-MONIKER dbio-basemodel)
                  (meta-models))) ]
    (reify MetaCache
      (getMetas [_] m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- safeGetConn

  ^Connection
  [^JDBCInfo jdbc]

  (let [ user (.getUser jdbc)
         url (.getUrl jdbc)
         dv (.getDriver jdbc)
         d (if (hgl? url) (DriverManager/getDriver url))
         p (if (hgl? user)
               (doto (Properties.) (.put "password" (nsb (.getPwd jdbc)))
                                   (.put "user" user)
                                   (.put "username" user))
               (Properties.)) ]
    (when (nil? d)
      (dbio-error (str "Can't load Jdbc Url: " url)))
    (when
      (and (hgl? dv)
           (not= (-> d (.getClass) (.getName)) dv))
        (warn "Expected " dv ", loaded with driver: " (.getClass d)))
    (.connect d url p)))

(defn make-connection ""

  ^Connection
  [^JDBCInfo jdbc]

  (let [ url (.getUrl jdbc)
         ^Connection conn (if (hgl? (.getUser jdbc))
                (safeGetConn jdbc)
                (DriverManager/getConnection url)) ]
    (when (nil? conn)
      (dbio-error (str "Failed to create db connection: " url)))
    (doto conn
      (.setTransactionIsolation  Connection/TRANSACTION_READ_COMMITTED))))

(defn test-connection "" [jdbc]
  (TryC (.close (make-connection jdbc))))

(defmulti resolve-vendor class)

(defmethod resolve-vendor JDBCInfo
  [jdbc]
  (with-open [ conn (make-connection jdbc) ]
    (resolve-vendor conn)))

(defmethod resolve-vendor Connection

  [^Connection conn]
  (let [ md (.getMetaData conn) ]
    (-> { :id (maybeGetVendor (.getDatabaseProductName md)) }
      (assoc :version (.getDatabaseProductVersion md))
      (assoc :name (.getDatabaseProductName md))
      (assoc :quote-string (.getIdentifierQuoteString md))
      (assoc :url (.getURL md))
      (assoc :user (.getUserName md))
      (assoc :lcis (.storesLowerCaseIdentifiers md))
      (assoc :ucis (.storesUpperCaseIdentifiers md))
      (assoc :mcis (.storesMixedCaseIdentifiers md)))))

(defmulti table-exist? (fn [a b] (class a)))

(defmethod table-exist? JDBCInfo
  [jdbc ^String table]
  (with-open [ conn (make-connection jdbc) ]
    (table-exist? conn table)))

(defmethod table-exist? Connection

  [^Connection conn ^String table]
  (with-local-vars [ rc false ]
    (Try!
      (let [ mt (.getMetaData conn)
             tbl (cond
                    (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                    (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                    :else table) ]
        (with-open [ res (.getColumns mt nil nil tbl nil) ]
          (when (and (not (nil? res)) (.next res))
            (var-set rc true)))))
    @rc))

(defmulti row-exist? (fn [a b] (class a)))

(defmethod row-exist? JDBCInfo
  [jdbc ^String table]
  (with-open [ conn (make-connection jdbc) ]
    (row-exist? conn table)))

(defmethod row-exist? Connection
  [^Connection conn ^String table]
  (with-local-vars [ rc false ]
    (Try!
      (let [ sql (str "SELECT COUNT(*) FROM  " (.toUpperCase table)) ]
        (with-open [ stmt (.createStatement conn) ]
          (with-open [ res (.executeQuery stmt sql) ]
            (when (and (notnil? res) (.next res))
              (var-set rc (> (.getInt res (int 1)) 0)))))) )
    @rc))

(defn- load-columns [^DatabaseMetaData mt ^String catalog ^String schema ^String table]
  (with-local-vars [ pkeys #{} cms {} ]
    (with-open [ rs (.getPrimaryKeys mt catalog schema table) ]
      (loop [ sum (transient #{}) more (.next rs) ]
        (if (not more)
          (var-set pkeys (persistent! sum))
          (recur
            (conj! sum (.toUpperCase (.getString rs (int 4))) )
            (.next rs)))))
    (with-open [ rs (.getColumns mt catalog schema table "%") ]
      (loop [ sum (transient {}) more (.next rs) ]
        (if (not more)
          (var-set cms (persistent! sum))
          (let [ opt (not= (.getInt rs (int 11)) DatabaseMetaData/columnNoNulls)
                 cn (.toUpperCase (.getString rs (int 4)))
                 ctype (.getInt rs (int 5)) ]
            (recur
              (assoc! sum (keyword cn)
                  { :column cn :sql-type ctype :null opt
                    :pkey (clojure.core/contains? @pkeys cn) })
              (.next rs))))))

    (with-meta @cms { :supportsGetGeneratedKeys (.supportsGetGeneratedKeys mt)
                      :supportsTransactions (.supportsTransactions mt) } )))

(defn load-table-meta "" [^Connection conn ^String table]
  (let [ mt (.getMetaData conn) dbv (resolve-vendor conn)
         catalog nil
         schema (if (= (:id dbv) :oracle) "%" nil)
         tbl (cond
                (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                :else table) ]
    ;; not good, try mixed case... arrrrrrrrrrhhhhhhhhhhhhhh
    ;;rs = m.getTables( catalog, schema, "%", null)
    (load-columns mt catalog schema tbl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- makePool ^JDBCPool [jdbc ^BoneCP impl]
  (let [ dbv (resolve-vendor jdbc) ]
    (reify

      Object

      (finalize [this]
        (Try! (.shutdown this)))

      JDBCPool

      (shutdown [_] (.shutdown impl))
      (vendor [_] dbv)
      (nextFree  [_]
        (try
            (.getConnection impl)
          (catch Throwable e#
            (dbio-error (str "No free connection."))))) )))

(defn make-db-pool ""

  ([^JDBCInfo jdbc] (make-db-pool jdbc {}))
  ([^JDBCInfo jdbc options]
    (let [ bcf (BoneCPConfig.)
           ^String dv (.getDriver jdbc) ]
      ;;(debug "Driver : " dv)
      ;;(debug "URL : "  (.getUrl jdbc))
      (when (hgl? dv) (for-name dv))
      (doto bcf
        (.setPartitionCount (Math/max 1 (nnz (:partitions options))))
        (.setLogStatementsEnabled (nbf (:debug options)))
        (.setPassword (nsb (.getPwd jdbc)))
        (.setJdbcUrl (.getUrl jdbc))
        (.setUsername (.getUser jdbc))
        (.setIdleMaxAgeInSeconds (* 60 60 24)) ;; 1 day
        (.setMaxConnectionsPerPartition (Math/max 2 (nnz (:max-conns options))))
        (.setMinConnectionsPerPartition (Math/max 1 (nnz (:min-conns options))))
        (.setPoolName (uid))
        (.setAcquireRetryDelayInMs 5000)
        (.setConnectionTimeoutInMs  (Math/max 5000 (nnz (:max-conn-wait options))))
        (.setDefaultAutoCommit false)
        (.setAcquireRetryAttempts 1))
      (makePool jdbc (BoneCP. bcf))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- splitLines [^String lines]
  (with-local-vars [ w (.length ^String DDL_SEP)
                     rc []
                     s2 lines ]
    (loop [ sum (transient [])
            ddl lines
            pos (.indexOf ddl ^String DDL_SEP) ]
      (if (< pos 0)
        (do (var-set rc (persistent! sum)) (var-set s2 (strim ddl)))
        (let [ nl (strim (.substring ddl 0 pos))
               d2 (.substring ddl (+ pos @w))
               p2 (.indexOf d2 ^String DDL_SEP) ]
          (recur (conj! sum nl) d2 p2))))
    (if (hgl? @s2)
      (conj @rc @s2)
      @rc)) )

(defn- maybeOK [^String dbn ^Throwable e]
  (let [ oracle (embeds? (nsb dbn) "oracle")
         ee (root-cause e)
         ec (if (instance? SQLException ee) (.getErrorCode ^SQLException ee) nil) ]
    (if (nil? ec)
      (throw e)
      (cond
        (and oracle (= 942 ec)(= 1418 ec)(= 2289 ec)(= 0 ec)) true
        :else (throw e)))))

(defmulti upload-ddl (fn [a b] (class a)))

(defmethod upload-ddl JDBCInfo
  [jdbc ^String ddl]
   (with-open [ conn (make-connection jdbc) ]
     (upload-ddl conn ddl)))

(defmethod upload-ddl Connection
  [^Connection conn ^String ddl]
    (debug "\n" ddl)
    (let [ dbn (.toLowerCase (-> (.getMetaData conn)(.getDatabaseProductName)))
           lines (splitLines ddl) ]
      (.setAutoCommit conn true)
      (doseq [ ^String line (seq lines) ]
        (let [ ln (StringUtils/strip (strim line) ";") ]
          (when (and (hgl? ln) (not= (.toLowerCase ln) "go"))
            (try
              (with-open [ stmt (.createStatement conn) ]
                (.executeUpdate stmt ln))
              (catch SQLException e#
                (maybeOK dbn e#))))
          ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dbio-create-obj [model]
  (with-meta
    {}
    { :typeid model } ))

(defn dbio-set-fld "" [pojo fld value]
  (assoc pojo (keyword fld) value))

(defn dbio-clr-fld "" [pojo fld]
  (dissoc pojo (keyword fld)))

(defn dbio-get-fld "" [pojo fld]
  (get pojo (keyword fld)))

(defn dbio-get-assoc [mc zm id]
  (if (nil? zm)
    nil
    (let [ m (:assocs zm)
           rc (get m id) ]
      (if (nil? rc)
        (dbio-get-assoc mc (get mc (:parent zm)) id)
        rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handling assocs

(defn- dbio-get-o2x [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         zm (get mc (:typeid (meta lhs)))
         ac (dbio-get-assoc mc zm (:as ctx))
         ^SQLr sql (:with ctx)
         rt (:cast ctx)
         fid (:fkey ac)
         fv (:rowid (meta lhs)) ]
    [sql (if (nil? rt) (:rhs ac) rt) { fid fv} ]))

(defn- dbio-set-o2x [ctx lhs rhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         zm (get mc (:typeid (meta lhs)))
         ac (dbio-get-assoc mc zm (:as ctx))
         pid (:typeid (meta rhs))
         fv (:rowid (meta lhs))
         fid (:fkey ac)
         ^SQLr sql (:with ctx)
         x (->
              (dbio-create-obj pid)
              (dbio-set-fld fid fv)
              (vary-meta merge-meta
                { :rowid (:rowid (meta rhs))
                  :verid (:verid (meta rhs)) } ))
         y (.update sql x) ]
    [lhs
         (merge y (dissoc rhs fid)) ] ))

;; one to many assocs
;;
(defn dbio-get-o2m [ctx lhs]
  (let [ [^SQLr sql rt pms] (dbio-get-o2x ctx lhs) ]
    (.findSome sql rt pms)))

(defn dbio-set-o2m [ctx lhs rhs]
  (dbio-set-o2x ctx lhs rhs))

(defn dbio-add-o2m [ctx lhs rhsObjs ]
  (with-local-vars [ rc (transient []) ]
    (doseq [ r (seq rhsObjs) ]
      (var-set rc
               (conj! @rc
                      (last (dbio-set-o2x ctx lhs r)))))
    [ lhs (persistent! @rc) ] ))

(defn dbio-clr-o2m [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         zm (get mc (:typeid (meta lhs)))
         ac (dbio-get-assoc mc zm (:as ctx))
         fv (:rowid (meta lhs))
         rt (:cast ctx)
         rp (if (nil? rt) (:rhs ac) rt)
         fid (:fkey ac)
         ^SQLr sql (:with ctx) ]
    (.execute
      sql
      (str "delete from "
           (ese (:table (get mc rp)))
           " where "
           (ese fid) " = ?")
      [ fv ])
    lhs))

;; one to one assocs
;;
(defn dbio-get-o2o [ctx lhs]
  (let [ [^SQLr sql rt pms] (dbio-get-o2x ctx lhs) ]
    (.findOne sql rt pms)))

(defn dbio-set-o2o [ctx lhs rhs]
  (dbio-set-o2x ctx lhs rhs))

(defn dbio-clr-o2o [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         zm (get mc (:typeid (meta lhs)))
         ac (dbio-get-assoc mc zm (:as ctx))
         fv (:rowid (meta lhs))
         rt (:cast ctx)
         fid (:fkey ac)
         ^SQLr sql (:with ctx)
         y (.findOne sql (if (nil? rt) (:rhs ac) rt) { fid fv } ) ]
    (when-not (nil? y)
      (let [ x (vary-meta (-> (dbio-create-obj (:typeid (meta y)))
                   (dbio-set-fld fid nil))
                          merge-meta (meta y)) ]
        (.update sql x)))
    lhs))


;; many to many assocs
;;

(defn dbio-set-m2m [ctx lhs rhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         lid (:typeid (meta lhs))
         rid (:typeid (meta rhs))
         lv (:rowid (meta lhs))
         rv (:rowid (meta rhs))
         zm (get mc lid)
         ac (dbio-get-assoc mc zm (:as ctx))
         mm (get mc (:joined ac))
         ml (:rhs (:lhs (:assocs mm)))
         rl (:rhs (:rhs (:assocs mm)))
         ^SQLr sql (:with ctx)
         x (dbio-create-obj (:id mm))
         y  (if (= ml lid)
              (-> x
                (dbio-set-fld :lhs-typeid (stripNSPath lid))
                (dbio-set-fld :lhs-oid lv)
                (dbio-set-fld :rhs-typeid (stripNSPath rid))
                (dbio-set-fld :rhs-oid rv))
              (-> x
                (dbio-set-fld :lhs-typeid (stripNSPath rid))
                (dbio-set-fld :lhs-oid rv)
                (dbio-set-fld :rhs-typeid (stripNSPath lid))
                (dbio-set-fld :rhs-oid lv))) ]
    (.insert sql y)))

(defn dbio-clr-m2m
  ([ctx lhs] (dbio-clr-m2m ctx lhs nil))
  ([ctx lhs rhs]
    (let [ mc (.getMetas ^MetaCache *META-CACHE*)
           lid (:typeid (meta lhs))
           lv (:rowid (meta lhs))
           rid (:typeid (meta rhs))
           rv (:rowid (meta rhs))
           zm (get mc lid)
           ac (dbio-get-assoc mc zm (:as ctx))
           mm (get mc (:joined ac))
           flds (:fields (meta mm))
           ml (:rhs (:lhs (:assocs mm)))
           rl (:rhs (:rhs (:assocs mm)))
           ^SQLr sql (:with ctx)
           [x y a b]  (if (= ml lid)
                        [ (:column (:lhs-oid flds)) (:column (:lhs-typeid flds))
                          (:column (:rhs-oid flds)) (:column (:rhs-typeid flds)) ]
                        [ (:column (:rhs-oid flds)) (:column (:rhs-typeid flds))
                          (:column (:lhs-oid flds)) (:column (:lhs-typeid flds)) ]) ]
      (if (nil? rhs)
        (.execute
          sql
          (str "delete from " (ese (:table mm))
           " where " (ese x) " =? and " (ese y) " =?")
          [ lv (stripNSPath lid) ] )
        (.execute
          sql
          (str "delete from " (ese (:table mm))
           " where " (ese x) " =? and " (ese y) " =? and "
           (ese a) " =? and " (ese b) " =?" )
          [ lv (stripNSPath lid)
            rv (stripNSPath rid) ])) )))

(defn dbio-clr-m2m [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         lid (:typeid (meta lhs))
         lv (:rowid (meta lhs))
         zm (get mc lid)
         ac (dbio-get-assoc mc zm (:as ctx))
         mm (get mc (:joined ac))
         flds (:fields (meta mm))
         ml (:rhs (:lhs (:assocs mm)))
         rl (:rhs (:rhs (:assocs mm)))
         ^SQLr sql (:with ctx)
         [x y]  (if (= ml lid)
                  [ (:column (:lhs-oid flds)) (:column (:lhs-typeid flds)) ]
                  [ (:column (:rhs-oid flds)) (:column (:rhs-typeid flds)) ]) ]
    (.execute
      sql
      (str "delete from "
           (ese (:table mm))
           " where "
           (ese x) " =? and "
           (ese y) " =?")
      [ lv (stripNSPath lid) ] ) ))

(defn dbio-get-m2m [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         lid (:typeid (meta lhs))
         lv (:rowid (meta lhs))
         zm (get mc lid)
         ac (dbio-get-assoc mc zm (:as ctx))
         mm (get mc (:joined ac))
         flds (:fields (meta mm))
         ml (:rhs (:lhs (:assocs mm)))
         rl (:rhs (:rhs (:assocs mm)))
         ^SQLr sql (:with ctx)
         eseRES (ese "RES")
         eseMM (ese "MM")
         [x y z k a t]  (if (= ml lid)
                        [:lhs-typeid :rhs-typeid
                         :lhs-oid :rhs-oid ml rl]
                        [:rhs-typeid :lhs-typeid
                         :rhs-oid :lhs-oid rl ml] ) ]
    (.select
      sql
      t
      (str "select distinct " eseRES ".* from "
           (ese (:table (get mc t)))
           " " eseRES " join " (ese (:table mm)) " " eseMM " on "
           (str eseMM "." (ese (:column (x flds))))
           "=? and "
           (str eseMM "." (ese (:column (y flds))))
           "=? and "
           (str eseMM "." (ese (:column (z flds))))
           "=? and "
           (str eseMM "." (ese (:column (k flds))))
           " = " (str eseRES "." (ese (:column (:rowid flds)))))
      [ (stripNSPath a) (stripNSPath t) lv ]
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private core-eof nil)

