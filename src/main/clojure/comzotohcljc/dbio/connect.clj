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

  comzotohcljc.dbio.connect )

(use '[clojure.tools.logging :only [info warn error debug] ])
(import '(java.util Map HashMap))

(use '[comzotohcljc.util.core :only [Try!] ])
(use '[comzotohcljc.util.str :only [nsb] ])
(use '[comzotohcljc.dbio.core :only [make-db-pool] ])
(use '[comzotohcljc.dbio.composite])
(use '[comzotohcljc.dbio.simple])
(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.oracle])
(use '[comzotohcljc.dbio.h2])

(import '(com.zotoh.frwk.dbio
  DBAPI JDBCPool JDBCInfo
  DBIOLocal DBIOError OptLockError))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hashJdbc "" ^long [jdbc]
  (.hashCode
    (str (:driver jdbc) (:url jdbc)
         (:user jdbc) (nsb (:pwdObj jdbc)))))

(defn registerJdbcTL "" [^JDBCInfo jdbc options]
  (let [ tloc (DBIOLocal/getCache)
         ^Map c (.get tloc)
         hc (.getId jdbc) ]
    (when-not (.containsKey c hc)
      (debug "no db pool found in thread-local, creating one...")
      (let [ p (make-db-pool jdbc options) ]
        (.put c hc p)))
    (.get c hc)))

(defn- maybe-finz-pool "" [ hc]
  (let [ tloc (DBIOLocal/getCache) ;; a thread local
         ^Map c (.get tloc) ;; c == java hashmap
         p (.get c hc) ]
    (when-not (nil? p)
      (Try! (.shutdown ^JDBCPool p))
      (.remove c hc))))

(defn- maybe-get-pool ""

  ^JDBCPool
  [ hc jdbc options]

  (let [ tloc (DBIOLocal/getCache) ;; get the thread local
         ^Map c (.get tloc)
         rc (.get c hc) ]
    (if (nil? rc)
      (registerJdbcTL jdbc options)
      rc)))

(defn dbio-connect "Connect to a datasource."

  ^DBAPI
  [^JDBCInfo jdbc metaCache options]

  (let [ hc (.getId jdbc) ]
    ;;(debug (.getMetas metaCache))
    (reify DBAPI

      (getMetaCache [_] metaCache)

      (supportsOptimisticLock [_]
        (if (false? (:opt-lock options)) false true))

      (vendor [_]
        (let [ ^JDBCPool
               p (maybe-get-pool hc jdbc options) ]
          (if (nil? p)
            nil
            (.vendor p))))

      (finz [_] nil)

      (open [_]
        (let [ ^JDBCPool
               p (maybe-get-pool hc jdbc options) ]
          (if (nil? p)
            nil
            (.nextFree p))))

      (newCompositeSQLr [this]
        (compositeSQLr metaCache this))

      (newSimpleSQLr [this]
        (simpleSQLr metaCache this)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private connect-eof nil)

