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

  comzotohcljc.tardis.auth.dms )

(import '(com.zotoh.frwk.dbio DBIOError JDBCInfo Schema))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.h2])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.oracle])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel! "czc.tardis.auth" StdAddress
  (with-db-fields {
    :addr1 { :size 200 :null false }
    :addr2 { :size 64}
    :city { :null false}
    :state {:null false}
    :zip {:null false}
    :country {:null false}
                   })
  (with-db-indexes { :i1 #{ :city :state :country }
    :i2 #{ :zip :country }
    :state #{ :state }
    :zip #{ :zip } } ))

(defmodel! "czc.tardis.auth"  AuthRole
  (with-db-fields
    { :name { :column "role_name" :null false }
      :desc { :column "description" :null false }
     })
  (with-db-uniques
    { :u1 #{ :name }
     }) )

(defmodel!  "czc.tardis.auth" LoginAccount
  (with-db-fields
    { :acctid { :null false }
      :email { :null false }
      :salt { :size 128 }
      :passwd { :null false :domain :Password }
     })
  (with-db-assocs
    { :roles { :kind :M2M
               :joined :czc.tardis.auth/AccountRole }
      :addr { :kind :O2O
              :cascade true
              :rhs :czc.tardis.auth/StdAddress }
     })
  (with-db-uniques
    { :u2 #{ :acctid }
     }) )

(defjoined! "czc.tardis.auth" AccountRole
           :czc.tardis.auth/LoginAccount
           :czc.tardis.auth/AuthRole)

(deftype AuthPluginSchema []
  Schema
  (getModels [_] [ StdAddress AuthRole LoginAccount AccountRole] ))

(def AUTH-MCACHE (make-MetaCache (AuthPluginSchema.)))

(defn generateAuthPluginDDL ^String [dbtype]
  (getDDL AUTH-MCACHE
    (case dbtype
      (:postgres :postgresql) Postgresql
      :mysql MySQL
      :h2 H2
      (:sqlserver :mssql) SQLServer
      :oracle Oracle
      (throw (DBIOError. (str "Unsupported database type: " dbtype)))) ))

(defn applyAuthPluginDDL [^JDBCInfo jdbc]
  (let [ dbtype (match-jdbc-url (.getUrl jdbc)) ]
    (upload-ddl jdbc (generateAuthPluginDDL dbtype))) )

(defn exportAuthPluginDDL [dbtype ^File file]
  (FileUtils/writeStringToFile file (generateAuthPluginDDL dbtype) "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dms-eof nil)

