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

(ns testcljc.tardis.auth)

(import '(com.zotoh.gallifrey.runtime AuthError UnknownUser))
(import '(java.io File))
(import '(com.zotoh.frwk.dbio
  Transactable SQLr MetaCache DBAPI))

(require '[comzotohcljc.crypto.codec :as CE])
(require '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.tardis.auth.core])
(use '[comzotohcljc.tardis.auth.dms])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.connect])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.h2])
(use '[clojure.test])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def METAC (atom (make-MetaCache (comzotohcljc.tardis.auth.dms.AuthPluginSchema.))))
(def JDBC (atom nil))
(def DB (atom nil))
(def ROLES (atom nil))

(defn init-test [f]
  (let [ dir (File. (System/getProperty "java.io.tmpdir"))
         db (str "" (System/currentTimeMillis))
         url (make-h2-db dir db "sa" (CE/pwdify ""))
        jdbc (make-jdbc (CU/uid)
               { :d H2-DRIVER :url url :user "sa" :passwd "" }
               (CE/pwdify "")) ]
    (reset! JDBC jdbc)
    (apply-authPlugin-ddl jdbc)
    (reset! DB (dbio-connect jdbc @METAC {})))
  (if (nil? f) nil (f))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- create-roles []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB) ]
      (.execWith
        sql
        (fn [^SQLr tx]
          (create-authRole tx "Admin" "???")
          (create-authRole tx "User" "???")
          (create-authRole tx "Developer" "???")
          (create-authRole tx "Tester" "???")))
      (let [ rs (.execWith
                  sql
                  (fn [^SQLr tx]
                    (.findAll tx
                              :czc.tardis.auth/AuthRole
                              "order by role_name desc"))) ]
        (== (count rs) 4)))))

(defn- fetch-roles []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           rs (.execWith
                sql
                (fn [^SQLr tx] (.findAll tx :czc.tardis.auth/AuthRole ))) ]
      (reduce (fn [sum r]
                (assoc sum (:name r) r))
              {}
              (seq rs)))))

(defn- create-acct []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           ros (fetch-roles)
           u (.execWith
               sql
               (fn [^SQLr tx]
                 (create-loginAccount tx "joeb" (CE/pwdify "hi")
                                      [ (get ros "User") ] )))
           rc (.execWith
                sql
                (fn [^SQLr tx]
                  (dbio-get-m2m {:as :roles :with tx} u))) ]
      (== (count rc) 1))))

(defn- load-acct-nouser []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB) ]
      (try
        (.execWith
          sql
          (fn [^SQLr tx]
            (get-loginAccount tx "xxxxx" (CE/pwdify "7soiwqhfasfhals"))))
        false
        (catch UnknownUser e#
          true)))))

(defn- load-acct-badpwd [user]
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB) ]
      (try
        (.execWith
          sql
          (fn [^SQLr tx]
            (get-loginAccount tx user (CE/pwdify "7soiwqhfasfhals"))))
        false
        (catch AuthError e#
          true)))))

(defn- load-acct-ok [user pwd]
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           u (.execWith
               sql
               (fn [^SQLr tx]
                 (get-loginAccount tx user (CE/pwdify pwd)))) ]
      (not (nil? u)))))

(deftest testdbio-dbstuff

  (is (do (init-test nil) true))

  (is (create-roles))
  (is (create-acct))
  (is (load-acct-ok "joeb" "hi"))
  (is (load-acct-nouser))
  (is (load-acct-badpwd "joeb"))
)



;;(use-fixtures :each init-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private auth-eof nil)

;;(clojure.test/run-tests 'testcljc.tardis.auth)


