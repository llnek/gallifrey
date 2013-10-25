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

  comzotohcljc.tardis.auth.core )

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(com.zotoh.gallifrey.runtime AuthError UnknownUser))
(import '(com.zotoh.gallifrey.etc
  PluginFactory Plugin PluginError))

(import '(com.zotoh.frwk.net ULFormItems ULFileItem))
(import '(org.apache.commons.codec.binary Base64))
(import '(com.zotoh.gallifrey.core Container))

(import '(com.zotoh.frwk.dbio
  DBAPI MetaCache SQLr JDBCPool JDBCInfo))

(import '(org.apache.commons.io FileUtils))
(import '(java.io File IOException))
(import '(java.util Properties))

(import '(org.apache.shiro.config IniSecurityManagerFactory))
(import '(org.apache.shiro SecurityUtils))
(import '(org.apache.shiro.subject Subject))

(import '( com.zotoh.wflow
  If BoolExpr
  FlowPoint Activity Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.gallifrey.io HTTPEvent HTTPResult))
(import '(com.zotoh.wflow.core Job))


(use '[comzotohcljc.util.core :only [notnil? stringify
                                     make-mmap uid
                                     test-nonil load-javaprops] ])
(use '[comzotohcljc.crypto.codec :only [pwdify] ])
(use '[comzotohcljc.util.str :only [nsb hgl? strim] ])
(use '[comzotohcljc.net.comms :only [getFormFields] ])

(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.core.wfs])
(use '[comzotohcljc.tardis.io.ios :only [getSignupInfo getLoginInfo realign!] ])
(use '[comzotohcljc.tardis.auth.dms])
(use '[comzotohcljc.dbio.connect :only [dbio-connect] ])
(use '[comzotohcljc.dbio.core])
(require '[clojure.data.json :as json])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AuthPlugin
  ""
  (getRoles [_ acctObj ] )
  (addAccount [_ options] )
  (getAccount [_ options]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkjdbc ^JDBCInfo [^comzotohcljc.util.core.MutableMapAPI impl]
  (let [ pkey (.mm-g impl :appKey)
         cfg (get (.mm-g impl :cfg) (keyword "_")) ]
    (make-jdbc "_" cfg (pwdify (:passwd cfg) pkey))))

(defn- getSQLr ^SQLr [^comzotohcljc.util.core.MutableMapAPI impl]
  (-> (dbio-connect (mkjdbc impl) AUTH-MCACHE {})
      (.newSimpleSQLr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-authRole "" [^SQLr sql ^String role ^String desc]
  (.insert sql (-> (dbio-create-obj :czc.tardis.auth/AuthRole)
                 (dbio-set-fld :name role)
                 (dbio-set-fld :desc desc)) ))

(defn remove-authRole "" [^SQLr sql role]
  (.execute sql
            (str "delete from "
                 (ese (:table AuthRole))
                 " where "
                 (ese (:column (:name (:fields (meta AuthRole)))))
                 " = ?")
            [(nsb role)]))

(defn list-authRoles ""
  [^SQLr sql]
  (.findAll sql :czc.tardis.auth/AuthRole))

(defn create-loginAccount  "" [^SQLr sql
                               ^String user
                               ^comzotohcljc.crypto.codec.Password pwdObj
                               options
                               roleObjs]
  (let [ [p s] (.hashed pwdObj)
         acc (.insert sql (-> (dbio-create-obj :czc.tardis.auth/LoginAccount)
                            (dbio-set-fld :email (strim (:email options)))
                            (dbio-set-fld :acctid (strim user))
                            ;;(dbio-set-fld :salt s)
                            (dbio-set-fld :passwd  p))) ]
    (doseq [ r (seq roleObjs) ]
      (dbio-set-m2m { :as :roles :with sql } acc r))
    acc))

(defn get-loginAccount  "" [^SQLr sql
                            ^String user
                            ^comzotohcljc.crypto.codec.Password pwdObj]
  (let [ acct (.findOne sql :czc.tardis.auth/LoginAccount
                        { :acctid (strim user) } ) ]
    (cond
      (nil? acct)
      (throw (UnknownUser. user))

      (.validateHash pwdObj (:passwd acct))
      acct

      :else
      (throw (AuthError. "Incorrect password"))) ))

(defn has-loginAccount  "" [^SQLr sql ^String user]
  (notnil? (.findOne sql :czc.tardis.auth/LoginAccount
                        { :acctid (strim user) } )))

(defn change-loginAccount "" [^SQLr sql
                              userObj
                              ^comzotohcljc.crypto.codec.Password pwdObj ]
  (let [ [p s] (.hashed pwdObj)
         u (-> userObj
              (dbio-set-fld :passwd p)
              (dbio-set-fld :salt s)) ]
    (.update sql u)))

(defn update-loginAccount "" [^SQLr sql userObj details]
  (if (empty? details)
    userObj
    (with-local-vars [ u userObj ]
      (doseq [ [f v] (seq details) ]
        (var-set u (dbio-set-fld @u f v)))
      (.update sql @u)) ))

(defn remove-loginAccountRole "" [^SQLr sql userObj roleObj]
  (dbio-clr-m2m {:as :roles :with sql } userObj roleObj))

(defn add-loginAccountRole "" [^SQLr sql userObj roleObj]
  (dbio-set-m2m {:as :roles :with sql } userObj roleObj))

(defn remove-loginAccount "" [^SQLr sql userObj]
  (.delete sql userObj))

(defn delete-loginAccount "" [^SQLr sql user]
  (.execute
    sql
    (str "delete from " (ese (:table LoginAccount))
         " where " (ese (:column (:acctid (:fields (meta LoginAccount)))))
         " =?")
    [ (strim user) ]))

(defn list-loginAccounts "" [^SQLr sql]
  (.findAll sql :czc.tardis.auth/LoginAccount))

(defn- init-shiro "" [^File appDir ^String appKey]
  (let [ ini (File. appDir "conf/shiro.ini")
         sm (-> (IniSecurityManagerFactory. (-> ini (.toURI)(.toURL)(.toString)))
              (.getInstance)) ]
    (SecurityUtils/setSecurityManager sm)
    (info "created shiro security manager: " sm)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work Flow
;;

(defn maybeSignupTest ^BoolExpr []
  (DefPredicate
    (evaluate [_ job]
      (let [^comzotohcljc.tardis.core.sys.Element ctr (.container ^Job job)
            ^comzotohcljc.tardis.auth.core.AuthPlugin
            pa (:auth (.getAttr ctr K_PLUGINS))
            ^HTTPEvent evt (.event ^Job job)
            info (getSignupInfo evt) ]
        (test-nonil "AuthPlugin" pa)
        (with-local-vars [ uid (:email info) ]
          (try
            (when (hgl? (:principal info))
              (var-set uid (:principal info)))
            (debug "about to add a user account - " @uid)
            (.setLastResult job { :account
              (.addAccount pa (merge info { :principal @uid } )) })
            (realign! evt (:account (.getLastResult job)) [])
            true
            (catch Throwable t#
              (error t# "")
              (.setLastResult job { :error t# } )
              false)))
        ))))

(defn maybeLoginTest ^BoolExpr []
  (DefPredicate
    (evaluate [_ job]
      (let [^comzotohcljc.tardis.core.sys.Element ctr (.container ^Job job)
            ^comzotohcljc.tardis.auth.core.AuthPlugin
            pa (:auth (.getAttr ctr K_PLUGINS))
            ^HTTPEvent evt (.event ^Job job)
            info (getLoginInfo evt) ]
        (test-nonil "AuthPlugin" pa)
        (debug "about to login user - " (:principal info))
        (try
          (let [ acct (.getAccount pa info)
                 rs (.getRoles pa acct) ]
            (realign! evt acct rs)
            true)
          (catch Throwable t#
            (error t# "")
            (.setLastResult job { :error t# })
            false))

        ))))

(defn- LOGIN-ERROR ^PTask []
  (DefWFTask
    (perform [_ fw job arg]
             )))

(defn- LOGIN-OK ^PTask []
  (DefWFTask (perform [_ fw job arg]
    (let [^HTTPEvent evt (.event ^Job job)
          ^comzotohcljc.tardis.io.ios.WebSession ss (.getSession evt) ]



             ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- makeAuthPlugin ""
  ^Plugin
  []
  (let [ impl (make-mmap) ]
    (reify
      Plugin
      (contextualize [_ ctr]
        (.mm-s impl :appDir (.getAppDir ^Container ctr))
        (.mm-s impl :appKey (.getAppKey ^Container ctr)))
      (configure [_ props]
        (let [ dbs (:databases (:env props)) ]
          (.mm-s impl :cfg (:jdbc dbs)) ))
      (initialize [_]
        (let []
          (applyAuthPluginDDL (mkjdbc impl))
          (init-shiro (.mm-g impl :appDir)
                      (.mm-g impl :appKey))))
      (start [_]
        (info "AuthPlugin started."))
      (stop [_]
        (info "AuthPlugin stopped."))
      (dispose [_]
        (info "AuthPlugin disposed."))

      AuthPlugin
      (addAccount [_ options]
        (let [ pkey (.mm-g impl :appKey)
               sql (getSQLr impl) ]
          (create-loginAccount sql
                               (:principal options)
                               (pwdify (:credential options) pkey)
                               options
                               [])))
      (getAccount [_ options]
        (let [ pkey (.mm-g impl :appKey)
               sql (getSQLr impl) ]
          (get-loginAccount sql
                            (:principal options)
                            (pwdify (:credential options) pkey))))
      (getRoles [_ acct] [])

      )))





(deftype AuthPluginFactory []
  PluginFactory
  (createPlugin [_]
    (require 'comzotohcljc.tardis.auth.core)
    (makeAuthPlugin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- doMain [& args]
  (let [ appDir (File. ^String (nth args 0))
         ^Properties mf (load-javaprops (File. appDir "META-INF/MANIFEST.MF"))
         pkey (.getProperty mf "Implementation-Vendor-Id")
         ^String cmd (nth args 1)
         ^String db (nth args 2)
         env (json/read-str
               (FileUtils/readFileToString (File. appDir "conf/env.conf") "utf-8")
               :key-fn keyword)
         cfg (get (:jdbc (:databases env)) (keyword db)) ]
    (when-not (nil? cfg)
      (let [ j (make-jdbc db cfg (pwdify (:passwd cfg) pkey))
             t (match-jdbc-url (nsb (:url cfg))) ]
        (cond
          (= "init-db" cmd)
          (let []
            (applyAuthPluginDDL j))

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (exportAuthPluginDDL t (File. ^String (nth args 3))))

          :else
          nil)) )))



;; home gen-sql alias outfile
;; home init-db alias
(defn -main "Main Entry" [& args]
  ;; for security, don't just eval stuff
  ;;(alter-var-root #'*read-eval* (constantly false))
  (if (< (count args) 3)
    nil
    (apply doMain args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private core-eof nil)


