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

  comzotohcljc.tardis.core.constants )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private SYS_DEVID_PFX "system.####")
(def ^:private SYS_DEVID_SFX "####")

(def SYS_DEVID_REGEX #"system::[0-9A-Za-z_\-\.]+" )
(def SHUTDOWN_DEVID #"system::kill_9" )

(def SHUTDOWN_URI "/kill9")
(def POD_PROTOCOL  "pod:" )
(def META_INF  "META-INF" )
(def POD_INF  "POD-INF" )
(def WEB_INF  "WEB-INF" )

(def DN_BLOCKS  "blocks" )
(def DN_BOOT "boot" )
(def DN_EXEC "exec" )
(def DN_CONF "conf" )
(def DN_CLASSES "classes" )
(def DN_LIB "lib" )
(def DN_CFG "etc" )
(def DN_BOXX "apps" )
(def DN_PODS  "pods" )
(def DN_LOGS "logs" )
(def DN_TMP "tmp" )
(def DN_DBS "dbs" )
(def DN_DIST "dist" )
(def DN_TEMPLATES  "templates" )
(def DN_VIEWS  "htmls" )
(def DN_PAGES  "pages" )
(def DN_PATCH "patch" )
(def DN_IMAGES "images" )
(def DN_SCRIPTS "scripts" )
(def DN_STYLES "styles" )
(def DN_PUBLIC "public" )

(def MN_FILE (str META_INF "/" "MANIFEST.MF"))
(def POD_CLASSES  (str POD_INF "/" DN_CLASSES))
(def POD_PATCH  (str POD_INF "/" DN_PATCH))
(def POD_LIB  (str POD_INF "/" DN_LIB))

(def WEB_CLASSES  (str WEB_INF  "/" DN_CLASSES))
(def WEB_LIB  (str WEB_INF  "/" DN_LIB))
(def WEB_LOG  (str WEB_INF  "/logs"))
(def WEB_XML  (str WEB_INF  "/web.xml"))

(def MN_RNOTES (str META_INF "/" "RELEASE-NOTES.txt"))
(def MN_README (str META_INF "/" "README.md"))
(def MN_NOTES (str META_INF "/" "NOTES.txt"))
(def MN_LIC (str META_INF "/" "LICENSE.txt"))

(def CFG_ENV_CF  (str DN_CONF  "/"  "env.conf" ))
(def CFG_APP_CF  (str DN_CONF  "/"  "app.conf" ))


(def K_GALLIFREY_APPDOMAIN :gallifrey-app-domain )
(def K_GALLIFREY_APPID :gallifrey-appid )
(def K_GALLIFREY_APPTASK :gallifrey-app-task )
(def K_JMXMGM :jmx-management )
(def K_HOMEDIR :gallifrey-home )
(def K_PROPS :gallifrey.conf )
(def K_ROUTE_INFO :route-info )
(def K_CLISH :cli-shell )
(def K_COMPS :components )
;;(def K_ENDORSED :endorsed )
(def K_REGS :registries )
(def K_KERNEL :kernel )
(def K_EXECV :execvisor )
(def K_DEPLOYER :deployer )
(def K_JCTOR :job-creator )
(def K_SCHEDULER :scheduler )
(def K_CONTAINERS :containers)
(def K_BLOCKS :blocks )
(def K_JMXSVR :jmxsvr )
(def K_MCACHE :meta-cache)
(def K_PLUGINS :plugins)
(def K_APPS :apps )
(def K_PODS :pods )
(def K_SVCS :services )
;;(def K_ROOT :root-rego )

(def K_ROOT_CZLR :root-loader )
(def K_APP_CZLR :app-loader )
(def K_EXEC_CZLR :exec-loader )

(def K_BASEDIR :base-dir )
(def K_PODSDIR :pods-dir )
(def K_CFGDIR :cfg-dir )
(def K_APPDIR :app-dir )
(def K_PLAYDIR :play-dir )
(def K_LOGDIR :log-dir )
(def K_TMPDIR :tmp-dir )
(def K_DBSDIR :dbs-dir )
(def K_BKSDIR :blocks-dir )

(def K_COUNTRY :country )
(def K_LOCALE :locale )
(def K_L10N :l10n )
(def K_LANG :lang )
(def K_RCBUNDLE :str-bundle )

(def K_PIDFILE :pid-file )
(def K_APPCONF_FP :app-conf-file)
(def K_APPCONF :app-conf)
(def K_ENVCONF_FP :env-conf-file)
(def K_ENVCONF :env-conf)
(def K_MFPROPS :mf-props)

(def K_META :meta )

(def EV_OPTS "____eventoptions")
(def JS_LAST "____lastresult")
(def JS_CRED "credential")
(def JS_USER "principal")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private constants-eof nil)



