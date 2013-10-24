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

  comzotohcljc.tardis.impl.ext )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(org.apache.commons.lang3 StringUtils))

(import '(java.util Map Properties))
(import '(java.net URL))
(import '(java.io File))
(import '(com.zotoh.gallifrey.runtime AppMain))
(import '(com.zotoh.gallifrey.etc PluginFactory Plugin))
(import '(com.zotoh.frwk.dbio MetaCache Schema DBIOLocal DBAPI))

(import '(com.zotoh.frwk.core
  Versioned Hierarchial Startable Disposable
  Identifiable ))
(import '(com.zotoh.frwk.server
  ComponentRegistry Component ServiceError ))
(import '(com.zotoh.gallifrey.core
  Container ConfigError ))

(import '(com.zotoh.gallifrey.io IOEvent))
(import '(com.zotoh.frwk.util Schedulable CoreUtils))
(import '(com.zotoh.wflow.core Job))
(import '(com.zotoh.wflow Pipeline))


(use '[clojure.tools.logging :only [info warn error debug] ])
;;(use '[comzotohcljc.tardis.io.core :only (make-emitter)])
(use '[comzotohcljc.tardis.io.core :rename {enabled? io-enabled?} ])
(use '[comzotohcljc.tardis.io.loops])
(use '[comzotohcljc.tardis.io.mails])
(use '[comzotohcljc.tardis.io.files])
(use '[comzotohcljc.tardis.io.jms])
(use '[comzotohcljc.tardis.io.http])
(use '[comzotohcljc.tardis.io.netty])
(use '[comzotohcljc.tardis.io.socket])
(use '[comzotohcljc.tardis.mvc.handler])
(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.impl.defaults
       :rename {enabled? blockmeta-enabled?
                start kernel-start
                stop kernel-stop } ])
(use '[comzotohcljc.tardis.etc.misc])
(use '[comzotohcljc.tardis.core.sys])

(use '[comzotohcljc.util.core :only [MuObj make-mmap] ])
(use '[ comzotohcljc.util.scheduler :only [make-scheduler] ])
(use '[ comzotohcljc.util.process :only [coroutine] ])
(use '[ comzotohcljc.util.core :only [load-javaprops] ])
(use '[ comzotohcljc.util.seqnum :only [next-long] ])
(use '[ comzotohcljc.util.str :only [hgl? nsb strim nichts?] ])
(use '[ comzotohcljc.util.meta :only [make-obj] ])
(use '[ comzotohcljc.crypto.codec :only [pwdify] ])
(use '[ comzotohcljc.dbio.connect :only [dbio-connect] ])
(use '[ comzotohcljc.dbio.core :only [make-jdbc make-MetaCache make-db-pool make-Schema] ])
(use '[ comzotohcljc.net.rts :only [load-routes] ])
(require '[clojure.data.json :as json])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(defprotocol CljAppMain
  ""
  (contextualize [_ ctr] )
  (configure [_ options] )
  (initialize [_] )
  (start [_] )
  (stop [_])
  (dispose [_] ))

(defn- make-job "" ^Job [_container evt]
  (let [ impl (make-mmap)
         jid (next-long) ]
    (with-meta
      (reify

        MuObj

        (setf! [_ k v] (.mm-s impl k v))
        (clear! [_] (.mm-c impl))
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k))
        (clrf! [_ k] (.mm-r impl k))

        Job

        (container [_] _container)
        (setv [this k v] (.setf! this k v))
        (unsetv [this k] (.clrf! this k))
        (getv [this k] (.getf this k))
        (setLastResult [this v] (.setf! this JS_LAST v))
        (getLastResult [this] (.getf this JS_LAST))
        (clrLastResult [this] (.clrf! this JS_LAST))
        (event [_] evt)
        (id [_] jid))

      { :typeid (keyword "czc.tardis.impl/Job") } )))

(defprotocol ^:private JobCreator
  ""
  (update [_ event options] ))

(defn- make-jobcreator ""
  ^comzotohcljc.tardis.impl.ext.JobCreator [parObj]
  (let [ impl (make-mmap) ]
    (info "about to synthesize a job-creator...")
    (with-meta
      (reify

        JobCreator

        (update [_  evt options]
          (let [ ^comzotohcljc.tardis.core.sys.Element
                 src (.emitter ^IOEvent evt)
                 c0 (.getAttr src :router)
                 c1 (:router options)
                 job (make-job parObj evt) ]
            (debug "event router = " c1)
            (debug "io router = " c0)
            (try
              (let [ p (Pipeline. job (if (hgl? c1) c1 c0))
                     q (if (nil? p) (make-OrphanFlow job) p) ]
                (.start ^Pipeline q))
              (catch Throwable e#
                (-> (make-FatalErrorFlow job) (.start)))))))

      { :typeid (keyword "czc.tardis.impl/JobCreator") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ContainerAPI

(defprotocol ^:private ContainerAPI
  ""
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc sid cfg] )
  (reifyServices [_] )
  (enabled? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-service-block [^Identifiable bk container nm cfg]
  (let [ eid (.id bk)
         ^comzotohcljc.tardis.core.sys.Element
         obj (if (= :czc.tardis.io/JettyIO eid)
               (makeServletEmitter container)
               (makeEmitter container eid nm))
         pkey (.getAppKey ^Container container)
         hid (:handler cfg)
         mm (meta obj) ]
    (info "about to synthesize an emitter: " eid)
    (info "emitter meta: " mm)
    (info "is emitter = " (isa?  (:typeid mm) :czc.tardis.io/Emitter))
    (info "config params = " cfg)
    (synthesize-component
      obj
      { :ctx container :props (assoc cfg :hhh.pkey pkey) })
    (.setAttr! obj :router hid)
    (info "emitter synthesized - OK. handler => " hid)
    obj))

(defn- getDBAPI? ^DBAPI [^String gid cfg ^String pkey mcache]
  (let [ ^Map c (.get (DBIOLocal/getCache))
         mkey (str "jdbc." (name gid))
         jdbc (make-jdbc mkey cfg
                            (pwdify (:passwd cfg) pkey)) ]
    (when-not (.containsKey c mkey)
      (let [ p (make-db-pool jdbc {} ) ]
        (.put c mkey p)))
    (dbio-connect jdbc mcache {})))

(defn- maybeGetDBAPI [^comzotohcljc.tardis.core.sys.Element co ^String gid]
  (let [ pkey (.getAppKey ^Container co)
         mcache (.getAttr co K_MCACHE)
         env (.getAttr co K_ENVCONF)
         cfg (:jdbc (:databases env))
         jj (cfg (keyword gid)) ]
    (if (nil? jj)
      nil
      (getDBAPI? gid jj pkey mcache))))

(defn- releaseSysResources [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^Schedulable sc (.getAttr co K_SCHEDULER)
         jc (.getAttr co K_JCTOR) ]
    (info "container releasing all system resources.")
    (when-not (nil? sc)
      (.dispose sc))))

(defn- make-app-container [pod]
  (let [ impl (make-mmap) ]
    (info "about to create an app-container...")
    (with-meta
      (reify

        Element

        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )
        (setCtx! [_ x] (.mm-s impl :ctx x) )
        (getCtx [_] (.mm-g impl :ctx) )

        Container

        (notifyObservers [this evt options]
          (let [ ^comzotohcljc.tardis.impl.ext.JobCreator
                 jc (.getAttr this K_JCTOR) ]
            (.update jc evt options)))
        (getAppKey [_] (.appKey ^comzotohcljc.tardis.impl.defaults.PODMeta pod))
        (getAppDir [this] (.getAttr this K_APPDIR))
        (acquireJdbc [this gid] (maybeGetDBAPI this gid))
        (core [this]
          (.getAttr this K_SCHEDULER))
        (hasService [_ serviceId]
          (let [ ^ComponentRegistry srg (.mm-g impl K_SVCS) ]
            (.has srg (keyword serviceId))))

        (getService [_ serviceId]
          (let [ ^ComponentRegistry srg (.mm-g impl K_SVCS) ]
            (.lookup srg (keyword serviceId))))

        Component

        (id [_] (.id ^Identifiable pod) )
        (version [_] "1.0")

        Hierarchial

        (parent [_] nil)

        Startable

        (start [_]
          (let [ ^comzotohcljc.tardis.core.sys.Registry srg (.mm-g impl K_SVCS)
                 main (.mm-g impl :main-app) ]
            (info "container starting all services...")
            (doseq [ [k v] (seq* srg) ]
              (info "service: " k " about to start...")
              (.start ^Startable v))
            (info "container starting main app...")
            (cond
              (satisfies? CljAppMain main)
              (.start ^comzotohcljc.tardis.impl.ext.CljAppMain main)
              (instance? AppMain main)
              (.start ^AppMain main)
              :else nil)))

        (stop [this]
          (let [ ^comzotohcljc.tardis.core.sys.Registry srg (.mm-g impl K_SVCS)
                 pls (.getAttr this K_PLUGINS)
                 main (.mm-g impl :main-app) ]
            (info "container stopping all services...")
            (doseq [ [k v] (seq* srg) ]
              (.stop ^Startable v))
            (info "container stopping all plugins...")
            (doseq [ p (seq pls) ]
              (.stop ^Startable p))
            (info "container stopping...")
            (cond
              (satisfies? CljAppMain main)
              (.stop ^comzotohcljc.tardis.impl.ext.CljAppMain main)
              (instance? AppMain main)
              (.stop ^AppMain main)
              :else nil) ))

        Disposable

        (dispose [this]
          (let [ ^comzotohcljc.tardis.core.sys.Registry srg (.mm-g impl K_SVCS)
                 pls (.getAttr this K_PLUGINS)
                 main (.mm-g impl :main-app) ]
            (doseq [ [k v] (seq* srg) ]
              (.dispose ^Disposable v))
            (doseq [ p (seq pls) ]
              (.dispose ^Disposable p))
            (info "container dispose() - main app getting disposed.")
            (cond
              (satisfies? CljAppMain main)
              (.dispose ^comzotohcljc.tardis.impl.ext.CljAppMain main)
              (instance? AppMain main)
              (.dispose ^AppMain main)
              :else nil)
            (releaseSysResources this) ))

        ContainerAPI

        (enabled? [_]
          (let [ env (.mm-g impl K_ENVCONF)
                 c (:container env) ]
            (if (false? (:enabled c))
              false
              true)))

        (reifyServices [this]
          (let [ env (.mm-g impl K_ENVCONF)
                 s (:services env) ]
            (if-not (empty? s)
                (doseq [ [k v] (seq s) ]
                  (reifyOneService this k v)))))

        (reifyOneService [this nm cfg]
          (let [ ^ComponentRegistry srg (.mm-g impl K_SVCS)
                 svc (nsb (:service cfg))
                 b (:enabled cfg) ]
            (if-not (or (false? b) (nichts? svc))
              (let [ s (reifyService this svc nm cfg) ]
                (.reg srg s)))))

        (reifyService [this svc nm cfg]
          (let [^comzotohcljc.util.core.MuObj ctx (.getCtx this)
                 ^ComponentRegistry root (.getf ctx K_COMPS)
                 ^ComponentRegistry bks (.lookup root K_BLOCKS)
                 ^ComponentRegistry bk (.lookup bks (keyword svc)) ]
            (when (nil? bk)
              (throw (ServiceError. (str "No such Service: " svc "."))))
            (make-service-block bk this nm cfg))) )

    { :typeid (keyword "czc.tardis.ext/Container") } )) )

(defn make-container [^comzotohcljc.tardis.core.sys.Element pod]
  (let [ c (make-app-container pod)
         ^comzotohcljc.util.core.MuObj ctx (.getCtx pod)
         cl (.getf ctx K_APP_CZLR)
         ^ComponentRegistry root (.getf ctx K_COMPS)
         apps (.lookup root K_APPS)
         ^URL url (.srcUrl ^comzotohcljc.tardis.impl.defaults.PODMeta pod)
         ps { K_APPDIR (File. (.toURI  url)) K_APP_CZLR cl } ]
    (comp-compose c apps)
    (comp-contextualize c ctx)
    (comp-configure c ps)
    (if (.enabled? ^comzotohcljc.tardis.impl.ext.ContainerAPI c)
      (do (coroutine (fn []
                          (do
                            (comp-initialize c)
                            (.start ^Startable c))) cl) c)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-configure :czc.tardis.ext/Container
  [^comzotohcljc.tardis.core.sys.Element co props]
  (let [ ^File appDir (K_APPDIR props)
         cfgDir (File. appDir ^String DN_CONF)
         srg (make-component-registry :EventSources K_SVCS "1.0" co)
         mf (load-javaprops (File. appDir ^String MN_FILE))
         envConf (json/read-str (FileUtils/readFileToString
                                (File. cfgDir "env.conf"))
                              :key-fn keyword)
         appConf (json/read-str (FileUtils/readFileToString
                                (File. cfgDir "app.conf"))
                              :key-fn keyword) ]
    ;;WebPage.setup(new File(appDir))
    ;;maybeLoadRoutes(cfgDir)
    ;;_ftlCfg = new FTLCfg()
    ;;_ftlCfg.setDirectoryForTemplateLoading( new File(_appDir, DN_PAGES+"/"+DN_TEMPLATES))
    ;;_ftlCfg.setObjectWrapper(new DefaultObjectWrapper())
    (synthesize-component srg {} )
    (doto co
      (.setAttr! K_APPDIR appDir)
      (.setAttr! K_SVCS srg)
      (.setAttr! K_ENVCONF_FP (File. cfgDir "env.conf"))
      (.setAttr! K_APPCONF_FP (File. cfgDir "app.conf"))
      (.setAttr! K_ENVCONF envConf)
      (.setAttr! K_APPCONF appConf)
      (.setAttr! K_MFPROPS mf))
    (info "container: configured app: " (.id ^Identifiable co))))


(defn- doCljApp [ctr opts ^comzotohcljc.tardis.impl.ext.CljAppMain obj]
  (.contextualize obj ctr)
  (.configure obj opts)
  (.initialize obj))

(defn- doJavaApp [^comzotohcljc.tardis.core.sys.Element ctr ^AppMain obj]
  (let [ ^File cfg (.getAttr ctr K_APPCONF_FP)
         json (CoreUtils/readJson cfg) ]
  (.contextualize obj ctr)
  (.configure obj json)
  (.initialize obj)) )

(defn- fmtPluginFname ^File [^String v ^File appDir]
  (let [ fnn (StringUtils/replace v "." "")
         m (File. appDir (str "modules/" fnn)) ]
    m))

(defn- plugin-inited? [^String v ^File appDir]
  (let [ pfile (fmtPluginFname v appDir) ]
    (.exists pfile)))

(defn- post-init-plugin [^String v ^File appDir]
  (let [ pfile (fmtPluginFname v appDir) ]
    (FileUtils/writeStringToFile pfile "ok" "utf-8")
    (info "initialized plugin: " v)))

(defn- doOnePlugin ^Plugin [co ^String v ^File appDir env app]
  (let [ ^PluginFactory pf (make-obj v)
         ^Plugin p (if (instance? PluginFactory pf)
             (.createPlugin pf)
             nil) ]
    (when (instance? Plugin p)
      (info "calling plugin-factory: " v)
      (.contextualize p co)
      (.configure p { :env env :app app })
      (if (plugin-inited? v appDir)
        (info "plugin " v " already initialized.")
        (do
          (.initialize p)
          (post-init-plugin v appDir)))
      (.start p))
    p))

(defmethod comp-initialize :czc.tardis.ext/Container
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^File appDir (.getAttr co K_APPDIR)
         env (.getAttr co K_ENVCONF)
         app (.getAttr co K_APPCONF)
         ^String dmCZ (nsb (:data-model app))
         ^Properties mf (.getAttr co K_MFPROPS)
         mCZ (strim (.get mf "Main-Class"))
         reg (.getAttr co K_SVCS)
         jc (make-jobcreator co)
         ^comzotohcljc.util.scheduler.SchedulerAPI
         sc (make-scheduler co)
         cfg (:container env) ]

    ;; handle the plugins
    (.setAttr! co K_PLUGINS
      (persistent!  (reduce (fn [sum en]
                              (assoc! sum (keyword (first en))
                                          (doOnePlugin co (last en) appDir env app)) )
                            (transient {}) (seq (:plugins app))) ))

    (.setAttr! co K_SCHEDULER sc)
    (.setAttr! co K_JCTOR jc)

    ;; build the user data-models or create a default one.
    (info "application data-model schema-class: " dmCZ )
    (.setAttr! co K_MCACHE (make-MetaCache
                             (if (hgl? dmCZ)
                               (let [ sc (make-obj dmCZ) ]
                                 (when-not (instance? Schema sc)
                                   (throw (ConfigError. (str "Invalid Schema Class " dmCZ))))
                                 sc)
                               (make-Schema [])) ))

    (when (nichts? mCZ) (warn "no main-class defined."))
    ;;(test-nestr "Main-Class" mCZ)

    (when (hgl? mCZ)
      (let [ obj (make-obj mCZ) ]
        (cond
          (satisfies? CljAppMain obj)
          (doCljApp co app obj)
          (instance? AppMain obj)
          (doJavaApp co obj)
          :else (throw (ConfigError. (str "Invalid Main Class " mCZ))))
        (.setAttr! co :main-app obj)
        (info "application main-class " mCZ " created and invoked")))

    (let [ sf (File. appDir (str DN_CONF "/static-routes.conf"))
           rf (File. appDir (str DN_CONF "/routes.conf")) ]
      (.setAttr! co :routes
        (vec (concat (if (.exists sf) (load-routes sf) [] )
                     (if (.exists rf) (load-routes rf) [] ))) ))

    (let [ svcs (:services env) ]
      (if (empty? svcs)
          (warn "No system service defined in env.conf.")
          (.reifyServices ^comzotohcljc.tardis.impl.ext.ContainerAPI co)))

    ;; start the scheduler
    (.activate sc cfg)

    (info "Initialized app: " (.id ^Identifiable co))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(def ^:private ext-eof nil)

