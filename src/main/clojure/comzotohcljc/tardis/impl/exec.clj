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

  comzotohcljc.tardis.impl.exec )

(import '(org.apache.commons.io.filefilter DirectoryFileFilter))
(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File FileFilter))
(import '(java.net URL))
(import '(java.util Date))
(import '(com.zotoh.frwk.io IOUtils))
(import '(com.zotoh.frwk.core
  Startable Versioned Hierarchial Identifiable))
(import '(com.zotoh.frwk.server
  Component ComponentRegistry))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.impl.defaults])
(use '[comzotohcljc.jmx.core])
(use '[comzotohcljc.tardis.impl.sys :only [make-kernel make-podmeta make-deployer] ])
(use '[ comzotohcljc.util.core :only [load-javaprops test-nestr nice-fpath TryC conv-long make-mmap uid test-nonil] ])
(use '[ comzotohcljc.util.str :only [nsb strim hgl?] ])
(use '[ comzotohcljc.util.ini :only [parse-inifile] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(def ^:private START-TIME (.getTime (Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol ExecvisorAPI
  ""
  (homeDir [_] )
  (confDir [_] )
  (podsDir [_] )
  (playDir [_] )
  (logDir [_] )
  (tmpDir [_] )
  (dbDir [_] )
  (blocksDir [_] )
  (getStartTime [_] )
  (kill9 [_] )
  (getUpTimeInMillis [_] ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chkManifest

  [^comzotohcljc.tardis.core.sys.Element execv
   app
   ^File des
   mf]

  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx execv)
         ^ComponentRegistry root (.getf ctx K_COMPS)
         ^ComponentRegistry apps (.lookup root K_APPS)
         ps (load-javaprops mf)
         ver (.getProperty ps "Implementation-Version" "")
         vid (.getProperty ps "Implementation-Vendor-Id")
         cz (.getProperty ps "Main-Class" "") ]

    (test-nestr "POD-MainClass" cz)
    (test-nestr "POD-Version" ver)

    (info "checking manifest for app: " app ", version: " ver ", main-class: " cz)

    ;;ps.gets("Manifest-Version")
    ;;.gets("Implementation-Title")
    ;;.gets("Implementation-Vendor-URL")
    ;;.gets("Implementation-Vendor")

    (let [ ^comzotohcljc.tardis.core.sys.Element
           m (-> (make-podmeta app ver nil cz vid (-> des (.toURI) (.toURL)))
                 (synthesize-component { :ctx ctx })) ]
      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx m) K_EXECV execv)
      (.reg apps m)
      m)))

(defn- inspect-pod [execv ^File des]
  (let [ app (FilenameUtils/getBaseName (nice-fpath des))
         mf (File. des ^String MN_FILE) ]
    (info "About to inspect app: " app)
    (info "app-dir: " des)
    (TryC
        (precondDir (File. des ^String POD_INF))
        (precondDir (File. des ^String POD_CLASSES))
        (precondDir (File. des ^String POD_LIB))
        (precondDir (File. des ^String META_INF))
        (precondFile (File. des ^String CFG_APP_CF))
        (precondFile (File. des ^String CFG_ENV_CF))
        (precondDir (File. des ^String DN_CONF))
        (precondFile mf)
        (chkManifest execv app des mf) )))

;; check all apps to ensure they are kosher.
(defn- inspect-pods [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         ^FileFilter ff DirectoryFileFilter/DIRECTORY
         ^File pd (.getf ctx K_PLAYDIR) ]
    (doseq [ f (seq (.listFiles pd ff)) ]
      (inspect-pod co f)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- start-jmx [^comzotohcljc.tardis.core.sys.Element co cfg]
  (info "JMX config " cfg)
  (TryC
    (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
           port (conv-long (nsb (get cfg "port")) 7777)
           ^String host (nsb (get cfg "host"))
           ^comzotohcljc.jmx.core.JMXServer jmx (make-jmxServer host) ]
      (.setRegistryPort jmx port)
      (.start ^Startable jmx)
      (.reg jmx co "com.zotoh" "execvisor" ["root=gallifrey"])
      (.setf! ctx K_JMXSVR jmx)
      (info (str "JMXserver listening on: " host " "  port)) )) )

(defn- stop-jmx [^comzotohcljc.tardis.core.sys.Element co]
  (TryC
    (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
           ^Startable jmx (.getf ctx K_JMXSVR) ]
      (when-not (nil? jmx)
        (.stop jmx))
      (.setf! ctx K_JMXSVR nil)))
  (info "JMX connection terminated."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-execvisor "" [parObj]
  (let [ impl (make-mmap) ]
    (info "creating execvisor, parent = " parObj)
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Versioned
        (version [_] "1.0")

        Hierarchial
        (parent [_] parObj)

        Identifiable
        (id [_] K_EXECV )

        ExecvisorAPI

        (getStartTime [_] START-TIME)
        (getUpTimeInMillis [_]
          (- (System/currentTimeMillis) START-TIME))
        (homeDir [this] (maybeDir (getCtx this) K_BASEDIR))
        (confDir [this] (maybeDir (getCtx this) K_CFGDIR))
        (podsDir [this] (maybeDir (getCtx this) K_PODSDIR))
        (playDir [this] (maybeDir (getCtx this) K_PLAYDIR))
        (logDir [this] (maybeDir (getCtx this) K_LOGDIR))
        (tmpDir [this] (maybeDir (getCtx this) K_TMPDIR))
        (dbDir [this] (maybeDir (getCtx this) K_DBSDIR))
        (blocksDir [this] (maybeDir (getCtx this) K_BKSDIR))
        (kill9 [this] (.stop ^Startable parObj))

        Startable
        (start [this]
          (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                 ^ComponentRegistry root (.getf ctx K_COMPS)
                 ^Startable k (.lookup root K_KERNEL) ]
            (inspect-pods this)
            (.start k)))
        (stop [this]
          (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                 ^ComponentRegistry
                 root (.getf ctx K_COMPS)
                 ^Startable k (.lookup root K_KERNEL) ]
            (stop-jmx this)
            (.stop k)))  )

       { :typeid (keyword "czc.tardis.impl/Execvisor") } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :czc.tardis.impl/Execvisor
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         ^comzotohcljc.util.ini.IWin32Conf
         cf (.getf ctx K_PROPS)
         comps (.getSection cf K_COMPS)
         regs (.getSection cf K_REGS)
         jmx  (.getSection cf K_JMXMGM) ]

    (info "initializing component: Execvisor: " co)
    (test-nonil "conf file: components" comps)
    (test-nonil "conf file: registries" regs)
    (test-nonil "conf file: jmx mgmt" jmx)

    (System/setProperty "file.encoding" "utf-8")

    (let [ ^File home (.homeDir ^comzotohcljc.tardis.impl.exec.ExecvisorAPI co)
           sb (doto (File. home ^String DN_BOXX)
                  (.mkdir))
           bks (doto (File. home (str DN_CFG "/" DN_BLOCKS))
                  (.mkdir))
           tmp (doto (File. home ^String DN_TMP)
                  (.mkdir))
           pods (File. home ^String DN_PODS)
           db (File. home ^String DN_DBS)
           log (doto (File. home ^String DN_LOGS)
                  (.mkdir)) ]
      ;;(precondDir pods)
      (precondDir sb)
      (precondDir log)
      (precondDir tmp)
      ;;(precondDir db)
      (precondDir bks)
      (doto ^comzotohcljc.util.core.MuObj (.getCtx co)
          (.setf! K_PODSDIR pods)
          (.setf! K_PLAYDIR sb)
          (.setf! K_LOGDIR log)
          (.setf! K_DBSDIR db)
          (.setf! K_TMPDIR tmp)
          (.setf! K_BKSDIR bks)))
    (start-jmx co jmx)
    (let [ ^ComponentRegistry root (make-component-registry :SystemRegistry K_COMPS "1.0" co)
           bks (make-component-registry :BlocksRegistry K_BLOCKS "1.0" nil)
           apps (make-component-registry :AppsRegistry K_APPS "1.0" nil)
           deployer (make-deployer)
           knl (make-kernel) ]

      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx co) K_COMPS root)
      (.reg root deployer)
      (.reg root knl)
      (.reg root apps)
      (.reg root bks)
      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx co) K_EXECV co)
      (let [ options { :ctx (.getCtx co) } ]
        (synthesize-component root options)
        (synthesize-component bks options)
        (synthesize-component apps options)
        (synthesize-component deployer options)
        (synthesize-component knl options)) )

    ))

(defn- make-blockmeta "" [^URL url]
  (let [ impl (make-mmap) ]
    (.mm-s impl :id (keyword (uid)))
    (.mm-s impl K_META url)
    ;; url points to block-meta file
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Component
        (id [_] (.mm-g impl :id))
        (version [_] "1.0")

        Hierarchial
        (parent [_] nil)

        BlockMeta

        (enabled? [_] (true? (.mm-g impl :active)))
        (metaUrl [_] (.mm-g impl K_META)) )

      { :typeid (keyword "czc.tardis.impl/BlockMeta") } )))


(defmethod comp-initialize :czc.tardis.impl/BlockMeta
  [^comzotohcljc.tardis.impl.defaults.BlockMeta bk]
  (let [ ^URL url (.metaUrl bk)
         ^comzotohcljc.util.ini.IWin32Conf cfg (parse-inifile url)
         inf (.getSection cfg "info") ]
    (test-nonil "Invalid block-meta file, no info section." inf)
    (info "initializing BlockMeta: " url)
    (let [ cz (strim (.optString cfg "info" "block-type" ""))
           ^comzotohcljc.tardis.core.sys.Element co bk  ]
      (when (hgl? cz)
        (.setAttr! co :id (keyword cz))
        (.setAttr! co :active true) )
      (.setAttr! co :version (strim (.optString cfg "info" "version" "")))
      (.setAttr! co :name (strim (.optString cfg "info" "name" "")))
      co)))

(defmethod comp-initialize :czc.tardis.impl/BlocksRegistry
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         bDir (.getf ctx K_BKSDIR)
         fs (IOUtils/listFiles ^File bDir "meta" false) ]
    (doseq [ ^File f (seq fs) ]
      (let [ ^comzotohcljc.tardis.core.sys.Element
             b (-> (make-blockmeta (-> f (.toURI)(.toURL)))
                   (synthesize-component {}) ) ]
        (.reg ^ComponentRegistry co b)
        (info "added one block: " (.id ^Identifiable b)) ))))




















(def ^:private exec-eof nil)

