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

  comzotohcljc.tardis.core.climain )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.jboss.netty.channel
  Channel ChannelFuture ChannelFutureListener))

(import '(com.zotoh.gallifrey.core ConfigError))
(import '(com.zotoh.frwk.server Component ComponentRegistry))
(import '(com.zotoh.frwk.core
  Versioned Identifiable Hierarchial
  Startable ))
(import '(com.zotoh.gallifrey.loaders
  AppClassLoader RootClassLoader ExecClassLoader))
(import '(com.zotoh.gallifrey.etc CmdHelpError))
(import '(java.util Locale))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

(use '[comzotohcljc.i18n.resources :only [get-resource] ])
(use '[comzotohcljc.util.process :only [pid safe-wait] ])
(use '[comzotohcljc.util.meta :only [set-cldr get-cldr] ])
(use '[comzotohcljc.util.core :only [test-nonil test-cond conv-long Try! print-mutableObj make-mmap] ])
(use '[comzotohcljc.util.str :only [hgl? nsb strim] ])
(use '[comzotohcljc.util.ini :only [parse-inifile] ])
(use '[comzotohcljc.netty.comms])
(use '[comzotohcljc.tardis.impl.exec :only [make-execvisor] ])
(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.impl.defaults])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def CLI-TRIGGER (promise))
(def STOPCLI (atom false))

(defn- inizContext ^comzotohcljc.util.core.MuObj [^File baseDir]
  (let [ cfg (File. baseDir ^String DN_CFG)
         home (.getParentFile cfg) ]
    (precondDir home)
    (precondDir cfg)
    (doto (make-context)
          (.setf! K_BASEDIR home)
          (.setf! K_CFGDIR cfg))))

(defn- setupClassLoader [^comzotohcljc.util.core.MuObj ctx]
  (let [ root (.getf ctx K_ROOT_CZLR)
         cl (ExecClassLoader. root) ]
    (set-cldr cl)
    (.setf! ctx K_EXEC_CZLR cl)
    ctx))

(defn- setupClassLoaderAsRoot [^comzotohcljc.util.core.MuObj ctx]
  (let [ root (RootClassLoader. (get-cldr)) ]
    (.setf! ctx K_ROOT_CZLR root)
    ctx))

(defn- maybeInizLoaders [^comzotohcljc.util.core.MuObj ctx]
  (let [ cz (get-cldr) ]
    (cond
      (instance? RootClassLoader cz)
      (do
        (.setf! ctx K_ROOT_CZLR cz)
        (setupClassLoader ctx))
      (instance? ExecClassLoader cz)
      (do
        (.setf! ctx K_ROOT_CZLR (.getParent cz))
        (.setf! ctx K_EXEC_CZLR cz))
      :else
      (setupClassLoader (setupClassLoaderAsRoot ctx)))
    (info "classloaders configured.  using ExecClassLoader.")
    ctx))

(defn- loadConf [^comzotohcljc.util.core.MuObj ctx]
  (let [ ^File home (.getf ctx K_BASEDIR)
         cf (File. home  (str DN_CONF "/" (name K_PROPS) ))
        ^comzotohcljc.util.ini.IWin32Conf
         w (parse-inifile cf)
         lg (.toLowerCase ^String (.optString w K_LOCALE K_LANG "en"))
         cn (.toUpperCase ^String (.optString w K_LOCALE K_COUNTRY ""))
         loc (if (hgl? cn) (Locale. lg cn) (Locale. lg)) ]
    (info (str "using locale: " loc))
    (doto ctx
      (.setf! K_PROPS w)
      (.setf! K_LOCALE loc))) )

(defn- setupResources [^comzotohcljc.util.core.MuObj ctx]
  (let [ rc (get-resource "comzotohcljc/tardis/etc/Resources"
                             (.getf ctx K_LOCALE)) ]
    (test-nonil "etc/resouces" rc)
    (.setf! ctx K_RCBUNDLE rc)
    (info "resource bundle found and loaded.")
    ctx))

(defn- pre-parse [^comzotohcljc.tardis.core.sys.Element cli args]
  (let [ bh (File. ^String (first args))
         ctx (inizContext bh) ]
    (info "inside pre-parse()")
    ;;(precondDir (File. bh ^String DN_BLOCKS))
    (precondDir (File. bh ^String DN_CFG))
    (precondDir (File. bh ^String DN_BOXX))
    (.setf! ctx K_CLISH cli)
    (.setCtx! cli ctx)
    (info "home directory looks ok.")
    ctx))

(defn- start-exec [^comzotohcljc.util.core.MuObj ctx]
  (do
    (info "about to start Hohenheim...")
    (let [ ^Startable exec (.getf ctx K_EXECV) ]
      (.start exec))
    (info "Hohenheim started.")
    ctx))

(defn- primodial [^comzotohcljc.util.core.MuObj ctx]
  (let [ cl (.getf ctx K_EXEC_CZLR)
         cli (.getf ctx K_CLISH)
        ^comzotohcljc.util.ini.IWin32Conf
         wc (.getf ctx K_PROPS)
         cz (.optString wc K_COMPS K_EXECV "") ]
    (test-cond "conf file:exec-visor"
                  (= cz "comzotohcljc.tardis.impl.Execvisor"))
    (info "inside primodial()")
    (let [ ^comzotohcljc.util.core.MuObj execv (make-execvisor cli) ]
      (.setf! ctx K_EXECV execv)
      (synthesize-component execv { :ctx ctx } )
      (info "Execvisor created and synthesized - OK.")
      ctx)))

(defn- stop-cli [^comzotohcljc.util.core.MuObj ctx]
  (let [ ^File pid (.getf ctx K_PIDFILE)
         execv (.getf ctx K_EXECV) ]
    (if-not @STOPCLI
      (do
        (reset! STOPCLI true)
        (when-not (nil? pid) (FileUtils/deleteQuietly pid))
        (info "about to stop Hohenheim...")
        (info "applications are shutting down...")
        (when-not (nil? execv)
          (.stop ^Startable execv))
        (info "Hohenheim stopped.")
        (info "Hohenheim says \"Goodbye\".")
        (deliver CLI-TRIGGER 911)))))

(defn- enableRemoteShutdown [^comzotohcljc.util.core.MuObj ctx]
  (let [ port (conv-long (System/getProperty "gallifrey.kill.port") 4444) ]
    (info "Enabling remote shutdown...")
    (makeMemHttpd
      "127.0.0.1"
      port
      { :usercb (reify NettyServiceIO
                  (onError [_ ch msginfo evt] nil)
                  (preSend [_ ch msg] nil)
                  (onRequest [_ ch req msginfo rdata]
                    (closeCF false (.write ^Channel ch (makeHttpReply 200)))
                    (stop-cli ctx))
                  (onReply [_ ch rsp msginfo rdata] nil)) } )))

(defn- hookShutdown [^comzotohcljc.util.core.MuObj ctx]
  (let [ cli (.getf ctx K_CLISH) ]
    (.addShutdownHook (Runtime/getRuntime)
          (Thread. (reify Runnable
                      (run [_] (Try! (stop-cli ctx))))))
    (enableRemoteShutdown ctx)
    (info "added shutdown hook.")
    ctx))

(defn- writePID [^comzotohcljc.util.core.MuObj ctx]
  (let [ fp (File. ^File (.getf ctx K_BASEDIR) "gallifrey.pid") ]
    (FileUtils/writeStringToFile fp (pid) "utf-8")
    (.setf! ctx K_PIDFILE fp)
    (info "wrote gallifrey.pid - OK.")
    ctx))

(defn- pause-cli [^comzotohcljc.util.core.MuObj ctx]
  (do
    (print-mutableObj ctx)
    (info "applications are now running...")
    (info "system thread paused on promise - awaits delivery.")
    (deref CLI-TRIGGER) ;; pause here
    (info "promise delivered!")
    (safe-wait 5000) ;; give some time for stuff to wind-down.
    (System/exit 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- make-climain [ & args ]
  (let [ impl (make-mmap) ]
    (reify

      Element

      (setCtx! [_ x] (.mm-s impl :ctx x))
      (getCtx [_] (.mm-g impl :ctx))
      (setAttr! [_ a v] (.mm-s impl a v) )
      (clrAttr! [_ a] (.mm-r impl a) )
      (getAttr [_ a] (.mm-g impl a) )

      Hierarchial
      (parent [_] nil)

      Versioned
      (version [_] "1.0")

      Identifiable
      (id [_] K_CLISH)

      Startable

      (start [this]
        (-> (pre-parse this args)
          (maybeInizLoaders)
          (loadConf)
          (setupResources )
          (primodial)
          (start-exec)
          (writePID)
          (hookShutdown)
          (pause-cli)) )

      (stop [this]
        (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this) ]
          (stop-cli ctx))))) )


(defn start-main "" [ & args ]
  (do
    (when (< (count args) 1)
      (throw (CmdHelpError. "Hohenheim Home not defined.")))
    (info "set gallifrey-home= " (first args))
    (let [ ^Startable cm  (apply make-climain args) ]
      (.start cm))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private climain-eof nil)

