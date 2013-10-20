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

  comzotohcljc.tardis.impl.sys )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(org.apache.commons.lang3 StringUtils))

(import '(com.zotoh.gallifrey.loaders AppClassLoader))
(import '(com.zotoh.frwk.server Component ComponentRegistry))
(import '(com.zotoh.frwk.core
  Disposable Identifiable Hierarchial Versioned Startable))
(import '(java.net URL))
(import '(java.io File))
(import '(java.security SecureRandom))
(import '(java.util.zip ZipFile))
(import '(com.zotoh.frwk.io IOUtils))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.impl.ext])
(use '[comzotohcljc.tardis.impl.defaults
       :rename {enabled? blockmeta-enabled?
                start kernel-start
                stop kernel-stop}])
(use '[ comzotohcljc.util.core :only [make-mmap TryC nice-fpath notnil? new-random] ])
(use '[ comzotohcljc.util.str :only [strim] ])
(use '[ comzotohcljc.util.process :only [safe-wait] ])
(use '[ comzotohcljc.util.files :only [unzip] ])
(use '[ comzotohcljc.util.mime :only [setup-cache] ])
(use '[ comzotohcljc.util.seqnum :only [next-long] ] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deployer

(defn make-deployer "" []
  (let [ impl (make-mmap) ]
    (with-meta
      (reify

        Element

        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )
        (setCtx! [_ x] (.mm-s impl :ctx x) )
        (getCtx [_] (.mm-g impl :ctx) )

        Component
        (id [_] K_DEPLOYER )
        (version [_] "1.0" )

        Hierarchial
        (parent [_] nil)

        Deployer

        (undeploy [this app]
          (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                 dir (File. ^File (.getf ctx K_PLAYDIR) ^String app) ]
            (when (.exists dir)
                (FileUtils/deleteDirectory dir))))

        (deploy [this src]
          (let [ app (FilenameUtils/getBaseName (nice-fpath src))
                 ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                 des (File. ^File (.getf ctx K_PLAYDIR) ^String app) ]
            (when-not (.exists des)
              (unzip src des)))) )

      { :typeid (keyword "czc.tardis.impl/Deployer") } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-contextualize :czc.tardis.impl/Deployer
  [co ctx]
  (do
    (precondDir (maybeDir ctx K_BASEDIR))
    ;;(precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (comp-clone-context co ctx)))

(defmethod comp-initialize :czc.tardis.impl/Deployer
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         ^File py (.getf ctx K_PLAYDIR)
         ^File pd (.getf ctx K_PODSDIR) ]
    (when (.isDirectory pd)
      (doseq [ ^File f (seq (IOUtils/listFiles pd "pod" false)) ]
        (.deploy ^comzotohcljc.tardis.impl.defaults.Deployer co f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kernel

(defn- maybe-start-pod

  [^comzotohcljc.tardis.core.sys.Element knl
   cset
   ^comzotohcljc.tardis.core.sys.Element pod]

  (TryC
    (let [ cache (.getAttr knl K_CONTAINERS)
           cid (.id ^Identifiable pod)
           app (.moniker ^comzotohcljc.tardis.impl.defaults.PODMeta pod)
           ctr (if (and (not (empty? cset))
                        (not (contains? cset app)))
                 nil
                 (make-container pod)) ]
      (debug "start-pod? cid = " cid ", app = " app " !! cset = " cset)
      (if (notnil? ctr)
        (do
          (.setAttr! knl K_CONTAINERS (assoc cache cid ctr))
        ;;_jmx.register(ctr,"", c.name)
          true
          )
        (do
          (info "kernel: container " cid " disabled.")
          false) ) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-kernel "" []
  (let [ impl (make-mmap) ]
    (.mm-s impl K_CONTAINERS {} )
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Component
        (version [_] "1.0")
        (id [_] K_KERNEL )

        Hierarchial
        (parent [_] nil)

        Kernel

        Startable
        (start [this]
          (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                 ^ComponentRegistry root (.getf ctx K_COMPS)
                 ^comzotohcljc.util.ini.IWin32Conf
                  wc (.getf ctx K_PROPS)
                  endorsed (strim (.optString wc K_APPS "endorsed" ""))
                 ^comzotohcljc.tardis.core.sys.Registry
                 apps (.lookup root K_APPS)
                 cs (if (= "*" endorsed)
                      #{}
                      (into #{}
                            (filter (fn [^String s] (> (.length s) 0))
                                    (map #(strim %)
                                         (seq (StringUtils/split endorsed ",;"))) ) )) ]
            ;; need this to prevent deadlocks amongst pods
            ;; when there are dependencies
            ;; TODO: need to handle this better
            (doseq [ [k v] (seq* apps) ]
              (let [ r (-> (new-random) (.nextInt 6)) ]
                (if (maybe-start-pod this cs v)
                  (safe-wait (* 1000 (Math/max (int 1) r))))))) )

        (stop [this]
          (let [ cs (.mm-g impl K_CONTAINERS) ]
            (doseq [ [k v] (seq cs) ]
              (.stop ^Startable v))
            (doseq [ [k v] (seq cs) ]
              (.dispose ^Disposable v))
            (.mm-s impl K_CONTAINERS {}))) )

      { :typeid (keyword "czc.tardis.impl/Kernel") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-podmeta "" [app ver parObj podType appid pathToPOD]
  (let [ pid (str podType "#" (next-long))
         impl (make-mmap) ]
    (info "PODMeta: " app ", " ver ", " podType ", " appid ", " pathToPOD )
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Component
        (version [_] ver)
        (id [_] pid )

        Hierarchial
        (parent [_] parObj)


        PODMeta

        (srcUrl [_] pathToPOD)
        (moniker [_] app)
        (appKey [_] appid)
        (typeof [_] podType))

      { :typeid (keyword "czc.tardis.impl/PODMeta") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :czc.tardis.impl/PODMeta
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         rcl (.getf ctx K_ROOT_CZLR)
         ^URL url (.srcUrl ^comzotohcljc.tardis.impl.defaults.PODMeta co)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (nice-fpath (File. (.toURI  url))) )
    (.setf! ctx K_APP_CZLR cl)))

(defmethod comp-compose :czc.tardis.impl/Kernel
  [co rego]
  ;; get the jmx server from root
  co)

(defmethod comp-contextualize :czc.tardis.impl/Kernel
  [co ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    ;;(precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (comp-clone-context co ctx)))
























(def ^:private sys-eof nil)

