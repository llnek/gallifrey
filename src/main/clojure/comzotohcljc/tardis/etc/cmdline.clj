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

  comzotohcljc.tardis.etc.cmdline )

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.gallifrey.etc CmdHelpError))
(import '(org.apache.commons.io FileUtils))
(import '(java.util Calendar ResourceBundle Properties Date))
(import '(java.io File))
(import '(com.zotoh.frwk.io IOUtils))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.tardis.core.climain :only [start-main] ])
(use '[comzotohcljc.tardis.etc.cli
       :only [createWeb createJetty createBasic
              antBuildApp bundleApp runAppBg
              createSamples createDemo] ])
(use '[comzotohcljc.i18n.resources :only [get-string] ])
(use '[comzotohcljc.util.core :only [nice-fpath is-windows? flatten-nil conv-long rc-str] ])
(use '[comzotohcljc.util.dates :only [add-months make-cal] ])
(use '[comzotohcljc.util.meta :only [] ])
(use '[comzotohcljc.util.str :only [nsb hgl? strim] ])
(use '[comzotohcljc.util.cmdline :only [make-CmdSeqQ cli-converse] ])
(use '[comzotohcljc.crypto.codec :only [create-strong-pwd pwdify] ])
(use '[comzotohcljc.crypto.core :only [assert-jce PEM_CERT make-ssv1PKCS12 make-csrreq] ])
(use '[comzotohcljc.tardis.core.constants])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *GALLIFREY-RSBUNDLE* nil)
(def ^:dynamic *GALLIFREY-HOME-DIR* "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- rcb ^ResourceBundle [] *GALLIFREY-RSBUNDLE*)
(defn- getHomeDir ^File [] *GALLIFREY-HOME-DIR*)

(defn- getBuildFilePath ^String []
  (nice-fpath (File. (File. (getHomeDir) (str DN_CFG "/app")) "ant.xml")))

(defn- onCreateApp [ & args]
  (let [ hhh (getHomeDir)
         ;; treat as domain e.g com.acme => app = acme
         app (nth args 2)
         t (re-matches #"^[a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z0-9_]+)*" app)
         id (if (nil? t)
              nil
              (if (nil? (last t))
                (first t)
                (.substring ^String (last t) 1))) ]
    (if (nil? id)
      (throw (CmdHelpError.))
      (case (nth args 1)
        ("mvc" "web") (createWeb hhh id app)
        "jetty" (createJetty hhh id app)
        "basic" (createBasic hhh id app)
        (throw (CmdHelpError.)))) ))

(defn- onCreate [ & args]
  (if (< (count args) 3)
    (throw (CmdHelpError.))
    (apply onCreateApp args)))

(defn- onBuild [ & args]
  (if (>= (count args) 2)
    (let [ appId (nth args 1)
           taskId (if (> (count args) 2) (nth args 2) "devmode") ]
      (antBuildApp (getHomeDir) appId taskId))
    (throw (CmdHelpError.))))

(defn- onPodify [ & args]
  (if (> (count args) 1)
    (bundleApp (getHomeDir) (nth args 1))
    (throw (CmdHelpError.))))

(defn- onTest [ & args]
  (if (> (count args) 1)
    (antBuildApp (getHomeDir) (nth args 1) "test")
    (throw (CmdHelpError.))))

(defn- onStart [ & args]
  (let [ s2 (if (> (count args) 1) (nth args 1) "") ]
    (cond
      (and (= s2 "bg") (is-windows?))
      (runAppBg (getHomeDir) true)

      :else
      (start-main (nice-fpath (getHomeDir))))))

(defn- onDebug [ & args]
  (onStart args))

(defn- onDemo [ & args]
  (if (> (count args) 1)
    (let [ s (nth args 1) h (getHomeDir) ]
      (if (= "samples" s)
        (createSamples h)
        (createDemo h s)))
    (throw (CmdHelpError.))))

(defn- generatePassword [len]
  (println (nsb (create-strong-pwd len))))

(defn- make-csr-qs [^ResourceBundle rcb]
  { "fname"
    (make-CmdSeqQ "fname" (get-string rcb "cmd.save.file")
                   "" "csr-req" true
                   (fn [a ^Properties ps] (do (.put ps "fn" a) "")))

    "size"
    (make-CmdSeqQ "size" (get-string rcb "cmd.key.size")
                   "" "1024" true
                   (fn [a ^Properties ps] (do (.put ps "size" a) "fname")))
    "c"
    (make-CmdSeqQ "c" (get-string rcb "cmd.dn.c")
                   "" "US" true
                   (fn [a ^Properties ps] (do (.put ps "c" a) "size")))

    "st"
    (make-CmdSeqQ "st" (get-string rcb "cmd.dn.st")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "st" a) "c")))

    "loc"
    (make-CmdSeqQ "loc" (get-string rcb "cmd.dn.loc")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "l" a) "st")))

    "o"
    (make-CmdSeqQ "o" (get-string rcb "cmd.dn.org")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "o" a) "loc")))

    "ou"
    (make-CmdSeqQ "ou" (get-string rcb "cmd.dn.ou")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "ou" a) "o")))

    "cn"
    (make-CmdSeqQ "cn" (get-string rcb "cmd.dn.cn")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "cn" a) "ou")))
  } )

(defn- make-key-qs [^ResourceBundle rcb]

  {
    "fname"
    (make-CmdSeqQ "fname" (get-string rcb "cmd.save.file")
                     "" "test.p12" true
                     (fn [a ^Properties ps] (do (.put ps "fn" a) "")))

    "pwd"
    (make-CmdSeqQ "pwd" (get-string rcb "cmd.key.pwd")
                     "" "" true
                     (fn [a ^Properties ps] (do (.put ps "pwd" a) "fname")))

    "duration"
    (make-CmdSeqQ "duration" (get-string rcb "cmd.key.duration")
                     "" "12" true
                     (fn [a ^Properties ps] (do (.put ps "months" a) "pwd")))

    "size"
    (make-CmdSeqQ "size" (get-string rcb "cmd.key.size")
                   "" "1024" true
                   (fn [a ^Properties ps] (do (.put ps "size" a) "duration")))

   } )


(defn- keyfile []
  (let [ csr (make-csr-qs *GALLIFREY-RSBUNDLE*)
         k (merge csr (make-key-qs *GALLIFREY-RSBUNDLE*))
         rc (cli-converse k "cn") ]
    (when-not (nil? rc)
      (let [ dn (clojure.string/join "," (flatten-nil (map (fn [k]
                                   (let [ v (get rc k) ]
                                     (if (hgl? v)
                                      (str (.toUpperCase (name k)) "=" v)
                                     nil)))
                                   [ :c :st :l :o :ou :cn ])) )
             ff (File. ^String (:fn rc))
             now (Date.) ]
        (println (str "DN entered: " dn))
        (make-ssv1PKCS12
          now
          (.getTime (add-months (make-cal now) (conv-long (:months rc) 12)))
          dn
          (pwdify (:pwd rc))
          (conv-long (:size rc) 1024)
          ff)
        (println (str "Wrote file: " ff))))))


        (defn- csrfile []
  (let [ csr (make-csr-qs *GALLIFREY-RSBUNDLE*)
         rc (cli-converse csr "cn") ]
    (when-not (nil? rc)
      (let [ dn (clojure.string/join "," (flatten-nil (map (fn [k]
                                   (let [ v (get rc k) ]
                                     (if (hgl? v)
                                      (str (.toUpperCase (name k)) "=" v)
                                     nil)))
                                   [ :c :st :l :o :ou :cn ])) )
             [req pkey] (make-csrreq
                          (conv-long (:size rc) 1024)
                          dn
                          PEM_CERT ) ]
        (println (str "DN entered: " dn))
        (let [ ff (File. (str (:fn rc) ".key")) ]
          (FileUtils/writeByteArrayToFile ff pkey)
          (println (str "Wrote file: " ff)))
        (let [ ff (File. (str (:fn rc) ".csr")) ]
          (FileUtils/writeByteArrayToFile ff req)
          (println (str "Wrote file: " ff))) ))))

(defn- onGenerate [ & args]
  (let [ ok (if (> (count args) 1)
              (case (nth args 1)
                "password" (do (generatePassword 12) true)
                "serverkey" (do (keyfile) true)
                "csr" (do (csrfile) true)
                false)
              false) ]
    (when-not ok
      (throw (CmdHelpError.)))))

(defn- genHash [text]
  (let [ ^comzotohcljc.crypto.codec.Password p (pwdify text) ]
    (println (.hashed p))))

(defn- onHash [ & args]
  (if (> (count args) 1)
    (genHash (nth args 1))
    (throw (CmdHelpError.))))

(defn- encrypt [pkey text]
  (let [ ^comzotohcljc.crypto.codec.Password p (pwdify text pkey) ]
    (println (.encoded p))))

(defn- onEncrypt [ & args]
  (if (> (count args) 2)
    (encrypt  (nth args 1) (nth args 2))
    (throw (CmdHelpError.))))

(defn- decrypt [pkey secret]
  (let [ ^comzotohcljc.crypto.codec.Password p (pwdify secret pkey) ]
    (println (.text p))))

(defn- onDecrypt [ & args]
  (if (> (count args) 2)
    (decrypt (nth args 1) (nth args 2))
    (throw (CmdHelpError.))))

(defn- onTestJCE [ & args]
  (do
    (assert-jce)
    (println "JCE is OK.")))

(defn- onVersion [ & args]
  (let [ s (FileUtils/readFileToString (File. (getHomeDir) "VERSION") "utf-8") ]
    (if (hgl? s)
      (println s)
      (println "Unknown version."))))

(defn- onHelp [ & args]
  (throw (CmdHelpError.)))

(defn- scanJars [^File dir ^StringBuilder out]
  (let [ sep (System/getProperty "line.separator")
         fs (IOUtils/listFiles dir "jar" false) ]
    (doseq [ f (seq fs) ]
      (doto out
        (.append (str "<classpathentry  kind=\"lib\" path=\""
                      (nice-fpath f) "\"/>" ))
        (.append sep)))))

(defn- genEclipseProj [app]
  (let [ cwd (File. (getHomeDir) (str DN_BOXX "/" app))
         ec (File. cwd "eclipse.projfiles")
         sb (StringBuilder.)
         lang "scala"
         ulang (.toUpperCase lang) ]
    (.mkdirs ec)
    (FileUtils/cleanDirectory ec)
    (FileUtils/writeStringToFile (File. ec ".project")
      (-> (rc-str (str "com/zotoh/gallifrey/eclipse/" lang "/project.txt") "utf-8")
          (StringUtils/replace "${APP.NAME}" app)
          (StringUtils/replace (str "${" ulang ".SRC}")
               (nice-fpath (File. cwd (str "src/main/" lang))))
          (StringUtils/replace "${TEST.SRC}"
               (nice-fpath (File. cwd (str "src/test/" lang)))))
      "utf-8")
    (scanJars (File. (getHomeDir) ^String DN_DIST) sb)
    (scanJars (File. (getHomeDir) ^String DN_LIB) sb)
    (scanJars (File. cwd ^String POD_CLASSES) sb)
    (scanJars (File. cwd ^String POD_LIB) sb)
    (FileUtils/writeStringToFile (File. ec ".classpath")
      (-> (rc-str (str "com/zotoh/gallifrey/eclipse/" lang "/classpath.txt") "utf-8")
          (StringUtils/replace "${CLASS.PATH.ENTRIES}" (.toString sb)))
      "utf-8")))

(defn- onIDE [ & args]
  (if (> (count args) 2)
    (case (nth args 1)
      "eclipse" (genEclipseProj (nth args 2))
      (throw (CmdHelpError.)))
    (throw (CmdHelpError.))))


(def ^:private _ARGS {
  :new #'onCreate
  :ide #'onIDE
  :build #'onBuild
  :podify #'onPodify
  :test #'onTest
  :debug #'onDebug
  :start #'onStart
  :demo #'onDemo
  :generate #'onGenerate
  :encrypt #'onEncrypt
  :decrypt #'onDecrypt
  :hash #'onHash
  :testjce #'onTestJCE
  :version #'onVersion
  :help #'onHelp
  })


(defn eval-command "" [home rcb & args]
  (let [ v (get _ARGS (keyword (first args))) ]
    (when (nil? v)
      (throw (CmdHelpError.)))
    (binding [ *GALLIFREY-HOME-DIR* home
               *GALLIFREY-RSBUNDLE* rcb]
      (apply v args))))

(defn get-commands "" [] (set (keys _ARGS)))









(def ^:private cmdline-eof nil)


