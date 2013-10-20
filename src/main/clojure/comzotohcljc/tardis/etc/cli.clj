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
  comzotohcljc.tardis.etc.cli )

(use '[clojure.tools.logging :only [info warn error debug] ])
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.io.filefilter
  FileFileFilter FileFilterUtils))
(import '(org.apache.commons.io
  FilenameUtils FileUtils))
(import '(java.util UUID))
(import '(java.io File))

(use '[comzotohcljc.util.files :only [unzip] ])
(use '[comzotohcljc.util.core :only [uid is-windows?] ])
(use '[comzotohcljc.util.ini :only [parse-inifile] ])
(require '[clojure.data.json :as json])
(use '[comzotohcljc.tardis.core.constants])
(use '[comzotohcljc.tardis.etc.task])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- copy-files "" [^File srcDir ^File destDir ext]
  (FileUtils/copyDirectory
    srcDir
    destDir
    (FileFilterUtils/andFileFilter FileFileFilter/FILE
                                   (FileFilterUtils/suffixFileFilter (str "." ext)))))

(defn- sanitizeAppDomain [appDomain]
  (-> appDomain
    (StringUtils/stripStart ".")
    (StringUtils/stripEnd ".")))

(defn runAppBg "" [^File hhhHome bg]
  (let [ prog2 (.getCanonicalPath (File. hhhHome "bin/gallifrey.bat"))
         prog (.getCanonicalPath (File. hhhHome "bin/gallifrey"))
         pj (if (is-windows?)
              (make-ExecTask "cmd.exe" hhhHome
                             [ "/C" "start" "/B" "/MIN" prog2 "start" ])
              (make-ExecTask prog hhhHome [ "start" "bg" ])) ]
    (execProj pj)))

(defn bundleApp "" [^File hhhHome appId]
  (let [ srcDir (File. hhhHome (str "apps/" appId))
         pod (File. hhhHome (str "pods/" appId ".pod"))
         pj (make-ZipTask srcDir pod [] [ "build.output.folder/**" ]) ]
    (execProj pj)))

(defn antBuildApp "" [^File hhhHome appId antTarget]
  (let [ pj (make-AntTask hhhHome appId antTarget) ]
    (execProj pj)))

(defn cleanAppClasses "" [^File webzDir ^File czDir]
  (FileUtils/cleanDirectory webzDir)
  (FileUtils/cleanDirectory czDir)
  (.mkdirs czDir))

(defn createDemo "Unzip the demo pod." [^File hhhHome demoId]
  (let [ fp (File. hhhHome (str "docs/samples/" demoId ".pod"))
         dest (File. hhhHome (str "apps/demo-" demoId)) ]
    (debug "Unzipping demo pod: " demoId)
    (when (.exists fp)
      (.mkdirs dest)
      (unzip fp dest))))

(defn createSamples "Unzip all samples." [^File hhhHome]
  (let [ top (File. hhhHome (str "docs/samples"))
         fs (.listFiles top) ]
    (debug "Unzipping all samples.")
    (doseq [ ^File f (seq fs) ]
      (when (and (.isFile f) (.endsWith (.getName f) ".pod"))
        (createDemo hhhHome (FilenameUtils/getBaseName (.toString f)))))))

(defn- post-create-app "" [^File hhhHome appId ^String appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId))
         h2db (str (if (is-windows?) "/c:/temp/" "/tmp/") (uid))
         appDomainPath (.replace appDomain "." "/") ]
    (-> (File. h2db) (.mkdirs))
    (with-local-vars [ fp nil ]
      (var-set fp (File. appDir (str "src/main/clojure/" appDomainPath "/core.clj")))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)) "utf-8")

      (var-set fp (File. appDir (str "src/main/clojure/" appDomainPath "/pipe.clj")))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)) "utf-8")

      (var-set fp (File. appDir "conf/env.conf"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@H2DBPATH@@" (str h2db "/" appId))
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)) "utf-8")

      (var-set fp (File. appDir "build.xml"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPCLJFILES@@"
                               (str "<arg value=\""
                                    appDomain
                                    ".core\"/>"
                                    "<arg value=\""
                                    appDomain
                                    ".pipe\"/>" ))
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)
          (StringUtils/replace "@@APPID@@" appId)) "utf-8")
    )))

(defn- create-app-common "" [^File hhhHome appId ^String appDomain flavor]
  (let [ appDir (doto (File. hhhHome (str "apps/" appId)) (.mkdirs))
         mfDir (doto (File. appDir "META-INF")(.mkdirs))
         appDomainPath (.replace appDomain "." "/") ]
    (with-local-vars [ fp nil ]
      (doseq [ s ["classes" "patch" "lib"]]
        (-> (File. appDir (str "POD-INF/" s)) (.mkdirs)))

      (doseq [ s ["RELEASE-NOTES.txt" "NOTES.txt" "LICENSE.txt" "README.md"]]
        (FileUtils/touch (File. mfDir ^String s)))

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/build.xml")
                                     (File. hhhHome (str "apps/" appId)))

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/MANIFEST.MF")
                                     mfDir)

      (-> (File. appDir "modules")(.mkdirs))
      (-> (File. appDir "conf")(.mkdirs))
      (-> (File. appDir "docs")(.mkdirs))

      (doseq [ s ["app.conf" "env.conf" "shiro.ini"]]
        (FileUtils/copyFileToDirectory (File. hhhHome (str "etc/app/" s))
                                       (File. appDir "conf")))

      (var-set fp (File. appDir "conf/app.conf"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@USER@@"
                               (System/getProperty "user.name")))
        "utf-8")

      (doseq [ s [ "scala" "java" (str "clojure/" appDomainPath) ]]
        (-> (File. appDir (str "src/main/" s)) (.mkdirs))
        (-> (File. appDir (str "src/test/" s)) (.mkdirs)))

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/core.clj")
                                     (File. appDir (str "src/main/clojure/" appDomainPath)))
      (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/pipe.clj")
                                     (File. appDir (str "src/main/clojure/" appDomainPath)))

      (-> (File. appDir "src/main/resources") (.mkdirs))

      (doseq [ s ["build.xs" "ivy.config.xml" "ivy.xml" "pom.xml"]]
        (FileUtils/copyFileToDirectory (File. hhhHome (str "etc/app/" s))
                                       appDir))

      (var-set fp (File. mfDir "MANIFEST.MF"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPKEY@@" (.toString (UUID/randomUUID)))
          (StringUtils/replace "@@APPMAINCLASS@@"  (str appDomain ".core.MyAppMain")))
                                   "utf-8")

      (var-set fp (File. appDir "pom.xml"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)
          (StringUtils/replace "@@APPID@@" appId)) "utf-8")

      (var-set fp (File. appDir "ivy.xml"))
      (FileUtils/writeStringToFile  ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)
          (StringUtils/replace "@@APPID@@" appId)) "utf-8")

      (var-set fp (File. appDir "build.xs"))
      (FileUtils/writeStringToFile  ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPTYPE@@" flavor)
          (StringUtils/replace "@@GALLIFREYHOME@@" (.getCanonicalPath hhhHome))) "utf-8")

    )))

(defn createBasic "" [^File hhhHome appId ^String appDomain]
  (create-app-common hhhHome appId appDomain "basic")
  (post-create-app hhhHome appId appDomain))

(defn- create-web-common "" [^File hhhHome appId ^String appDomain]
  (let [ wfc (File. hhhHome (str DN_CFG "/app/weblibs.conf" ) )
         wbs (json/read-str (FileUtils/readFileToString wfc "utf-8")
                            :key-fn keyword)
         appDir (File. hhhHome (str "apps/" appId))
         wlib (doto (File. appDir "public/vendors") (.mkdirs))
         hf (parse-inifile (File. hhhHome (str DN_CONF "/" (name K_PROPS))))
         wlg (.optString hf "webdev" "lang" "coffee")
         buf (StringBuilder.)
         appDomainPath (.replace appDomain "." "/") ]

    (doseq [ s ["js" "less"]]
      (-> (File. appDir (str "src/main/" s)) (.mkdirs)))

    (doseq [ s ["controllers" "views" "models"]]
      (-> (File. appDir (str "src/main/" wlg "/" s)) (.mkdirs)))

    (doseq [ s ["images" "scripts" "styles"]]
      (-> (File. appDir (str "public/" s)) (.mkdirs)))

    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/favicon.png")
                                   (File. appDir "public/images"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/pipe.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))

    (-> (File. appDir (str "src/test/" wlg)) (.mkdirs))

    (FileUtils/copyFile wfc (File. wlib ".list"))
    (doseq [ df (:libs wbs) ]
      (let [ dn (:dir df)
             dd (File. hhhHome (str "etc/weblibs/" dn))
             td (File. wlib ^String dn) ]
        (when (.isDirectory dd)
          (FileUtils/copyDirectoryToDirectory dd wlib)
          (when-not (:skip df)
            (doseq [ f (:js df) ]
              (-> buf (.append (FileUtils/readFileToString (File. td ^String f) "utf-8"))
                  (.append (str "\n\n/* @@@" f "@@@ */"))
                  (.append "\n\n")))))))

    (FileUtils/writeStringToFile (File. appDir "public/c/webcommon.js")
                                 (.toString buf)
                                 "utf-8")

    (FileUtils/writeStringToFile (File. appDir "public/c/webcommon.css")
                                 ""
                                 "utf-8")

    ))

(defn createJetty "" [^File hhhHome appId ^String appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId)) ]
    (create-app-common hhhHome appId appDomain "web")
    (create-web-common hhhHome appId appDomain)
    (doseq [ s [ "classes" "lib" ]]
      (-> (File. appDir (str "WEB-INF/" s)) (.mkdirs)))
    (FileUtils/copyFile (File. hhhHome "etc/jetty/jetty.conf")
                        (File. appDir "conf/env.conf"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/jetty/web.xml")
                                   (File. appDir "WEB-INF"))
    (post-create-app hhhHome appId appDomain)))

(defn createWeb "" [^File hhhHome appId ^String appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId))
         appDomainPath (.replace appDomain "." "/") ]
    (with-local-vars [fp nil]
      (create-app-common hhhHome appId appDomain "web")
      (create-web-common hhhHome appId appDomain)
      (copy-files (File. hhhHome "etc/netty") (File. appDir "conf") "conf")
      (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/static-routes.conf")
                                     (File. appDir "conf"))
      (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/routes.conf")
                                     (File. appDir "conf"))
      (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/favicon.png")
                                     (File. appDir "public/images"))
      (doseq [ s ["errors" "htmls"]]
        (-> (File. appDir (str "pages/" s)) (.mkdirs)))

      (copy-files (File. hhhHome "etc/netty") (File. appDir "pages/errors") ".err")
      (copy-files (File. hhhHome "etc/netty") (File. appDir "pages/htmls") "ftl")

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/index.html")
                                     (File. appDir "public"))

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/main.less")
                                     (File. appDir "src/main/less"))

      (var-set fp (File. appDir "conf/routes.conf"))
      (FileUtils/writeStringToFile ^File @fp
        (-> (FileUtils/readFileToString ^File @fp "utf-8")
          (StringUtils/replace "@@APPDOMAIN@@" appDomain)) "utf-8")

      (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/pipe.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))

      (post-create-app hhhHome appId appDomain) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private cli-eof nil)

