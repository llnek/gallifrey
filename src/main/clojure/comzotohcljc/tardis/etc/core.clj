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

  comzotohcljc.tardis.etc.core
  (:gen-class))

(import '(com.zotoh.gallifrey.etc CmdHelpError))
(import '(java.util Locale))
(import '(java.io File))

(use '[clojure.tools.logging :only [info debug] ])
(use '[comzotohcljc.tardis.etc.cmdline :only [get-commands eval-command] ])
(use '[comzotohcljc.util.core :only [test-cond] ])
(use '[comzotohcljc.util.str :only [make-string] ])
(use '[comzotohcljc.util.files :only [dir-read?] ])
(use '[comzotohcljc.i18n.resources :only [get-resource] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(def ^:private CMDLINE-INFO [
  ["new [mvc|jetty|basic] <app-name> "  "e.g. new mvc foo"]
  ["podify <app-name>"  "e.g. package app as a pod file"]

  ["ide eclipse <app-name>" "Generate eclipse project files."]
  ["build <app-name> [target]" "Build app."]
  ["test <app-name>" "Run test cases."]

  ["debug" "Start & debug the application."]
  ["start [bg]" "Start the application."]

  ["generate serverkey" "Create self-signed server key (pkcs12)."]
  ["generate password" "Generate a random password."]
  ["generate csr" "Create a Certificate Signing Request."]
  ["encrypt <password> <clear-text>" "e.g. encrypt SomeSecretData"]
  ["decrypt <password> <cipher-text>" "e.g. decrypt Data"]
  ["hash <password>" "e.g. hash SomePassword"]
  ["testjce" "Check JCE  Policy Files."]

  ["demo samples" "Generate a set of samples."]
  ["version" "Show version info."] ])

(defn- drawHelpLines [^String fmt ^clojure.lang.IPersistentCollection arr]
  (doseq [ [k v] (seq arr) ]
    (print (String/format fmt (into-array Object [k v]) ))))

(defn- usage []
 (println (make-string \= 78))
  (println "> tardis <commands & options>")
  (println "> -----------------")
  (drawHelpLines "> %-35s %s\n" CMDLINE-INFO)
  (println ">")
  (println "> help - show standard commands")
  (println (make-string \= 78))
  )


;; arg(0) is gallifrey-home
;;println("#### apprunner loader = " + getClass().getClassLoader().getClass().getName())
;;println("#### sys loader = " + ClassLoader.getSystemClassLoader().getClass().getName())
;;mkCZldrs(home)
(defn- parseArgs [rcb & args]
  (let [ h (File. ^String (first args)) ]
    (test-cond (str "Cannot access Gallifrey home " h) (dir-read? h))
      (if (not (contains? (get-commands) (keyword (nth args 1))))
        false
        (fn [] (apply eval-command h rcb (drop 1 args))))))

(defn -main "Main Entry" [& args]
  ;;(debug "Gallifrey: Main Entry")
  ;; for security, don't just eval stuff
  ;;(alter-var-root #'*read-eval* (constantly false))
  (let [ rcpath (str "comzotohcljc/tardis/etc/Resources")
         rcb (get-resource rcpath (Locale/getDefault)) ]
    (if (< (count args) 2)
      (usage)
      (let [ rc (apply parseArgs rcb args) ]
        (if (fn? rc)
          (try (rc) (catch CmdHelpError e# (usage)))
          (usage))))))






(def ^:private core-eof nil)

