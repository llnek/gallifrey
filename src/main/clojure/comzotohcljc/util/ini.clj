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

(ns ^{ :doc "Functions to load and query a .ini file."
       :author "kenl" }

  comzotohcljc.util.ini)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.net URL))
(import '(java.io File IOException InputStreamReader
  LineNumberReader PrintStream))
(import '(com.zotoh.frwk.util NCOrderedMap))
(import '(java.util Map))

(use '[ comzotohcljc.util.files :only [file-read?] ])
(use '[ comzotohcljc.util.core :only [conv-bool conv-long conv-double] ])
(use '[ comzotohcljc.util.str :only [nsb] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol IWin32Conf
  "A Windows INI file object."
  (getSection [_ sectionName] )
  (sectionKeys [_ ] )
  (dbgShow [_])
  (getString [_ sectionName property] )
  (getLong [_ sectionName property] )
  (getBool [_ sectionName property] )
  (getDouble [_ sectionName property] )
  (optString [_ sectionName property dft] )
  (optLong [_ sectionName property dft] )
  (optBool [_ sectionName property dft] )
  (optDouble [_ sectionName property dft] ) )

(defmulti ^comzotohcljc.util.ini.IWin32Conf parse-inifile "Parse a INI config file." class)

(defn- throwBadIni "" [^LineNumberReader rdr]
  (throw (IOException. (str "Bad ini line: " (.getLineNumber rdr)))))

(defn- throwBadKey [k] (throw (Exception. (str "No such property " k "."))))
(defn- throwBadMap [s] (throw (Exception. (str "No such section " s "."))))

(defn- maybeSection [^LineNumberReader rdr ^Map ncmap ^String line]
  (let [ s (StringUtils/trim (StringUtils/strip line "[]")) ]
    (when (StringUtils/isEmpty s) (throwBadIni rdr))
    (if-not (.containsKey ncmap s) (.put ncmap s (NCOrderedMap.)))
    s))

(defn- maybeLine [^LineNumberReader rdr ^Map ncmap ^Map section ^String line]
  (let [ ^Map kvs (.get ncmap section) ]
    (when (nil? kvs) (throwBadIni rdr))
    (let [ pos (.indexOf line (int \=))
           nm (if (> pos 0) (.trim (.substring line 0 pos)) "" ) ]
        (when (StringUtils/isEmpty nm) (throwBadIni rdr))
        (.put kvs nm (.trim (.substring line (+ pos 1)))) )) )

(defn- evalOneLine
  ^String [^LineNumberReader rdr ^Map ncmap ^String line ^String curSec]
  (let [ ln (.trim line) ]
    (cond
      (or (StringUtils/isEmpty ln) (.startsWith ln "#"))
      curSec

      (.matches ln "^\\[.*\\]$")
      (maybeSection rdr ncmap ln)

      :else
      (do (maybeLine rdr ncmap curSec ln) curSec)

      )) )


(defn- hasKV [^Map m k]
  (let [ kn (name k) ]
    (if (or (nil? kn) (nil? m)) nil (.containsKey m kn)) ))

(defn- getKV
  ^String [^comzotohcljc.util.ini.IWin32Conf cf s k err]

  (let [ kn (name k)
         sn (name s)
         ^Map mp (.getSection cf sn) ]
    (cond
      (nil? mp) (if err (throwBadMap sn) nil)
      (nil? k) (if err (throwBadKey "") nil)
      (not (hasKV mp k)) (if err (throwBadKey kn) nil)
      :else (nsb (.get mp kn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-winini [^Map mapOfSections]
  (reify IWin32Conf

    (getSection [_ sectionName]
      (if (nil? sectionName)
        nil
        (let [ m (.get mapOfSections (name sectionName)) ]
          (if (nil? m) nil (into {} m)))))

    (sectionKeys [_] (.keySet mapOfSections))

    (getString [this section property]
      (nsb (getKV this section property true)))

    (optString [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc) dft rc)))

    (getLong [this section property]
      (conv-long (getKV this section property true) 0))

    (optLong [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (conv-long rc 0))))

    (getDouble [this section property]
      (conv-double (getKV this section property true) 0.0))

    (optDouble [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (conv-double rc 0.0))))

    (getBool [this section property]
      (conv-bool (getKV this section property true) false))

    (optBool [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (conv-bool rc false))))

    (dbgShow [_]
      (let [ buf (StringBuilder.) ]
        (doseq [ [k v] (seq mapOfSections) ]
          (do
            (.append buf (str "[" (name k) "]\n"))
            (doseq [ [x y] (seq v) ]
              (.append buf (str (name x) "=" y)))
            (.append buf "\n")))
        (println buf)))
  ))

(defmethod parse-inifile String
  ^comzotohcljc.util.ini.IWin32Conf [^String fpath]
  (if (nil? fpath)
    nil
    (parse-inifile (File. fpath))))

(defmethod parse-inifile File
  ^comzotohcljc.util.ini.IWin32Conf [^File file]
  (if (or (nil? file) (not (file-read? file)))
    nil
    (parse-inifile (.toURL (.toURI file)))))

(defn- parseIniFile
  ^comzotohcljc.util.ini.IWin32Conf [^URL fUrl]
  (with-open [ inp (.openStream fUrl) ]
    (let [ rdr (LineNumberReader. (InputStreamReader. inp "utf-8"))
           total (NCOrderedMap.) ]
    (loop [ curSec "" line (.readLine rdr)  ]
      (if (nil? line)
        (make-winini total)
        (recur (evalOneLine rdr total line curSec) (.readLine rdr) )))) ))


(defmethod parse-inifile URL
  ^comzotohcljc.util.ini.IWin32Conf [^URL fileUrl]
  (if (nil? fileUrl)
    nil
    (parseIniFile fileUrl)))





(def ^:private ini-eof nil)

