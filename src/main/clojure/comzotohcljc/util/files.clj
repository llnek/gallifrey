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

(ns ^{ :doc "General file related utilities."
       :author "kenl" }

  comzotohcljc.util.files)

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io
  File FileInputStream FileOutputStream
  InputStream OutputStream ))
(import '(java.util ArrayList))
(import '(org.apache.commons.io FileUtils))
(import '(org.apache.commons.io IOUtils))
(import '(java.util.zip ZipFile ZipEntry))
(import '(com.zotoh.frwk.io XData))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn file-readwrite? "Returns true if file is readable & writable."
  [^File fp]
  (if (and (not (nil? fp))
           (.exists fp)
           (.isFile fp)
           (.canRead fp)
           (.canWrite fp))
    true
    false) )

(defn file-read? "Returns true if file is readable."
  [^File fp]
  (if (and (not (nil? fp))
           (.exists fp)
           (.isFile fp)
           (.canRead fp))
    true
    false) )

(defn dir-readwrite? "Returns true if directory is readable and writable."
  [^File dir]
  (if (and (not (nil? dir))
           (.exists dir)
           (.isDirectory dir)
           (.canRead dir)
           (.canWrite dir) )
    true
    false) )

(defn dir-read? "Returns true if directory is readable."
  [^File dir]
  (if (and (not (nil? dir))
           (.exists dir)
           (.isDirectory dir)
           (.canRead dir) )
    true
    false) )

(defn can-exec? "Returns true if file or directory is executable."
  [^File fp]
  (if (and (not (nil? fp))
           (.exists fp)
           (.canExecute fp))
    true
    false) )

(defn parent-path "Get the path to the parent directory."
  ^String [^String path]
  (if (StringUtils/isEmpty path)
    path
    (.getParent (File. path))) )

(defn- jiggleZipEntryName ""
  ^String [^ZipEntry en]
  (do
    (.replaceAll (.getName en) "^[\\/]+","")) )

(defn- doOneEntry "" [^ZipFile src ^File des ^ZipEntry en]
  (let [ f (File. des (jiggleZipEntryName en) ) ]
    (if (.isDirectory en)
      (.mkdirs f)
      (do
        (.mkdirs (.getParentFile f))
        (with-open [ inp (.getInputStream src en) ]
          (with-open [ os (FileOutputStream. f) ]
            (IOUtils/copy inp os)))))))

(defn unzip "Unzip contents of zip file to a target folder."
  [^File src ^File des]
  (let [ fpz (ZipFile. src)
         ents (.entries fpz) ]
    (.mkdirs des)
    (loop [ hasMore (.hasMoreElements ents) ]
      (if (false? hasMore)
        nil
        (do
          (doOneEntry fpz des (.nextElement ents))
          (recur (.hasMoreElements ents)))))))

(defn save-file "Save a file to a directory."
  [^File dir ^String fname ^XData xdata]
  (let [ fp (File. dir fname) ]
    (FileUtils/deleteQuietly fp)
    (if (.isDiskFile xdata)
      (FileUtils/moveFile (.fileRef xdata) fp)
      (FileUtils/writeByteArrayToFile fp (.javaBytes xdata)))))

(defn get-file "Get a file from a directory."
  ^XData [^File dir ^String fname]
  (let [ fp (File. dir fname)
         rc (XData.) ]
    (if (and (.exists fp) (.canRead fp))
      (doto rc (.setDeleteFile false)
              (.resetContent fp) )
      nil)) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def ^:private files-eof nil)

