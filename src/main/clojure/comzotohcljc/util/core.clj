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

(ns ^{ :doc "General utilties." :author "kenl" }
  comzotohcljc.util.core)

(use '[clojure.tools.logging :only [info warn error debug] ])
;;(:use [clojure.string])
(import '(java.security SecureRandom))
(import '(java.net URL))
(import '(java.nio.charset Charset))
(import '(java.io
  InputStream File FileInputStream
  ByteArrayInputStream ByteArrayOutputStream))
(import '(java.util Properties Date Calendar GregorianCalendar TimeZone))
(import '(java.util.zip DataFormatException Deflater Inflater))
(import '(java.sql Timestamp))
(import '(java.rmi.server UID))
(import '(org.apache.commons.lang3.text StrSubstitutor))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.commons.io IOUtils FilenameUtils))
(import '(org.apache.commons.lang3 SerializationUtils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmulti ^String nice-fpath "Convert the path into nice format (no) backslash." class)
(defmulti ^Properties load-javaprops "Load java properties from input-stream." class)

(def ^:private _BOOLS #{ "true" "yes"  "on"  "ok"  "active"  "1"} )
(def ^:private _PUNCS #{ \_ \- \. \( \) \space } )
(deftype TYPE_NICHTS [])

(defmacro TryC "Catch exception and log it."
  [ & exprs ]
  `(try (do ~@exprs) (catch Throwable e# (warn e# "") nil )) )

(defmacro Try! "Eat all exceptions."
  [ & exprs ]
  `(try (do ~@exprs) (catch Throwable e# nil )) )

(defmacro notnil? "True is x is not nil."
  [x]
  `(not (nil? ~x)))

(def NICHTS (TYPE_NICHTS.) )
(def _KB 1024)
(def _MB (* 1024 1024))
(def _GB (* 1024 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- nsb "" ^String [s] (if (nil? s) "" (.toString ^Object s)))

(defn- get-czldr ""
  (^ClassLoader [] (get-czldr nil) )
  (^ClassLoader [^ClassLoader cl]
    (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl)))

(defn nil-nichts "" ^Object [obj] (if (nil? obj) NICHTS obj))

(defn is-nichts? "" [obj] (identical? obj NICHTS))

(defn flatten-nil "Get rid of any nil(s) in a sequence."
  [vs]
  (cond
    (nil? vs)
    nil
    (empty? vs)
    []
    :else
    (into [] (remove nil? vs))))

(defn ndz "Returns 0.0 if param is nil." ^double [d] (if (nil? d) 0.0 d))
(defn nnz "Returns 0 is param is nil." ^long [n] (if (nil? n) 0 n))
(defn nbf "Returns false if param is nil." [b] (if (nil? b) false b))

(defn match-char? "Returns true if this char exists inside this set of chars."
  [ch setOfChars]
  (if (nil? setOfChars) false (contains? setOfChars ch)))

(defn sysvar "Get value for this system property."
  ^String
  [^String v]
  (if (StringUtils/isEmpty v) nil (System/getProperty v)))

(defn envvar "Get value for this env var."
  ^String
  [^String v]
  (if (StringUtils/isEmpty v) nil (System/getenv v)))

(defn uid "Generate a unique id using std java."
  ^String
  []
  (.replaceAll (.toString (UID.)) "[:\\-]+" ""))

(defn new-random "Return a new random object."
  (^SecureRandom [] (new-random 4))
  (^SecureRandom [numBytes] (SecureRandom. (SecureRandom/getSeed numBytes)) ))

(defn now-jtstamp "Return a java sql Timestamp."
  ^Timestamp
  []
  (Timestamp. (.getTime (Date.))))

(defn now-date "Return a java Date." ^Date [] (Date.) )

(defn now-cal "Return a Gregorian Calendar."
  ^Calendar
  []
  (GregorianCalendar. ))

(defn to-charset "Return a java Charset of the encoding."
  (^Charset [^String enc] (Charset/forName enc))
  (^Charset [] (to-charset "utf-8")) )

(defmethod nice-fpath String
  ^String
  [^String fpath]
  (FilenameUtils/normalizeNoEndSeparator (nsb fpath) true))

(defmethod nice-fpath File
  ^String
  [^File aFile]
  (if (nil? aFile) "" (nice-fpath (.getCanonicalPath aFile)) ))

(defn subs-var "Replaces all system & env variables in the value."
  ^String
  [^String value]
  (if (nil? value)
    ""
    (.replace (StrSubstitutor. (System/getenv))
              (StrSubstitutor/replaceSystemProperties value))))

(defn subs-svar "Expand any sys-var found inside the string value."
  ^String
  [^String value]
  (if (nil? value) "" (StrSubstitutor/replaceSystemProperties value)) )

(defn subs-evar "Expand any env-var found inside the string value."
  ^String
  [^String value]
  (if (nil? value) "" (.replace (StrSubstitutor. (System/getenv)) value)) )

(defn subs-props "Expand any env & sys vars found inside the property values."
  ^Properties
  [^Properties props]
  (reduce
    (fn [^Properties bc k]
      (.put bc k (subs-var (.get props k))) bc )
    (Properties.) (.keySet props) ))

(defn sysprop "Get the value of a system property."
  ^String
  [^String prop]
  (System/getProperty (nsb prop) ""))

(defn homedir "Get the user's home directory."
  ^File
  []
  (File. (sysprop "user.home")) )

(defn getuser "Get the current user login name."
  ^String
  []
  (sysprop "user.name"))

(defn getcwd "Get the current dir."
  ^String
  []
  (sysprop "user.dir"))

(defn trim-lastPathSep "Get rid of trailing dir paths."
  ^String
  [path]
  (.replaceFirst (nsb path) "[/\\\\]+$"  ""))

(defn serialize "Object serialization."
  [obj]
  (if (nil? obj) nil (SerializationUtils/serialize obj)) )

(defn deserialize "Object deserialization."
  [^bytes bits]
  (if (nil? bits) nil (SerializationUtils/deserialize bits)))

(defn get-classname "Get the object's class name."
  ^String
  [^Object obj]
  (if (nil? obj) "null" (.getName (.getClass obj))))

(defn file-path "Get the file path."
  ^String
  [^File aFile]
  (if (nil? aFile) "" (nice-fpath aFile)))

(defn is-windows? "Returns true if platform is windows."
  []
  (>= (.indexOf (.toLowerCase (sysprop "os.name")) "windows") 0 ))

(defn is-unix? "Returns true if platform is *nix."
  []
  (not (is-windows?)))

(defn conv-long "Parse string as a long value."
  ^long
  [^String s dftLongVal]
  (try (Long/parseLong s) (catch Throwable e# dftLongVal)))

(defn conv-double "Parse string as a double value."
  ^double
  [^String s dftDblVal]
  (try (Double/parseDouble s) (catch Throwable e# dftDblVal)))

(defn conv-bool "Parse string as a boolean value."
  ^Boolean
  [^String s]
  (contains? _BOOLS (.toLowerCase (nsb s))))

(defmethod load-javaprops InputStream
  [^InputStream inp]
  (doto (Properties.) (.load inp)))

(defmethod load-javaprops File
  [^File aFile]
  (load-javaprops (-> aFile (.toURI) (.toURL) )))

(defmethod load-javaprops URL
  [^URL aFile]
  (with-open [ inp (.openStream aFile) ]
    (load-javaprops inp)))

(defn stringify "Make a string from bytes."
  (^String [^bytes bits] (stringify bits "utf-8"))
  (^String [^bytes bits ^String encoding] (if (nil? bits) nil (String. bits encoding))))

(defn bytesify "Get bytes with the right encoding."
  (^bytes [^String s] (bytesify s "utf-8"))
  (^bytes [^String s ^String encoding] (if (nil? s) nil (.getBytes s encoding))))

(defn rc-stream "Load the resource as stream."
  (^InputStream [^String rcPath] (rc-stream rcPath nil))
  (^InputStream [^String rcPath ^ClassLoader czLoader]
    (if (nil? rcPath) nil (.getResourceAsStream (get-czldr czLoader) rcPath))) )

(defn rc-url "Load the resource as URL."
  (^URL [^String rcPath] (rc-url rcPath nil))
  (^URL [^String rcPath ^ClassLoader czLoader]
    (if (nil? rcPath) nil (.getResource (get-czldr czLoader) rcPath))) )

(defn rc-str "Load the resource as string."
  (^String [^String rcPath ^String encoding] (rc-str rcPath encoding nil))
  (^String [^String rcPath] (rc-str rcPath "utf-8" nil))
  (^String [^String rcPath ^String encoding ^ClassLoader czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (stringify (IOUtils/toByteArray inp) encoding ))) )

(defn rc-bytes "Load the resource as byte[]."
  (^bytes [^String rcPath] (rc-bytes rcPath nil))
  (^bytes [^String rcPath ^ClassLoader czLoader]
    (with-open [ inp (rc-stream rcPath czLoader) ]
      (IOUtils/toByteArray inp))) )

(defn deflate "Compress the given byte[]."
  [^bytes bits]
  (if (nil? bits)
    nil
    (let [ buf (byte-array 1024)
           cpz (Deflater.) ]
      (doto cpz
        (.setLevel (Deflater/BEST_COMPRESSION))
        (.setInput bits)
        (.finish))
      (with-open [ bos (ByteArrayOutputStream. (alength bits)) ]
        (loop []
          (if (.finished cpz)
            (.toByteArray bos)
            (do (.write bos buf 0 (.deflate cpz buf)) (recur))
          ))))) )

(defn inflate "Decompress the given byte[]."
  [^bytes bits]
  (if (nil? bits)
    nil
    (let [ buf (byte-array 1024)
           decr (Inflater.)
           baos (ByteArrayOutputStream. (alength bits)) ]
      (.setInput decr bits)
      (loop []
        (if (.finished decr)
            (.toByteArray baos)
            (do (.write baos buf 0 (.inflate decr buf)) (recur))
          )))) )

(defn normalize "Normalize a filepath, hex-code all non-alpha characters."
  [^String fname]
  (let [ rc (reduce
              (fn [^StringBuilder buf ^Character ch]
                (if (or (java.lang.Character/isLetterOrDigit ch)
                        (contains? _PUNCS ch))
                  (.append buf ch)
                  (.append buf (str "0x" (Integer/toString (int ch) 16)) ))
                buf)
              (StringBuilder.)
              (seq fname)) ]
    (str "" rc)))

(defn now-millis "Return the current time in milliseconds."
  ^long
  []
  (java.lang.System/currentTimeMillis))

(defn get-fpath "Return the file path only."
  ^String
  [^String fileUrlPath]
  (if (nil? fileUrlPath)
    ""
    (.getPath (URL. fileUrlPath))) )

(defn fmt-fileurl "Return the file path as URL."
  ^URL
  [^String path]
  (if (nil? path)
    nil
    (.toURL (.toURI (File. path)))))

(defn- fetch-tmpdir ^File [extra]
  (let [ fp (File. (str (sysprop "java.io.tmpdir") "/" extra) ) ]
    (.mkdirs fp)
    fp))

(defn make-tmpdir "Generate and return a new temp File dir."
  ^File
  []
  (fetch-tmpdir (uid)))

(defn get-tmpdir "Return the current temp File dir."
  ^File
  []
  (fetch-tmpdir ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti test-isa "Tests if object is subclass of parent."
  (fn [a b c] (cond (instance? Class b) :class :else :object)))

(defmethod test-isa :class
  [^String param ^Class childz ^Class parz]
  (assert (and (not (nil? childz)) (.isAssignableFrom parz childz))
        (str "" param " not-isa " (.getName parz)) ))

(defmethod test-isa :object
  [^String param ^Object obj ^Class parz]
  (assert (and (not (nil? obj)) (.isAssignableFrom parz (.getClass obj)))
        (str "" param " not-isa " (.getName parz)) ))

(defn test-nonil "Assert object is not null."
  [^String param obj]
  (assert (not (nil? obj)) (str "" param " is null.")))

(defn test-cond "Assert a condition."
  [^String msg cnd ]
  (assert (= cnd true) (str msg)))

(defn test-nestr "Assert string is not empty."
  [^String param v]
  (assert (not (StringUtils/isEmpty v)) (str "" param " is empty.")))

(defmulti test-nonegnum "Assert number is not negative."
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmulti test-posnum "Assert number is positive."
  (fn [a b]
    (cond
      (instance? Double b) :double
      (instance? Long b) :long
      (instance? Float b) :double
      (instance? Integer b) :long
      :else (throw (IllegalArgumentException. "allow numbers only")))))

(defmethod test-nonegnum :double
  [^String param v]
  (assert (>= v 0.0) (str "" param " must be >= 0.")))

(defmethod test-nonegnum :long
  [^String param v]
  (assert (>= v 0) (str "" param " must be >= 0.")))

(defmethod test-posnum :double
  [^String param v]
  (assert (> v 0.0) (str "" param " must be > 0.")))

(defmethod test-posnum :long
  [^String param v]
  (assert (> v 0) (str "" param " must be > 0.")))

(defn test-neseq "Assert sequence is not empty."
  [^String param v]
  (assert (not (nil? (not-empty v))) (str  param  " must be non empty.") ))

(defn throwBadArg "Force throw a bad parameter exception."
  [msg]
  (throw (IllegalArgumentException. ^String msg)))

(defn root-cause "Dig into error and find the root exception."
  ^Throwable
  [root]
  (loop [r root t (if (nil? root) nil (.getCause ^Throwable root)) ]
    (if (nil? t)
      r
      (recur t (.getCause t)) )))

(defn root-causemsg "Dig into error and find the root exception message."
  [root]
  (let [ e (root-cause root) ]
    (if (nil? e) "" (str (.getName (.getClass e)) ": " (.getMessage e)))))

(defn gen-numbers "Return a list of random int numbers between a range."
  ^clojure.lang.IPersistentCollection
  [start end howMany]
  (if (or (>= start end) (< (- end start) howMany) )
    []
    (let [ _end (if (< end Integer/MAX_VALUE) (+ end 1) end )
           r (new-random) ]
      (loop [ rc [] cnt howMany ]
        (if (<= cnt 0)
          rc
          (let [ n (.nextInt r _end) ]
            (if (and (>= n start) (not (contains? rc n)))
              (recur (conj rc n) (dec cnt))
              (recur rc cnt) )))))) )

(defn sort-join "Sort a list of strings and then concatenate them."
  ([ss] (sort-join "" ss))
  ([sep ss] (if (nil? ss) "" (clojure.string/join sep (sort ss)))))

(defn into-map ""
  [^Properties props]
  (persistent! (reduce (fn [sum k]
                         (assoc! sum (keyword k) (.get props k)))
                       (transient {})
                       (seq (.keySet props)))) )

(defprotocol MuObj
  "A Mutable Object."
  (setf! [_ k v] )
  (seq* [_] )
  (getf [_ k] )
  (clear! [_] )
  (clrf! [_ k] ))

(defprotocol MutableMapAPI
  "A Mutable Map."
  (mm-r [_ k] )
  (mm-m* [_] )
  (mm-g [_ k] )
  (mm-c [_] )
  (mm-s [_ k v]))

(deftype MutableMap [ ^:unsynchronized-mutable data ] MutableMapAPI
  (mm-s [_ k v] (set! data (assoc data k v)))
  (mm-r [_ k] (set! data (dissoc data k)))
  (mm-m* [_] (seq data))
  (mm-g [_ k] (get data k))
  (mm-c [_ ] (set! data {} )) )

(defn make-mmap ""
  ^comzotohcljc.util.core.MutableMapAPI
  []
  (MutableMap. {} ))

(defn print-mutableObj ""
  ([ctx] (print-mutableObj ctx false))
  ([^comzotohcljc.util.core.MuObj ctx dbg]
    (let [ b (StringBuilder.) ]
      (doseq [ [k v] (.seq* ctx) ]
        (.append b (str k " = " v "\n")))
      (let [ s (str "\n" b) ]
        (if dbg
          (debug s)
          (info s)))) ))

(defn stripNSPath [path]
  (let [ s (str path) ]
    (if (.startsWith s ":")
      (.substring s 1)
      s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private core-eof nil)

