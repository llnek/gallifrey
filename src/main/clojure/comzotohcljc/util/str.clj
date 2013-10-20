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

(ns ^{ :doc "String utilities."
       :author "kenl" }

  comzotohcljc.util.str)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io CharArrayWriter File
  OutputStream OutputStreamWriter
  Reader Writer))
(import '(java.util Arrays Collection
  Iterator StringTokenizer))
(import '(java.lang StringBuilder))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- ^String lcs [^String s] (.toLowerCase s))
(defn- ^String ucs [^String s] (.toUpperCase s))

(defn has-nocase? "Returns true if this sub-string is inside this string."
  [^String aStr s]
  (do
    (>= (.indexOf (lcs aStr) (lcs s)) 0)))

(defn embeds? "Returns true if this sub-string is inside this string."
  [^String aStr ^String s]
  (do
    (>= (.indexOf aStr s) 0)))

(defn has? "Returns true if this character is inside this string."
  [^String aStr ^Character ch]
  (do
    (>= (.indexOf aStr (int ch)) 0)))

(defn nsb "Returns empty string if obj is null, or obj.toString."
  ^String [^Object obj]
  (cond

    (nil? obj)
    ""

    (keyword? obj)
    (name obj)

    :else
    (.toString obj)))

(defn nsn "Returns \"(null)\" if obj is null, or obj.toString."
  ^String [^Object obj]
  (if (nil? obj) "(null)" (.toString obj)))

(defn same? "Returns true if these 2 strings are the same."
  [^String a ^String b]
  (cond
    (and (nil? a) (nil? b))
    true

    (or (nil? a) (nil? b))
    false

    (not= (.length a) (.length b))
    false

    :else
    (Arrays/equals (.toCharArray a) (.toCharArray b)) ) )

(defn hgl? "Returns true if this string is not empty."
  [^String s]
  (if (nil? s) false (> (.length s) 0)))

(defn nichts?  "Returns true if this string is empty."
  [^String s]
  (not (hgl? s)))

(defn strim "Safely trim this string - handles null."
  ^String [^String s]
  (if (nil? s) "" (.trim s)))

(defn add-delim! "Append to a string-builder, optionally inserting a delimiter if the buffer is not empty."
  ^StringBuilder [^StringBuilder buf ^String delim ^String item]
  (do
    (when-not (nil? item)
      (when (and (> (.length buf) 0) (not (nil? delim)))
        (.append buf delim))
      (.append buf item))
    buf))

(defn splunk "Split a large string into chucks, each chunk having a specific length."
  [^String largeString chunkLength]
  (if (nil? largeString)
    []
    (loop [ ret (transient []) src largeString ]
      (if (<= (.length src) chunkLength)
        (persistent! (if (> (.length src) 0)
          (conj! ret src)
          ret) )
        (recur (conj! ret (.substring src 0 chunkLength))
               (.substring src chunkLength)) ))))

(defn hasic-any? "Tests String.indexOf() against a list of possible args. (ignoring case)."
  [^String src substrs]
  (if (nil? src)
    false
    (if (some #(>= (.indexOf (lcs src) (lcs %)) 0) substrs) true false)))

(defn has-any? "Returns true if src contains one of these substrings."
  [^String src substrs]
  (if (nil? src)
    false
    (if (some (fn [^String s] (>= (.indexOf src s) 0)) substrs) true false)))

(defn swic-any? "Tests startsWith (ignore-case)."
  [^String src pfxs]
  (if (nil? src)
    false
    (if (some #(.startsWith (lcs src) (lcs %)) pfxs) true false)))

(defn sw-any? "Tests startWith(), looping through the list of possible prefixes."
  [^String src pfxs]
  (if (nil? src)
    false
    (if (some #(.startsWith src %) pfxs) true false)))

(defn eqic-any? "Tests String.equals() against a list of possible args. (ignore-case)."
  [^String src strs]
  (if (nil? src)
    false
    (if (some #(.equalsIgnoreCase src %) strs) true false)))

(defn eq-any? "Tests String.equals() against a list of possible args."
  [^String src strs]
  (if (nil? src)
    false
    (if (some #(.equals src %) strs) true false)))

(defn make-string "Make a string of contain length."
  ^String [ ch  cnt]
  (let [ buf (StringBuilder.) ]
    (dotimes [ n cnt ]
      (.append buf ^Character ch))
    (.toString buf)) )

(defn right "Gets the rightmost len characters of a String."
  ^String [^String src len]
  (if (nil? src)
    ""
    (StringUtils/right src ^long len)) )

(defn left "Gets the leftmost len characters of a String."
  ^String [^String src len]
  (if (nil? src)
    ""
    (StringUtils/left src ^long len)) )




(def ^:private str-eof nil)

