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

(ns ^{  :doc "Utility functions for class related or reflection related operations."
        :author "kenl" }
  comzotohcljc.util.meta)

(import '(java.lang.reflect Member Field Method Modifier))
(use '[ comzotohcljc.util.str :only [eq-any? hgl?] ])
(use '[ comzotohcljc.util.core :only [test-nonil] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmulti is-child "Returns true if clazz is subclass of this base class."
  (fn [a b]
    (cond
      (instance? Class b)
      :class

      :else
      :object)))

(defmethod is-child :class
  [^Class basz ^Class cz]
  (if (or (nil? basz) (nil? cz)) false (.isAssignableFrom basz cz)))

(defmethod is-child :object
  [^Class basz ^Object obj]
  (if (or (nil? basz) (nil? obj)) false (is-child basz (.getClass obj))))

(defn bytes-class "Return the java class for byte[]."
  ^Class []
  (Class/forName "[B"))

(defn chars-class "Return the java class for char[]."
  ^Class []
  (Class/forName "[C"))

(defn is-boolean? "True if class is Boolean."
  [^Class classObj]
  (eq-any? (.getName classObj) ["boolean" "Boolean" "java.lang.Boolean"] ))

(defn is-void? "True if class is Void."
  [^Class classObj]
  (eq-any? (.getName classObj) ["void" "Void" "java.lang.Void"] ))

(defn is-char? "True if class is Char."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "char" "Char" "java.lang.Character" ] ))

(defn is-int? "True if class is Int."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "int" "Int" "java.lang.Integer" ] ))

(defn is-long? "True if class is Long."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "long" "Long" "java.lang.Long" ] ))

(defn is-float? "True if class is Float."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "float" "Float" "java.lang.Float" ]))

(defn is-double? "True if class is Double."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "double" "Double" "java.lang.Double" ]))

(defn is-byte? "True if class is Byte."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "byte" "Byte" "java.lang.Byte" ]))

(defn is-short? "True if class is Short."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "short" "Short" "java.lang.Short" ]))

(defn is-string? "True if class is String."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "String" "java.lang.String" ]))

(defn is-object? "True if class is Object."
  [^Class classObj]
  (eq-any? (.getName classObj) [ "Object" "java.lang.Object" ]))

(defn is-bytes? "True if class is byte[]."
  [^Class classObj]
  (= classObj (bytes-class)) )

(defn for-name "Load a java class by name."
  (^Class [^String z] (for-name z nil))
  (^Class [^String z ^ClassLoader cl]
    (if (nil? cl) (java.lang.Class/forName z) (java.lang.Class/forName z true cl))) )

(defn get-cldr "Get the current classloader."
  (^ClassLoader [] (get-cldr nil))
  (^ClassLoader [^ClassLoader cl] (if (nil? cl) (.getContextClassLoader (Thread/currentThread)) cl)))

(defn set-cldr "Set current classloader."
  [^ClassLoader cl]
  (let []
    (test-nonil "class-loader" cl)
    (.setContextClassLoader (Thread/currentThread) cl)))

(defn load-class "Load this class by name."
  (^Class [^String clazzName] (load-class clazzName nil))
  (^Class [^String clazzName ^ClassLoader cl]
    (if (not (hgl? clazzName)) nil (.loadClass (get-cldr cl) clazzName))))

(defn- ctorObj
  ^Object [^Class cz]
  (do
    (test-nonil "java-class" cz)
    (.newInstance (.getDeclaredConstructor cz (make-array Class 0))
                  (make-array Object 0)  )))

(defn make-obj "Make an object of this class by calling the default constructor."
  (^Object [^String clazzName] (make-obj clazzName nil))
  (^Object [^String clazzName ^ClassLoader cl]
   (if (not (hgl? clazzName)) nil (ctorObj (load-class clazzName cl)))) )

(defn list-parents "List all parent classes."
  [^Class javaClass]
  (let [ rc (loop [ sum (transient []) par javaClass ]
              (if (nil? par)
                (persistent! sum)
                (recur (conj! sum par) (.getSuperclass par))))  ]
    ;; since we always add the original class, we need to ignore it on return
    (if (> (count rc) 1) (rest rc) [] )))


(defn- iterXXX [ ^Class cz ^long level getDeclXXX bin ]
  (let [ props (getDeclXXX cz) ]
    (reduce (fn [sum ^Member m]
              (let [ x (.getModifiers m) ]
                (if (and (> level 0)
                         (or (Modifier/isStatic x) (Modifier/isPrivate x)) )
                  sum
                  (assoc! sum (.getName m) m)))) bin props) ))

(defn- listMtds [ ^Class cz ^long level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level (fn [^Class c] (.getDeclaredMethods c))
             (if (nil? par) (transient {}) (listMtds par (inc level))))))

(defn- listFlds [ ^Class cz ^long level ]
  (let [ par (.getSuperclass cz) ]
    (iterXXX cz level (fn [^Class c] (.getDeclaredFields c))
             (if (nil? par) (transient {}) (listFlds par (inc level))))))

(defn list-methods "List all methods belonging to this class, including inherited ones."
  [^Class javaClass]
  (vals (if (nil? javaClass)
          {}
          (persistent! (listMtds javaClass 0 ))   )) )

(defn list-fields "List all fields belonging to this class, including inherited ones."
  [^Class javaClass]
  (vals (if (nil? javaClass)
          {}
          (persistent! (listFlds javaClass 0 ))     )) )





(def ^:private meta-eof nil)

