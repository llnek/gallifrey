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

  comzotohcljc.jmx.bean )


(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.frwk.jmx NameParams))

(import '(java.lang.reflect Field Method))
(import '(java.lang Exception IllegalArgumentException))
(import '(java.util Arrays))
(import '(javax.management
  Attribute
  AttributeList
  AttributeNotFoundException
  DynamicMBean
  MBeanAttributeInfo
  MBeanException
  MBeanInfo
  MBeanOperationInfo
  MBeanParameterInfo
  ReflectionException))

(use '[comzotohcljc.util.core :only [make-mmap notnil? TryC] ])
(use '[comzotohcljc.util.str :only [has-any?] ])
(use '[comzotohcljc.util.meta :only [is-boolean? is-void? is-object?
                                     is-string? is-short? is-long?
                                     is-int? is-double? is-float? is-char? ] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ^:private BFieldInfo
  ""
  (isGetter [_] )
  (isSetter [_] )
  (field [_] ))

(defn- mkBFieldInfo "" [^Field fld getr setr]
  (reify BFieldInfo
    (isGetter [_] getr)
    (isSetter [_] setr)
    (field [_] fld)))

(defprotocol ^:private BPropInfo
  ""
  (getType [_] )
  (desc [_] )
  (getter [_] )
  (getName [_] )
  (setter [_] )
  (setSetter [_ m] )
  (setGetter [_ m] )
  (isQuery [_] ))

(defn- mkBPropInfo "" [^String prop
                       ^String descn ^Method getr ^Method setr]
  (let [ impl (make-mmap) ]
    (.mm-s impl :getr getr)
    (.mm-s impl :setr setr)
    (.mm-s impl :type nil)
    (reify BPropInfo
      (getType [this]
        (let [ ^Method g (getter this)
               ^Method s (setter this) ]
          (if (notnil? g)
            (.getReturnType g)
            (if (nil? s)
              nil
              (let [ ps (.getParameterTypes s) ]
                (if (== 1 (count ps)) (first ps) nil))))))
      (getter [_] (.mm-g impl :getr))
      (getName [_] prop)
      (desc [_] descn)
      (setter [_] (.mm-g impl :setr))
      (setSetter [_ m] (.mm-s impl :setr m))
      (setGetter [_ m] (.mm-s impl :getr m))
      (isQuery [this]
        (let [ ^Method g (getter this) ]
          (if (nil? g)
            false
            (and (-> g (.getName)(.startsWith "is"))
                 (is-boolean? (getType this)))))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- unknownError "" [attr]
  (AttributeNotFoundException. (str "Unknown property " attr)))

(defn- beanError "" [^String msg]
  (MBeanException. (Exception. msg)))

(defn- badArg "" [^String msg]
  (IllegalArgumentException. msg))

(defn- assertArgs "" [mtd ptypes n]
  (when (not (== n (count ptypes)))
    (throw (badArg (str "\"" mtd "\" needs " n "args.") ))))

(defn- maybeGetPropName ""
  ^String
  [^String mn]
  (let [ pos (cond
               (or (.startsWith mn "get")
                   (.startsWith mn "set"))
               3
               (.startsWith mn "is")
               2
               :else
               -1) ]
    (if (< pos 0)
      ""
      (str (Character/toLowerCase (.charAt mn pos)) (.substring mn (+ pos 1))))))

(defn- mkParameterInfo "" [^Method mtd]
  (with-local-vars [ ptypes (.getParameterTypes mtd)
                     ctr 1
                     rc (transient []) ]
    (doseq [ ^Class t (seq @ptypes) ]
      (var-set rc
               (conj! @rc (MBeanParameterInfo. (str "p" @ctr) (.getName t) "")))
      (var-set ctr (inc @ctr)))
    (persistent! @rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- testJmxType "" [ ^Class cz]
  (if (or (is-boolean? cz)
          (is-void? cz)
          (is-object? cz)
          (is-string? cz)
          (is-short? cz)
          (is-long? cz)
          (is-int? cz)
          (is-double? cz)
          (is-float? cz)
          (is-char? cz))
    cz
    nil))

(defn- testJmxTypes "" [^Class rtype ptypes]
  (if (and (not (empty? ptypes))
           (true? (some (fn [^Class c] (if (testJmxType c) false true))
                   (seq ptypes))) )
    false
    (testJmxType rtype)))

(defn- handleProps "" [^Class cz mtds]
  (with-local-vars [ props (transient {}) ba (transient []) ]
    (doseq [ ^Method mtd (seq mtds) ]
      (let [ mn (.getName mtd)
             ptypes (.getParameterTypes mtd)
             rtype (.getReturnType mtd)
             pname (maybeGetPropName mn)
             ^comzotohcljc.jmx.bean.BPropInfo
             methodInfo (get props pname) ]
        (cond
          (or (.startsWith mn "is")
              (.startsWith mn "get"))
          (if (== 0 (count ptypes))
            (if (nil? methodInfo)
              (var-set props
                       (assoc! @props pname (mkBPropInfo pname "" mtd nil)))
              (.setGetter methodInfo mtd)))

          (.startsWith mn "set")
          (if (== 1 (count ptypes))
            (if (nil? methodInfo)
              (var-set props
                       (assoc! @props pname (mkBPropInfo pname "" nil mtd)))
              (.setSetter methodInfo mtd)))
          :else nil)))
    (let [ rc (persistent! @props) ]
      (doseq [ [k ^comzotohcljc.jmx.bean.BPropInfo v] rc ]
        (when-let [ ^Class mt (testJmxType (.getType v)) ]
          (conj! @ba
                 (MBeanAttributeInfo.
                   (.getName v)
                   ;; could NPE here if type is nil!
                   (.getName mt)
                   (.desc v)
                   (notnil? (.getter v))
                   (notnil? (.setter v))
                   (.isQuery v)))) )
      [ (persistent! @ba) rc ] )) )

(defn- handleFlds [^Class cz]
  (with-local-vars [ flds (transient {})
                     rc (transient []) ]
    (doseq [ ^Field field (seq (.getDeclaredFields cz)) ]
      (let [ fnm (.getName field) ]
        (when (.isAccessible field)
          (var-set flds
                   (assoc! @flds fnm (mkBFieldInfo field true true)))
          (var-set rc
                   (conj! @rc
                     (MBeanAttributeInfo.
                       fnm
                       (-> field (.getType)(.getName) )
                       (str fnm " attribute")
                       true
                       true
                       (and (.startsWith fnm "is")
                            (is-boolean? (.getType field)))))))))
    [ (persistent! @rc)(persistent! @flds) ] ))

(defn- handleMethods "" [^Class cz mtds]
  (info "jmx-bean: processing class: " cz)
  (with-local-vars [ metds (transient {}) rc (transient []) ]
    (doseq [ ^Method m (seq mtds) ]
      (let [ ^Class rtype (.getReturnType m)
             ptypes (.getParameterTypes m)
             mn (.getName m) ]
        (cond
          (has-any? mn [ "_QMARK" "_BANG" "_STAR" ])
          (info "jmx-skipping " mn)

          (testJmxTypes rtype ptypes)
          (let [ pns (map (fn [^Class c] (.getName c)) (seq ptypes))
                 nameParams (NameParams. mn (into-array String pns))
                 pmInfos (mkParameterInfo m) ]
            (var-set metds (assoc! @metds nameParams m))
            (info "jmx-adding method " mn)
            (var-set rc
                     (conj! @rc (MBeanOperationInfo.
                        mn
                        (str mn " operation")
                        (into-array MBeanParameterInfo pmInfos)
                        (.getName rtype)
                        MBeanOperationInfo/ACTION_INFO ))))

          :else
          (info "jmx-skipping " mn) )))
    [ (persistent! @rc) (persistent! @metds)] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-jmxBean "Make a JMX bean from this object." [^Object obj]
  (let [ impl (make-mmap) cz (.getClass obj)
         ms (.getMethods cz)
         ;;[ps propsMap] (handleProps cz ms)
         ps [] propsMap {}
         ;;[fs fldsMap] (handleFlds cz)
         fs [] fldsMap {}
         [ms mtdsMap] (handleMethods cz ms)
         bi (MBeanInfo. (.getName cz)
                        (str "Information about: " cz)
                        (into-array MBeanAttributeInfo (concat ps fs))
                        nil
                        (into-array MBeanOperationInfo ms)
                        nil) ]
    (reify DynamicMBean
      (getMBeanInfo [_] bi)
      (getAttribute [_ attrName]
        (let [ ^comzotohcljc.jmx.bean.BPropInfo
               prop (get propsMap attrName)
               ^comzotohcljc.jmx.bean.BFieldInfo
               fld (get fldsMap attrName) ]
          (cond
            (nil? prop)
            (do
              (when (or (nil? fld)
                        (not (.isGetter fld)))
                (throw (unknownError attrName)))
              (let [ ^Field f (.field fld) ]
                (.get f obj)))

            (nil? (.getter prop))
            (throw (unknownError attrName))

            :else
            (let [ ^Method f (.getter prop) ]
              (.invoke f obj (into-array Object []))))))

      (getAttributes [this attrNames]
        (let [ rcl (AttributeList.) ]
          (doseq [ ^String nm (seq attrNames) ]
            (try
              (.add rcl (Attribute. nm (.getAttribute this nm)))
              (catch Throwable e#
                (error e# "")
                (.add rcl (Attribute. nm (.getMessage e#))))))
          rcl))

      (setAttribute [_ attr]
        (let [ v (.getValue ^Attribute attr)
               an (.getName ^Attribute attr)
               ^comzotohcljc.jmx.bean.BPropInfo
               prop (get propsMap an)
               ^comzotohcljc.jmx.bean.BFieldInfo
               fld (get fldsMap an) ]
          (cond
            (nil? prop)
            (do
              (when (or (nil? fld)
                        (not (.isSetter fld)))
                (throw (unknownError an)))
              (let [ ^Field f (.field fld) ]
                (.set f obj v)))

            (nil? (.setter prop))
            (throw unknownError an)

            :else
            (let [ ^Method f (.setter prop) ]
              (.invoke f obj v)))))

      (setAttributes [this attrs]
        (let [ rcl (AttributeList. (count attrs)) ]
          (doseq [ ^Attribute a (seq attrs) ]
            (let [ nn (.getName a) ]
              (try
                (.setAttribute this a)
                (.add rcl (Attribute. nn (.getAttribute this nn)))
                (catch Throwable e#
                  (error e# "")
                  (.add rcl (Attribute. nn (.getMessage e#)))))) )
          rcl))

      (invoke [_ opName params sig]
        (let [ ^Method mtd (get mtdsMap (NameParams. opName sig)) ]
          (debug "jmx-invoking method " opName
            "\n(params) " (seq params)
            "\n(sig) " (seq sig))
          (when (nil? mtd)
            (throw (beanError (str "Unknown operation \"" opName "\""))))
          (TryC
            (if (empty? params)
              (.invoke mtd obj (into-array Object []))
              (.invoke mtd obj params)))))

      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private bean-eof nil)

