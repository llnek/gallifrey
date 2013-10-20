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

  comzotohcljc.jmx.core )

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [make-mmap Try! TryC] ])
(use '[comzotohcljc.util.str :only [hgl? ] ])

(import '(java.lang.management ManagementFactory))
(import '(java.net InetAddress MalformedURLException))
(import '(java.rmi NoSuchObjectException))
(import '(com.zotoh.frwk.core Startable))
(import '(java.rmi.registry LocateRegistry Registry))
(import '(java.rmi.server UnicastRemoteObject))
(import '(javax.management DynamicMBean JMException MBeanServer ObjectName))
(import '(javax.management.remote JMXConnectorServer
  JMXConnectorServerFactory
  JMXServiceURL))
(import '(org.apache.commons.lang3 StringUtils))

(use '[comzotohcljc.jmx.names])
(use '[comzotohcljc.jmx.bean])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkJMXrror "" [^String msg ^Throwable e]
  (throw (doto (JMException. msg)
           (.initCause e))))

(defn- startRMI "" [ ^comzotohcljc.util.core.MutableMapAPI impl]
  (let [ ^long port (.mm-g impl :regoPort) ]
    (try
      (.mm-s impl :rmi (LocateRegistry/createRegistry port))
    (catch Throwable e#
      (mkJMXrror (str "Failed to create RMI registry: " port) e#)))) )

(defn- startJMX "" [ ^comzotohcljc.util.core.MutableMapAPI impl]
  (let [ hn (-> (InetAddress/getLocalHost)(.getHostName))
         ^long regoPort (.mm-g impl :regoPort)
         ^long port (.mm-g impl :port)
         ^String host (.mm-g impl :host)
         endpt (-> "service:jmx:rmi://{{host}}:{{sport}}/jndi/rmi://:{{rport}}/jmxrmi"
                 (StringUtils/replace "{{host}}" (if (hgl? host) host hn))
                 (StringUtils/replace "{{sport}}" (str "" port))
                 (StringUtils/replace "{{rport}}" (str "" regoPort)))
         url (try
               (JMXServiceURL. endpt)
               (catch Throwable e#
                  (mkJMXrror (str "Malformed url: " endpt) e#)))
         ^JMXConnectorServer
         conn (try
                (JMXConnectorServerFactory/newJMXConnectorServer
                  url
                  nil
                  (ManagementFactory/getPlatformMBeanServer))
                (catch Throwable e#
                  (mkJMXrror (str "Failed to connect JMX") e#))) ]
    (try
      (.start conn)
      (catch Throwable e#
        (mkJMXrror (str "Failed to start JMX") e#)))

    (.mm-s impl :beanSvr (.getMBeanServer conn))
    (.mm-s impl :conn conn)))

(defn- doReg "" [ ^MBeanServer svr ^ObjectName objName ^DynamicMBean mbean]
  (try
    (.registerMBean svr mbean objName)
    (catch Throwable e#
      (mkJMXrror (str "Failed to register bean: " objName) e#)))
  (info "registered jmx-bean: " objName)
  objName)

(defprotocol JMXServer
  ""
  (reg [_ obj domain nname paths] )
  (setRegistryPort [_ port])
  (setServerPort [_ port])
  (reset [_] )
  (dereg [_ nm] ))

(defn make-jmxServer "" [ ^String host]
  (let [ impl (make-mmap)
         objNames (atom []) ]
    (.mm-s impl :regoPort 7777)
    (.mm-s impl :port 0)
    (reify

      JMXServer

      (reset [_]
        (let [ ^MBeanServer bs (.mm-g impl :beanSvr) ]
          (doseq [ nm (seq @objNames) ]
            (Try!
               (.unregisterMBean bs nm)) )
          (reset! objNames [])))

      (dereg [_ objName]
        (let [ ^MBeanServer bs (.mm-g impl :beanSvr) ]
          (.unregisterMBean bs objName)))

      (reg [_ obj domain nname paths]
        (let [ ^MBeanServer bs (.mm-g impl :beanSvr) ]
          (try
            (reset! objNames
                    (conj @objNames
                          (doReg bs (make-objectName domain nname paths)
                                 (make-jmxBean obj))))
            (catch Throwable e#
              (mkJMXrror (str "Failed to register object: " obj) e#)))))

      ;; jconsole port
      (setRegistryPort [_ port] (.mm-s impl :regoPort port))

      (setServerPort[_ port] (.mm-s impl :port port))

      Startable

      (start [_]
        (let [ p1 (.mm-g impl :regoPort)
               p2 (.mm-g impl :port) ]
          (when-not (> p2 0) (.mm-s impl :port (inc p1)))
          (startRMI impl)
          (startJMX impl)) )

      (stop [this]
        (let [ ^JMXConnectorServer c (.mm-g impl :conn)
               ^Registry r (.mm-g impl :rmi) ]
          (reset this)
          (when-not (nil? c)
            (TryC (.stop c)))
          (.mm-s impl :conn nil)
          (when-not (nil? r)
            (TryC
              (UnicastRemoteObject/unexportObject r true)))
          (.mm-s impl :rmi nil)))

      )))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private core-eof nil)

