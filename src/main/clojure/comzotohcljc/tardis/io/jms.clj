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

  comzotohcljc.tardis.io.jms)

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.frwk.core Identifiable))
(import '(java.util
  Hashtable Properties ResourceBundle))
(import '(javax.jms Connection
  ConnectionFactory
  Destination
  Connection
  Message
  MessageConsumer
  MessageListener
  Queue
  QueueConnection
  QueueConnectionFactory
  QueueReceiver
  QueueSession
  Session
  Topic
  TopicConnection
  TopicConnectionFactory
  TopicSession
  TopicSubscriber))
(import '(javax.naming Context InitialContext))
(import '(java.io IOException))
(import '(com.zotoh.gallifrey.io JMSEvent))

(use '[comzotohcljc.util.core :only [MuObj make-mmap uid TryC] ])
(use '[comzotohcljc.crypto.codec :only [pwdify] ])
(use '[comzotohcljc.util.seqnum :only [next-long] ])
(use '[comzotohcljc.util.str :only [hgl? nsb] ])
(use '[comzotohcljc.tardis.core.sys])
(use '[comzotohcljc.tardis.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn makeJMSClient "" [container]
  (makeEmitter container :czc.tardis.io/JMS))

(defmethod ioes-reify-event :czc.tardis.io/JMS
  [co & args]
  (let [ eeid (next-long)
         impl (make-mmap)
         msg (first args) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        JMSEvent
        (bindSession [_ s] (.mm-s impl :ios s))
        (getSession [_] (.mm-g impl :ios))
        (getId [_] eeid)
        (emitter [_] co)
        (getMsg [_] msg))

      { :typeid :czc.tardis.io/JMSEvent } )))

(defn- onMessage [^comzotohcljc.tardis.io.core.EmitterAPI co msg]
      ;;if (msg!=null) block { () => msg.acknowledge() }
  (.dispatch co (ioes-reify-event co msg)))

(defmethod comp-configure :czc.tardis.io/JMS
  [^comzotohcljc.tardis.core.sys.Element co cfg]
  (let [ pkey (:hhh.pkey cfg) ]
    (.setAttr! co :contextFactory (:contextfactory cfg))
    (.setAttr! co :connFactory (:connfactory cfg))
    (.setAttr! co :jndiUser (:jndiuser cfg))
    (.setAttr! co :jndiPwd (pwdify (:jndipwd cfg) pkey))
    (.setAttr! co :jmsUser (:jmsuser cfg))
    (.setAttr! co :jmsPwd (pwdify (:jmspwd cfg) pkey))
    (.setAttr! co :durable (:durable cfg))
    (.setAttr! co :providerUrl (:providerurl cfg))
    (.setAttr! co :destination (:destination cfg))
    co))

(defn- inizFac ^Connection [^comzotohcljc.tardis.core.sys.Element co
                ^InitialContext ctx
                ^ConnectionFactory cf]

  (let [ ^String des (.getAttr co :destination)
         c (.lookup ctx des)
         ju (.getAttr co :jmsUser)
         jp (nsb (.getAttr co :jmsPwd))
         ^Connection conn (if (hgl? ju)
                (.createConnection cf ju (if (hgl? jp) jp nil))
                (.createConnection cf)) ]

    (if (instance? Destination c)
      ;;TODO ? ack always ?
      (->
        (.createSession conn false Session/CLIENT_ACKNOWLEDGE)
        (.createConsumer c)
        (.setMessageListener (reify MessageListener
          (onMessage [_ m] (onMessage co m)))))
      (throw (IOException. "Object not of Destination type.")))

    conn))

(defn- inizTopic ^Connection [^comzotohcljc.tardis.core.sys.Element co
                  ^InitialContext ctx
                  ^TopicConnectionFactory cf]

  (let [ ^String jp (nsb (.getAttr co :jmsPwd))
         ^String des (.getAttr co :destination)
         ^String ju (.getAttr co :jmsUser)
         conn (if (hgl? ju)
                (.createTopicConnection cf ju (if (hgl? jp) jp nil))
                (.createTopicConnection cf))
         s (.createTopicSession conn false Session/CLIENT_ACKNOWLEDGE)
         t (.lookup ctx des) ]

    (when-not (instance? Topic t)
      (throw (IOException. "Object not of Topic type.")))

    (-> (if (.getAttr co :durable)
          (.createDurableSubscriber s t (uid))
          (.createSubscriber s t))
      (.setMessageListener (reify MessageListener
                              (onMessage [_ m] (onMessage co m))) ))
    conn))


(defn- inizQueue ^Connection [^comzotohcljc.tardis.core.sys.Element co
                  ^InitialContext ctx
                  ^QueueConnectionFactory cf]

  (let [ ^String jp (nsb (.getAttr co :jmsPwd))
         ^String des (.getAttr co :destination)
         ^String ju (.getAttr co :jmsUser)
         conn (if (hgl? ju)
                (.createQueueConnection cf ju (if (hgl? jp) jp nil))
                (.createQueueConnection cf))
         s (.createQueueSession conn false Session/CLIENT_ACKNOWLEDGE)
         q (.lookup ctx des) ]

    (when-not (instance? Queue q)
      (throw (IOException. "Object not of Queue type.")))

    (-> (.createReceiver s ^Queue q)
        (.setMessageListener (reify MessageListener
              (onMessage [_ m] (onMessage co m)))))
    conn))


(defmethod ioes-start :czc.tardis.io/JMS
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^String cf (.getAttr co :contextFactory)
         pl (.getAttr co :providerUrl)
         ^String ju (.getAttr co :jndiUser)
         ^String jp (nsb (.getAttr co :jndiPwd))
         vars (Hashtable.) ]

    (when (hgl? cf)
      (.put vars Context/INITIAL_CONTEXT_FACTORY cf))

    (when (hgl? pl)
      (.put vars Context/PROVIDER_URL pl))

    (when (hgl? ju)
      (.put vars "jndi.user" ju)
      (when (hgl? jp)
        (.put vars "jndi.password" jp)))

    (let [ ctx (InitialContext. vars)
           obj (.lookup ctx ^String (.getAttr co :connFactory))
           c (cond
               (instance? QueueConnectionFactory obj)
               (inizQueue co ctx obj)

               (instance? TopicConnectionFactory obj)
               (inizTopic co ctx obj)

               (instance? ConnectionFactory obj)
               (inizFac co ctx obj)

               :else
               nil) ]
      (when (nil? c)
        (throw (IOException. "Unsupported JMS Connection Factory")) )

      (.setAttr! co :conn c)
      (.start c)
      (ioes-started co))))


(defmethod ioes-stop :czc.tardis.io/JMS
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^Connection c (.getAttr co :conn) ]
    (when-not (nil? c)
      (TryC (.close c)))
    (.setAttr! co :conn nil)
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private jms-eof nil)

