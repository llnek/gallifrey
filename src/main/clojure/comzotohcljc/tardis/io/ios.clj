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

  comzotohcljc.tardis.io.ios )

(import '(com.zotoh.gallifrey.runtime AuthError))
(import '(org.apache.commons.lang3 StringUtils))

(import '(com.zotoh.frwk.util CoreUtils))
(import '(java.net HttpCookie URLDecoder URLEncoder))
(import '(com.zotoh.gallifrey.io HTTPResult HTTPEvent IOSession Emitter))
(import '(com.zotoh.gallifrey.core Container))
(import '(com.zotoh.frwk.net ULFormItems ULFileItem))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj conv-long notnil?
                                     make-mmap bytesify] ])
(use '[comzotohcljc.crypto.core :only [gen-mac] ])
(use '[comzotohcljc.util.str :only [nsb hgl? add-delim!] ])
(use '[comzotohcljc.util.guids :only [new-uuid] ])
(use '[comzotohcljc.tardis.io.http :only [scanBasicAuth] ])
(use '[comzotohcljc.net.comms :only [getFormFields] ])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private SESSION_COOKIE "gallifrey" )
(def ^:private SSID_FLAG "f_01ec")
(def ^:private TS_FLAG "f_684f" )
(def ^:private NV_SEP "\u0000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol WebSession
  ""
  (setAttribute [_ k v] )
  (getAttribute [_ k] )
  (removeAttribute [_ k] )
  (clear [_] )
  (listAttributes [_] )
  (setMaxInactiveInterval [_ idleSecs] )
  (isNew [_] )
  (isSSL [_] )
  (invalidate [_] )
  (yield [_] )
  (getCreationTime [_]  )
  (getId [_] )
  (getLastAccessedTime [_] )
  (getMaxInactiveInterval [_] ))


(defn getSignupInfo "" [^HTTPEvent evt]
  (let [ data (.data evt) ]
    (with-local-vars [user nil pwd nil email nil]
      (cond
        (instance? ULFormItems data)
        (doseq [ ^ULFileItem x (getFormFields data) ]
          (debug "Form field: " (.getFieldName x) " = " (.getString x))
          (case (.getFieldName x)
            "password" (var-set pwd  (.getString x))
            "user" (var-set user (.getString x))
            "email" (var-set email (.getString x))
            nil))

        :else
        (do
          (var-set pwd (.getParameterValue evt "password"))
          (var-set email (.getParameterValue evt "email"))
          (var-set user (.getParameterValue evt "user"))) )

      { :principal @user :credential @pwd  :email @email } )))


(defn getLoginInfo "" [^HTTPEvent evt]
  (let [ ba (scanBasicAuth evt)
         data (.data evt) ]
    (with-local-vars [user nil pwd nil]
      (cond
        (instance? ULFormItems data)
        (doseq [ ^ULFileItem x (getFormFields data) ]
          (debug "Form field: " (.getFieldName x) " = " (.getString x))
          (case (.getFieldName x)
            "password" (var-set pwd  (.getString x))
            "user" (var-set user (.getString x))
            nil))

        (notnil? ba)
        (do
          (var-set user (first ba))
          (var-set pwd (last ba)))

        :else
        (do
          (var-set pwd (.getParameterValue evt "password"))
          (var-set user (.getParameterValue evt "user"))) )

      { :principal @user :credential @pwd } )))

(defn refashion "" [^HTTPEvent evt acctObj roles]
  (let [ ^comzotohcljc.tardis.io.ios.WebSession mvs (.getSession evt)
         ^comzotohcljc.tardis.core.sys.Element netty (.emitter evt)
         idleSecs (.getAttr netty :cacheMaxAgeSecs) ]
    (doto mvs
      (.invalidate )
      (.setAttribute TS_FLAG
                     (+ (System/currentTimeMillis)
                        (* idleSecs 1000)))
      (.setAttribute SSID_FLAG (new-uuid))
      (.yield ))))


(defn- hibernate "" [^comzotohcljc.tardis.io.ios.WebSession mvs
                    ^HTTPResult res]
  (let [ ctr (.container ^Emitter (.emitter res))
         pkey (-> ctr (.getAppKey)(bytesify))
         s (reduce (fn [sum en]
                     (add-delim! sum NV_SEP (str (first en) ":" (last en))))
                   (StringBuilder.)
                   (seq (.listAttributes mvs)))
         data (URLEncoder/encode (nsb s) "utf-8")
         idleSecs (.getMaxInactiveInterval mvs)
         mac (gen-mac pkey data)
         ck (HttpCookie. SESSION_COOKIE (str mac "-" data)) ]
    (doto ck
      (.setSecure (.isSSL mvs))
      (.setHttpOnly true)
      (.setPath "/"))
    (when (> idleSecs 0)
      (.setMaxAge ck idleSecs))
    (.addCookie res ck)
    ))


(defn- resurrect [ ^HTTPEvent evt ]
  (let [ ^Emitter netty (.emitter evt)
         ctr (.container netty)
         pkey (-> ctr (.getAppKey)(bytesify))
         ck (.getCookie evt SESSION_COOKIE)
         cookie (nsb (if-not (nil? ck) (.getValue ck)))
         pos (.indexOf cookie (int \-))
         [rc1 rc2] (if (< pos 0)
                     ["" ""]
                     [(.substring cookie 0 pos)
                      (.substring cookie (+ pos 1) )] ) ]

    (when-not (and (hgl? rc1)
                   (hgl? rc2)
                   (= (gen-mac pkey rc2) rc1))
      (error "Session token - broken.")
      (throw (AuthError. "Bad Session.")))

    (let [ ss (CoreUtils/splitNull (URLDecoder/decode rc2 "utf-8"))
           idleSecs (.getAttr
                     ^comzotohcljc.tardis.core.sys.Element
                     netty :cacheMaxAgeSecs)
           ^comzotohcljc.tardis.io.ios.WebSession mvs (.getSession evt) ]
      (doseq [ ^String s (seq ss) ]
          (let [ [n v] (StringUtils/split s ":") ]
            (.setAttribute mvs n v)))
      (let [ ts (conv-long (nsb (.getAttribute mvs TS_FLAG)) -1) ]
        (if (or (< ts 0)
                (< ts (System/currentTimeMillis)))
          (throw (AuthError. "Expired Session.")))
        (.setAttribute mvs
                       TS_FLAG
                       (+ (System/currentTimeMillis)
                          (* idleSecs 1000))))
      )))

(defn make-session [co ssl]
  (let [ impl (make-mmap)
         fc (fn []
              (.mm-s impl :maxIdleSecs 3600)
              (.mm-s impl :valid false)
              (.mm-s impl :createTS  0)
              (.mm-s impl :lastTS  0)
              (.mm-s impl :newOne true)) ]

    (with-meta
      (reify

        WebSession
          (setAttribute [_ k v] (.mm-s impl k v) )
          (getAttribute [_ k] (.mm-g impl k) )
          (removeAttribute [_ k] (.mm-r impl k) )
          (clear [_] (.mm-c impl))
          (listAttributes [_] (.mm-m* impl))

          (setMaxInactiveInterval [_ idleSecs]
            (when (and (number? idleSecs)(> idleSecs 0))
              (.mm-s impl
                   :maxIdleSecs idleSecs)))

          (isNew [_] (.mm-g impl :newOne))
          (isSSL [_] ssl)

          (invalidate [_] (.mm-c impl) (fc))
          (yield [_]
            (.mm-s impl :createTS  (System/currentTimeMillis))
            (.mm-s impl :lastTS  (System/currentTimeMillis))
            (.mm-s impl :valid true)
            (.mm-s impl :newOne true))

          (getCreationTime [_]  (.mm-g impl :createTS))
          (getId [_] (.mm-g impl SSID_FLAG))
          (getLastAccessedTime [_] (.mm-g impl :lastTS))
          (getMaxInactiveInterval [_] (.mm-g impl :maxIdleSecs))

        IOSession
          (getImpl [_] nil)
          (handleResult [_ evt res] nil)
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.tardis.io/WebSession } )))


(defn make-ws-session [co ssl] (make-session co ssl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ios-eof nil)

