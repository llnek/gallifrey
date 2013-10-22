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

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj conv-long
                                     make-mmap bytesify] ])
(use '[comzotohcljc.crypto.core :only [gen-mac] ])
(use '[comzotohcljc.util.str :only [nsb hgl? add-delim!] ])
(use '[comzotohcljc.util.guids :only [new-uuid] ])

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
  (getCreationTime [_]  )
  (getId [_] )
  (getLastAccessedTime [_] )
  (getMaxInactiveInterval [_] ))


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


(defn- resurrect [^comzotohcljc.tardis.io.ios.WebSession mvs
                  ^HTTPEvent evt]
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
                     netty :cacheMaxAgeSecs) ]
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
  (let [ now (System/currentTimeMillis)
         attrs (make-mmap)
         impl (make-mmap) ]
    (.mm-s impl SSID_FLAG (new-uuid))
    (.mm-s impl :createTS now)
    (.mm-s impl :lastTS now)
    (.mm-s impl :valid false)
    (.mm-s impl :maxIdleSecs 3600)
    (.mm-s impl :newOne true)
    (with-meta
      (reify

        WebSession
          (setAttribute [_ k v] (.mm-s attrs k v) )
          (getAttribute [_ k] (.mm-g attrs k) )
          (removeAttribute [_ k] (.mm-r attrs k) )
          (clear [_] (.mm-c attrs))
          (listAttributes [_] (.mm-m* attrs))
          (setMaxInactiveInterval [_ idleSecs]
            (.mm-s impl
                   :maxInactiveInterval
                   (if (and (number? idleSecs)(> idleSecs 0)) idleSecs -1)))
          (isNew [_] (.mm-g impl :newOne))
          (isSSL [_] ssl)
          (invalidate [_]
            (.mm-s impl :createTS 0)
            (.mm-s impl :valid false)
            (.mm-s impl :newOne true)
            (.mm-c impl)
            (.mm-c attrs) )
          (getCreationTime [_]  (.mm-g impl :createTS))
          (getId [_] (.mm-g impl SSID_FLAG))
          (getLastAccessedTime [_] (.mm-g impl :lastTS))
          (getMaxInactiveInterval [_] (* 1000 (.mm-g impl :maxIdleSecs)))

        IOSession
          (getImpl [_] nil)
          (handleResult [_ evt res] nil)
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.tardis.io/WebSession } )))


(defn make-ws-session [co ssl] (make-session co ssl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ios-eof nil)

