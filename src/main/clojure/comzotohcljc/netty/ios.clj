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
  comzotohcljc.netty.ios )


(import '(org.apache.commons.lang3 StringUtils))

(import '(com.zotoh.frwk.util CoreUtils))
(import '(java.net URLDecoder))
(import '(com.zotoh.gallifrey.io HTTPEvent IOSession Emitter))
(import '(com.zotoh.gallifrey.core Container))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj conv-long make-mmap bytesify] ])
(use '[comzotohcljc.crypto.core :only [gen-mac] ])
(use '[comzotohcljc.util.str :only [nsb hgl?] ])
(use '[comzotohcljc.util.guids :only [new-uuid] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(def ^:private SESSION_COOKIE "h3_ssid" )
(def ^:private SSID_FLAG "f_01ec")
(def ^:private TS_FLAG "f_684f" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol WebSession
  ""
  (setAttribute [_ k v] )
  (getAttribute [_ k] )
  (removeAttribute [_ k] )
  (clear [_] )
  (setMaxInactiveInterval [_ idleSecs] )
  (isNew [_] )
  (invalidate [_] )
  (getCreationTime [_]  )
  (getId [_] )
  (getLastAccessedTime [_] )
  (getMaxInactiveInterval [_] ))

(defn- resurrect [^comzotohcljc.netty.ios.WebSession mvs
                  ^HTTPEvent evt]
  (let [ ctr (.container ^Emitter (.emitter evt))
         pkey (-> ctr (.getAppKey)(bytesify))
         ck (.getCookie evt SESSION_COOKIE)
         cookie (nsb (if-not (nil? ck) (.getValue ck)))
         pos (.indexOf cookie (int \-))
         [rc1 rc2] (if (< pos 0)
                     ["" ""]
                     [(.substring cookie 0 pos)
                      (.substring cookie (+ pos 1) )] ) ]
    (when (and (hgl? rc1)
               (hgl? rc2)
               (= (gen-mac pkey rc2) rc1))
      (let [ ss (CoreUtils/splitNull (URLDecoder/decode rc2 "utf-8")) ]
        (doseq [ s (seq ss) ]
          (let [ [n v] (StringUtils/split ^String s ":") ]
            (.setAttribute mvs n v)))))
    (let [ ts (nsb (.getAttribute mvs TS_FLAG))
           ^comzotohcljc.tardis.core.sys.Element
           netty (.emitter evt)
           idleSecs (.getAttr netty :cacheMaxAgeSecs)
           expired (if (hgl? ts)
                     (< (conv-long ts 0) (System/currentTimeMillis))
                     (and (number? idleSecs) (> idleSecs 0))) ]
      (if (and expired (number? idleSecs) (> idleSecs 0))
        (.setAttribute mvs TS_FLAG (+ (System/currentTimeMillis)
                                      (* idleSecs 1000)))))

    (.bindSession evt mvs)
    mvs))

(defn make-ws-session []
  (let [ now (System/currentTimeMillis)
         impl (make-mmap) ]
    (.mm-s impl SSID_FLAG (new-uuid))
    (.mm-s impl :createTS now)
    (.mm-s impl :lastTS now)
    (.mm-s impl :valid false)
    (.mm-s impl :maxIdleSecs 3600)
    (.mm-s impl :newOne true)
    (with-meta
      (reify
        IOSession
          (getImpl [_] nil)
          (handleResult [_ evt res] nil)
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.tardis.io/WebSockSession } )))

(defn make-session []
  (let [ now (System/currentTimeMillis)
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
          (setAttribute [_ k v] (.mm-s impl k v) )
          (getAttribute [_ k] (.mm-g impl k) )
          (removeAttribute [_ k] (.mm-r impl k) )
          (clear [_] (.mm-c impl))
          (setMaxInactiveInterval [_ idleSecs]
            (.mm-s impl
                   :maxInactiveInterval
                   (if (and (number? idleSecs)(> idleSecs 0)) idleSecs -1)))
          (isNew [_] (.mm-g impl :newOne))
          (invalidate [_]
            (.mm-s impl :createTS 0)
            (.mm-s impl :valid false)
            (.mm-s impl :newOne true)
            (.mm-c impl) )
          (getCreationTime [_]  (.mm-g impl :createTS))
          (getId [_] (.mm-g impl SSID_FLAG))
          (getLastAccessedTime [_] (.mm-g impl :lastTS))
          (getMaxInactiveInterval [_] (* 1000 (.mm-g impl :maxIdleSecs)))

        IOSession
          (getImpl [_] nil)
          (handleResult [_ evt res] nil)
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.tardis.io/HttpSession } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ios-eof nil)

