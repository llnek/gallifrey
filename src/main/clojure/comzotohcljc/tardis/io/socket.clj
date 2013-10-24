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

  comzotohcljc.tardis.io.socket)

(import '(java.net InetAddress ServerSocket Socket))
(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.core Identifiable))
(import '(com.zotoh.gallifrey.io SocketEvent))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.tardis.io.core])
(use '[comzotohcljc.tardis.core.sys])

(use '[comzotohcljc.util.process :only [coroutine] ])
(use '[comzotohcljc.util.meta :only [get-cldr] ])
(use '[comzotohcljc.util.core :only [test-posnum conv-long] ])
(use '[comzotohcljc.util.str :only [nsb hgl?] ])
(use '[comzotohcljc.util.seqnum :only [next-long] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn makeSocketIO "" [container]
  (makeEmitter container :czc.tardis.io/SocketIO))

(defmethod ioes-reify-event :czc.tardis.io/SocketIO
  [co & args]
  (let [ ^Socket soc (first args)
         eeid (next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        SocketEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (getSockOut [_] (.getOutputStream soc))
        (getSockIn [_] (.getInputStream soc))
        (emitter [_] co)
        (dispose [_] (IOUtils/closeQuietly soc)))

      { :typeid :czc.tardis.io/SocketEvent } )))

(defmethod comp-configure :czc.tardis.io/SocketIO
  [^comzotohcljc.tardis.core.sys.Element co cfg]
  (let [ tout (:sock-timeout-millis cfg)
         host (:host cfg)
         port (:port cfg)
         blog (:backlog cfg) ]
    (test-posnum "socket-io port" port)
    (.setAttr! co :timeoutMillis (conv-long tout 0))
    (.setAttr! co :host (nsb host))
    (.setAttr! co :port port)
    (.setAttr! co :backlog (conv-long blog 100))
    co))


(defmethod comp-initialize :czc.tardis.io/SocketIO
  [^comzotohcljc.tardis.core.sys.Element co]

  (let [ backlog (.getAttr co :backlog)
         host (.getAttr co :host)
         port (.getAttr co :port)
         ip (if (hgl? host) (InetAddress/getByName host) (InetAddress/getLocalHost))
         soc (ServerSocket. port backlog ip) ]
    (info "opened Server Socket " soc  " (bound?) " (.isBound soc))
    (doto soc (.setReuseAddress true))
    (.setAttr! co :ssocket soc)))

(defn- sockItDown [^comzotohcljc.tardis.io.core.EmitterAPI co ^Socket soc]
  (let []
    (.dispatch co (ioes-reify-event co soc) {} )))

(defmethod ioes-start :czc.tardis.io/SocketIO
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket)
         cl (get-cldr) ]
    (when-not (nil? ssoc)
      (coroutine (fn [] 
                   (while (.isBound ssoc)
                     (try
                       (sockItDown co (.accept ssoc))
                       (catch Throwable e#
                         (warn e# "")
                         (IOUtils/closeQuietly ssoc)
                         (.setAttr! co :ssocket nil)))))
                 cl))
    (ioes-started co)))

(defmethod ioes-stop :czc.tardis.io/SocketIO
  [^comzotohcljc.tardis.core.sys.Element co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket) ]
    (IOUtils/closeQuietly ssoc)
    (.setAttr! co :ssocket nil)
    (ioes-stopped co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private socket-eof nil)

