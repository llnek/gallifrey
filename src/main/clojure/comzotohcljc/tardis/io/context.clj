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

  comzotohcljc.tardis.io.context

  (:gen-class
    :name comzotohcljc.tardis.io.WebContext
    :extends javax.servlet.ServletContextListener
    :init myInit
    :constructors {[] []}
    :state myState
  ))

(import '(javax.servlet ServletContextListener ServletContext ServletContextEvent))
(import '(java.io File))
(import '(com.zotoh.gallifrey.core Container))
(import '(com.zotoh.gallifrey.io Emitter))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [TryC] ])
(use '[comzotohcljc.util.str :only [nsb] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! *warn-on-reflection* false)

(defn- inizAsJ2EE [^ServletContext ctx ^String ctxPath]
  (let [ webinf (File. (.getRealPath ctx "/WEB-INF/"))
         root (.getParentFile webinf) ]
    nil))

(defn -contextInitialized [_ ^ServletContextEvent evt]
  (let [ x (.getServletContext evt)
         m (.getMajorVersion x)
         n (.getMinorVersion x)
         ctx   (if (or (> m 2) (and (= m 2)(> n 4)))
                   (.getContextPath x)
                   nil) ]
    (debug "WEBContextListener: contextInitialized()")
    (TryC
        (inizAsJ2EE x (nsb ctx)) ) ))

(defn -contextDestroyed [this e]
  (let [ state (.myState this)
         ^Emitter src @state ]
    (debug "WEBContextListener: contextDestroyed()")
    (reset! state nil)
    (when-not (nil? src)
      (-> src (.container) (.dispose )))))

(defn -myInit []
  ([] (atom nil)))


(def ^:private context-eof nil)


