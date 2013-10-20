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

(ns ^{ :doc "OS Process related utilities." 
       :author "kenl" }

  comzotohcljc.util.process)

(import '(java.lang.management ManagementFactory))
(import '(com.zotoh.frwk.util CoreUtils))
(import '(java.lang Thread Runnable))

(use '[ comzotohcljc.util.meta :only [get-cldr] ])
(use '[ comzotohcljc.util.core :only [Try!] ])
(use '[ comzotohcljc.util.str :only [nsb] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn async-exec "Run the code (runnable) in a separate daemon thread."
  ([^Runnable runable] (async-exec runable (get-cldr)))
  ([^Runnable runable ^ClassLoader cl]
    (if (nil? runable)
      nil
      (doto (Thread. runable)
        (.setContextClassLoader cl)
        (.setDaemon true)
        (.start))) ))

(defn coroutine "Run this function asynchronously."
  ([func] (coroutine func nil))
  ([func cl]
    (let [ r (reify Runnable
               (run [_] (when (fn? func) (func)))) ]
      (async-exec r cl))))

(defn safe-wait "Block current thread for some millisecs."
  [millisecs]
  (Try!
    (when (> millisecs 0) (Thread/sleep millisecs))
    ))

(defn pid "Get the current process pid."
  ^String []
  (let [ ss (.split (nsb (.getName (ManagementFactory/getRuntimeMXBean))) "@") ]
    (if (or (nil? ss) (empty ss)) "" (first ss))) )



(def ^:private process-eof nil)

