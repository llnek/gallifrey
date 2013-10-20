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
  comzotohcljc.net.rts )

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io File))
(import '(jregex Matcher Pattern))
(import '(java.util StringTokenizer))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj make-mmap test-nestr] ])
(use '[comzotohcljc.util.str :only [nsb nichts? hgl?] ])
(use '[comzotohcljc.util.ini :only [parse-inifile] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol RouteInfo
  ""
  (getHandler [_] )
  (getPath [_] )
  (isStatic? [_] )
  (getVerbs [_] )
  (resemble? [_ mtd path] )
  (collect [_ matcher] ))


(defn- make-route-info "" [route ^String verb handler]
  (let [ verbList (.toUpperCase verb)
         impl (make-mmap) ]
    (with-meta
      (reify

        MuObj

        (setf! [_ k v] (.mm-s impl k v) )
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k) )
        (clrf! [_ k] (.mm-r impl k) )
        (clear! [_] (.mm-c impl))

        RouteInfo

        (isStatic? [_] (.mm-g impl :static))
        (getHandler [_] handler)
        (getPath [_] route)
        (getVerbs [_] verbList)

        (resemble? [_ mtd path]
          (let [ rg (.mm-g impl :regex)
                 m (.matcher ^Pattern rg path) ]
            (if (and (.matches m)
                     (or (= "*" verbList)
                         (>= (.indexOf verbList
                                       (.toUpperCase ^String mtd)) 0)))
              m
              nil)))

        (collect [_ mc]
          (let [ ph (.mm-g impl :placeHolders)
                 ^Matcher mmc mc
                 gc (.groupCount mmc) ]
            (with-local-vars [ rc (transient {}) r2 "" ]
              (doseq [ h (seq ph) ]
                (var-set r2 (last h))
                (var-set rc
                         (assoc! rc
                                 @r2
                                 (nsb (.group mmc ^String @r2)))))
              (persistent! @rc)))) )

      { :typeid :czc.net/RouteInfo } )) )

(defn- initRoute ""

  [^comzotohcljc.util.core.MuObj rc
   ^String path]

  (let [ tknz (StringTokenizer. path "/" true)
         buff (StringBuilder.) ]
    (with-local-vars [ cg 0 gn "" ts "" phs (transient []) ]
      (while (.hasMoreTokens tknz)
        (var-set ts (.nextToken tknz))
        (if (= @ts "/")
          (.append buff "/")
          (do
            (if (.startsWith ^String @ts ":")
              (do
                (var-set gn (.substring ^String @ts 1))
                (var-set cg (inc @cg))
                (var-set phs (conj! @phs [ @cg @gn ] ))
                (var-set ts  (str "({" @gn "}[^/]+)")))
              (let [ c (StringUtils/countMatches @ts "(") ]
                (if (> c 0)
                  (var-set cg (+ @cg c)))))
            (.append buff @ts))))
      (let [ pp (.toString buff) ]
        (info "route added: " path " \ncanonicalized to: " pp)
        (.setf! rc :regex (Pattern. pp))
        (.setf! rc :path pp))
      (.setf! rc :placeHolders (persistent! @phs))
      rc)) )

(defn- mkRoute "" [stat path
                   ^comzotohcljc.util.ini.IWin32Conf cfile]
  (let [ tpl (.optString cfile path :template "")
         verb (.optString cfile path :verb "")
         mpt (.optString cfile path :mount "")
         pipe (.optString cfile path :pipe "")
         ^comzotohcljc.util.core.MuObj
         rc (make-route-info
              path
              (if (and stat (nichts? verb)) "GET" verb)
              pipe) ]
    (if stat
      (do
        (.setf! rc :mountPoint mpt)
        (.setf! rc :static true)
        (test-nestr "static-route mount point" mpt))
      (do
        (test-nestr "http method for route" verb)
        (test-nestr "pipeline for route" pipe)))
    (when (hgl? tpl)
      (.setf! rc :template tpl))
    (initRoute rc path)
    rc))

;;
;; path can be /host.com/abc/:id1/gg/:id2
;;
(defn load-routes "" [^File file]
  (let [ stat  (-> file (.getName)(.startsWith "static-"))
         cf (parse-inifile file) ]
    (with-local-vars [rc (transient []) ]
      (doseq [ s (seq (.sectionKeys cf)) ]
        (var-set rc (conj! @rc (mkRoute stat s cf))))
      (persistent! @rc))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private rts-eof nil)



