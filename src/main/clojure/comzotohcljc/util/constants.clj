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

  comzotohcljc.util.constants)

(def TS_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])\\s\\d\\d:\\d\\d:\\d\\d")
(def DT_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")

(def TS_FMT_NANO "yyyy-MM-dd HH:mm:ss.fffffffff" )
(def TS_FMT "yyyy-MM-dd HH:mm:ss")

(def DT_FMT_MICRO "yyyy-MM-dd'T'HH:mm:ss.SSS" )
(def DT_FMT "yyyy-MM-dd'T'HH:mm:ss" )
(def DATE_FMT "yyyy-MM-dd" )

(def ISO8601_FMT "yyyy-MM-dd'T'HH:mm:ss.SSSZ" )

(def USASCII "ISO-8859-1" )
(def UTF16 "UTF-16" )
(def UTF8 "UTF-8" )
(def SLASH   "/" )
(def PATHSEP   SLASH )

(def BOOLS #{ "true", "yes", "on", "ok", "active", "1"} )

(def MONTHS [ "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC" ] )


(def COPYRIGHT "Copyright (c) 2013 Cherimoia, LLC. All rights reserved.")




(def ^:private constants-eof nil)


