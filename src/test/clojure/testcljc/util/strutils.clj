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


(ns testcljc.util.strutils)

(use '[clojure.test])
(require '[comzotohcljc.util.str :as SU])


(deftest testutil-strutils

(is (false? (SU/has? "hallowed are the ori" \z)))
(is (true? (SU/has? "hallowed are the ori" \w)))

(is (= "heeloo" (SU/nsb "heeloo")))
(is (= "" (SU/nsb nil)))

(is (= "heeloo" (SU/nsn "heeloo")))
(is (= "(null)" (SU/nsn nil)))

(is (false? (SU/same? "aaa" "axa")))
(is (true? (SU/same? "aaa" "aaa")))

(is (true? (SU/hgl? "haha")))
(is (false? (SU/hgl? "")))

(is (= "haha" (SU/strim "            haha                          ")))
(is (= "" (SU/strim nil)))

(is (= "joe;blogg" (let [ x (StringBuilder.) ]
                (SU/add-delim! x ";" "joe")
                (SU/add-delim! x ";" "blogg")
                (.toString x))))

(is (= 4 (count (SU/splunk "hello, how are you" 5))))

(is (true? (SU/hasic-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))
(is (false? (SU/has-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))

(is (true? (SU/swic-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "Hall" ])))
(is (true? (SU/sw-any? "hallowed are the ori" [ "sdfsdg" "jffflf" "ha" ])))
(is (false? (SU/sw-any? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

(is (true? (SU/eqic-any? "heeloo" [ "sdfsdg" "jffflf" "HeeLoo" ])))
(is (true? (SU/eq-any? "heeloo" [ "sdfsdg" "jffflf" "heeloo" ])))
(is (false? (SU/eq-any? "heeloo" [ "sdfsdg" "jffflf" ])))

(is (= 10 (.length (SU/make-string \x 10))))
(is (= "ori" (SU/right "Hallowed are the ori" 3)))
(is (= "Hal" (SU/left "Hallowed are the ori" 3)))










)

(def ^:private strutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.strutils)

