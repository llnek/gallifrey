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


(ns testcljc.i18n.i18nstuff)

(use '[clojure.test])
(require '[comzotohcljc.i18n.resources :as NU])
(require '[comzotohcljc.util.core :as CU])


(deftest testi18n-i18nstuff

(is (= "hello joe, how is your dawg"
       (let [ rs (NU/load-resource (CU/rc-url "com/zotoh/frwk/i18n/Resources_en.properties")) ]
           (NU/get-string rs "test" [ "joe", "dawg" ]))))










)

(def ^:private i18nstuff-eof nil)

;;(clojure.test/run-tests 'testcljc.i18n.i18nstuff)

