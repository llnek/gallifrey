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


(ns testcljc.util.metautils)

(use '[clojure.test])
(require '[comzotohcljc.util.meta :as MU])


(deftest testutil-metautils

(is (true? (MU/is-child (Class/forName "java.lang.Number") (Class/forName "java.lang.Integer"))))
(is (true? (MU/is-child (Class/forName "java.lang.Number") (Integer. 3))))
(is (identical? (MU/bytes-class) (class (byte-array 0))))
(is (identical? (MU/chars-class) (class (char-array 0))))

(is (true? (MU/is-boolean? (class (boolean true)))))
(is (true? (MU/is-char? (class (char 3)))))
(is (true? (MU/is-int? (class (int 3)))))
(is (true? (MU/is-long? (class (long 3)))))
(is (true? (MU/is-float? (class (float 3.2)))))
(is (true? (MU/is-double? (class (double 3.2)))))
(is (true? (MU/is-byte? (class (aget (byte-array 1) 0)))))
(is (true? (MU/is-short? (class (short 3)))))
(is (true? (MU/is-string? (class ""))))
(is (true? (MU/is-bytes? (class (byte-array 0)))))

(is (not (nil? (MU/for-name "java.lang.String"))))
(is (not (nil? (MU/get-cldr))))

(is (true? (do (MU/set-cldr (MU/get-cldr)) true)))

(is (not (nil? (MU/load-class "java.lang.String"))))

(is (= "" (MU/make-obj "java.lang.String")))

(is (= 1 (count (MU/list-parents (Class/forName "java.lang.String")))))

(is (> (count (MU/list-methods (Class/forName "java.lang.String"))) 40))
(is (> (count (MU/list-fields (Class/forName "java.lang.String"))) 5))


)

(def ^:private metautils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.metautils)

