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


(ns testcljc.util.byteutils)

(use '[clojure.test])
(import '(java.nio.charset Charset))
(require '[comzotohcljc.util.bytes :as BU])


(def ^:private CS_UTF8 "utf-8")

(deftest testutils-byteutils

(is (= "heeloo" (String. (BU/to-chars (BU/to-bytes (.toCharArray "heeloo") CS_UTF8) CS_UTF8))))

(is (= 4 (alength ^bytes (BU/write-bytes (Integer/MAX_VALUE)))))
(is (= 8 (alength ^bytes (BU/write-bytes (Long/MAX_VALUE)))))

(is (= (Integer/MAX_VALUE) (BU/read-int (BU/write-bytes (Integer/MAX_VALUE)))))
(is (= (Long/MAX_VALUE) (BU/read-long (BU/write-bytes (Long/MAX_VALUE)))))

)

(def ^:private byteutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.byteutils)

