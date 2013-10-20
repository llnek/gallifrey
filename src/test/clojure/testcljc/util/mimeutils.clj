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


(ns testcljc.util.mimeutils)

(use '[clojure.test])
(import '(java.io File InputStream))
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.util.mime :as MU])


(eval '(MU/setup-cache (CU/rc-url "com/zotoh/frwk/mime/mime.properties")))


(deftest testutil-mimeutils

(is (= "utf-16" (MU/get-charset "text/plain; charset=utf-16")))

(is (true? (MU/is-signed? "saljas application/x-pkcs7-mime laslasdf lksalfkla multipart/signed signed-data ")))
(is (true? (MU/is-encrypted? "saljas laslasdf lksalfkla application/x-pkcs7-mime  enveloped-data ")))
(is (true? (MU/is-compressed? "saljas laslasdf lksalfkla application/pkcs7-mime compressed-data")))
(is (true? (MU/is-mdn? "saljas laslasdf lksalfkla multipart/report   disposition-notification    ")))

(is (instance? InputStream (MU/maybe-stream (IO/streamify (CU/bytesify "hello")))))
(is (instance? InputStream (MU/maybe-stream (CU/bytesify "hello"))))
(is (instance? InputStream (MU/maybe-stream "hello")))
(is (not (instance? InputStream (MU/maybe-stream 3))))

(is (= "a b" (MU/url-decode (MU/url-encode "a b"))))

(is (>= (.indexOf (MU/guess-mimetype (File. "/tmp/abc.jpeg")) "image/") 0))
(is (> (.indexOf (MU/guess-contenttype (File. "/tmp/abc.pdf")) "/pdf") 0))





)

(def ^:private mimeutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.mimeutils)

