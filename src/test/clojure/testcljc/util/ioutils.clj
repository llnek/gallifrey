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


(ns testcljc.util.ioutils)

(use '[clojure.test])

(import '(org.apache.commons.io FileUtils))
(import '(java.io
  FileReader File InputStream
  OutputStream FileOutputStream))
(import '(com.zotoh.frwk.io IOUtils XData XStream))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])


(def ^:private TMP_DIR (File. (System/getProperty "java.io.tmpdir")))
(def ^:private TMP_FP (File. ^File TMP_DIR (str (CU/uid) ".txt")))
(eval '(do (FileUtils/writeStringToFile ^File TMP_FP "heeloo" "utf-8")))
;; force to use file
;;(eval '(do (IOUtils/setStreamLimit 2)))


(deftest testutil-ioutils

(is (true? (.exists (IO/make-tmpfile))))

(is (true? (let [ v (IO/newly-tmpfile) ]
              (and (.exists ^File (first v)) (nil? (nth v 1))))))

(is (true? (let [ v (IO/newly-tmpfile true)
                  rc (and (.exists ^File (first v)) (instance? OutputStream (nth v 1))) ]
             (when rc (.close ^OutputStream (nth v 1)))
             rc)))

(is (instance? InputStream (IO/streamify (byte-array 10))))

(is (instance? OutputStream (IO/make-baos)))

(is (= "616263" (IO/hexify-string (CU/bytesify "abc"))))

(is (= "heeloo world!" (CU/stringify (IO/gunzip (IO/gzip (CU/bytesify "heeloo world!"))))))

(is (true? (do (IO/reset-stream! (IO/streamify (CU/bytesify "hello"))) true)))

(is (true? (let [ xs (IO/open-file (.getCanonicalPath ^File TMP_FP))
                    rc (instance? XStream xs) ] (.close ^XStream xs) rc)))

(is (true? (let [ xs (IO/open-file ^File TMP_FP) rc (instance? XStream xs) ] (.close ^XStream xs) rc)))

(is (= "heeloo world" (CU/stringify (IO/from-gzb64 (IO/to-gzb64 (CU/bytesify "heeloo world"))))))

(is (>= (with-open [ ^InputStream inp (IO/open-file TMP_FP) ] (IO/available inp)) 6))

(is (true? (let [ ^File fp (with-open [ ^InputStream inp (IO/open-file TMP_FP) ]
                       (IO/copy-stream inp)) ]
             (.exists fp))))

(is (true? (let [ v (IO/newly-tmpfile false) ]
                (with-open [^InputStream inp (IO/open-file TMP_FP) ]
                  (with-open [ os (FileOutputStream. ^File (first v)) ]
                    (IO/copy-bytes inp os 4)))
                (>= (.length ^File (first v)) 4))))

(is (true? (.isDiskFile (IO/make-xdata true))))
(is (false? (.isDiskFile (IO/make-xdata))))

(is (true? (let [ x (with-open [ ^InputStream inp (IO/open-file TMP_FP) ] (IO/read-bytes inp true)) ]
                (and (instance? XData x) (.isDiskFile ^XData x) (> (.size ^XData x) 0))) ))

(is (true? (let [ x (with-open [ ^InputStream inp (IO/open-file TMP_FP) ] (IO/read-bytes inp)) ]
                (and (instance? XData x) (not (.isDiskFile ^XData x)) (> (.size ^XData x) 0))) ))

(is (true? (let [ x (with-open [ rdr (FileReader. ^File TMP_FP) ] (IO/read-chars rdr true)) ]
                (and (instance? XData x) (.isDiskFile ^XData x) (> (.size ^XData x) 0))) ))

(is (true? (let [ x (with-open [ rdr (FileReader. ^File TMP_FP) ] (IO/read-chars rdr)) ]
                (and (instance? XData x) (not (.isDiskFile ^XData x)) (> (.size ^XData x) 0))) ))

(is (= "heeloo" (String. (IO/morph-chars (CU/bytesify "heeloo")))))

(is (false? (with-open [ ^InputStream p1 (IO/open-file TMP_FP)]
                (with-open [ ^InputStream p2 (IO/open-file TMP_FP)] (IO/diff? p1 p2)))))

)

(def ^:private ioutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.ioutils)

