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


(ns testcljc.dbio.dbstuff)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io File))
(import '(java.util GregorianCalendar Calendar))
(import '(com.zotoh.frwk.dbio
  Transactable SQLr MetaCache DBAPI))

(require '[comzotohcljc.crypto.codec :as CE])
(require '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.connect])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.h2])
(use '[clojure.test])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel Address
  (with-db-fields {
    :addr1 { :size 200 :null false }
    :addr2 { :size 64}
    :city { :null false}
    :state {:null false}
    :zip {:null false}
    :country {:null false}
                   })
  (with-db-indexes { :i1 #{ :city :state :country }
    :i2 #{ :zip :country }
    :state #{ :state }
    :zip #{ :zip } } ))

(defmodel Person
  (with-db-fields {
    :first_name { :null false }
    :last_name { :null false }
    :iq { :domain :Int }
    :bday {:domain :Calendar :null false }
    :sex {:null false}
                   })
  (with-db-assocs {
    :spouse { :kind :O2O :rhs (dbio-scopeType "Person") }
                   })
  (with-db-indexes { :i1 #{ :first_name :last_name }
    :i2 #{ :bday }
    } ))

(defjoined EmpDepts
           (dbio-scopeType "Department")
           (dbio-scopeType "Employee"))

(defmodel Employee
  (with-db-parent-model  (dbio-scopeType "Person"))
  (with-db-fields {
    :salary { :domain :Float :null false }
    :pic { :domain :Bytes }
    :passcode { :domain :Password }
    :descr {}
    :login {:null false}
                   })
  (with-db-assocs {
    :depts { :kind :M2M :joined (dbio-scopeType "EmpDepts") }
                   })
  (with-db-indexes { :i1 #{ :login }
    } ))

(defmodel Department
  (with-db-fields {
    :dname { :null false }
                   })
  (with-db-assocs {
    :emps { :kind :M2M :joined (dbio-scopeType "EmpDepts") }
                   })
  (with-db-uniques {
    :u1 #{ :dname }
    } ))

(defmodel Company
  (with-db-fields {
    :cname { :null false }
    :revenue { :domain :Double :null false }
    :logo { :domain :Bytes }
                   })
  (with-db-assocs {
    :depts { :kind :O2M :rhs (dbio-scopeType "Department") }
    :emps { :kind :O2M :rhs (dbio-scopeType "Employee") }
    :hq { :kind :O2O :rhs (dbio-scopeType "Address") }
                   })
  (with-db-uniques {
    :u1 #{ :cname }
    } ))

(def METAC (atom nil))
(def JDBC (atom nil))
(def DB (atom nil))

(defn init-test [f]
  (reset! METAC
    (make-MetaCache (make-Schema
                      [Address Person EmpDepts Employee Department Company])))
  (let [ dir (File. (System/getProperty "java.io.tmpdir"))
         db (str "" (System/currentTimeMillis))
         url (make-h2-db dir db "sa" (CE/pwdify ""))
        jdbc (make-jdbc (CU/uid)
               { :d H2-DRIVER :url url :user "sa" :passwd "" }
               (CE/pwdify "")) ]
    (reset! JDBC jdbc)
    (upload-ddl jdbc (getDDL @METAC :h2))
    (reset! DB (dbio-connect jdbc @METAC {})))
  (if (nil? f) nil (f))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkt [t] (keyword (str "testcljc.dbio.dbstuff/" t)))


(defn- mkEmp [fname lname login]
  (let [ emp (dbio-create-obj (mkt "Employee")) ]
    (-> emp
      (dbio-set-fld :first_name fname)
      (dbio-set-fld :last_name  lname)
      (dbio-set-fld :iq 100)
      (dbio-set-fld :bday (GregorianCalendar.))
      (dbio-set-fld :sex "male")
      (dbio-set-fld :salary 1000000.00)
      (dbio-set-fld :pic (.getBytes "poo"))
      (dbio-set-fld :passcode "secret")
      (dbio-set-fld :desc "idiot")
      (dbio-set-fld :login login ))))

(defn- create-company [cname]
  (-> (dbio-create-obj :testcljc.dbio.dbstuff/Company)
    (dbio-set-fld :cname cname)
    (dbio-set-fld :revenue 100.00)
    (dbio-set-fld :logo (.getBytes "hi"))))

(defn- create-dept [dname]
  (-> (dbio-create-obj :testcljc.dbio.dbstuff/Department)
    (dbio-set-fld :dname dname)))

(defn- create-emp[fname lname login]
  (let [ obj (mkEmp fname lname login)
         ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
         o2 (.execWith sql
             (fn [^SQLr tx]
               (.insert tx obj))) ]
    o2))

(defn- fetch-all-emps []
  (let [ ^SQLr sql (.newSimpleSQLr ^DBAPI @DB)
         o1 (.findAll sql (mkt "Employee")) ]
    o1))

(defn- fetch-emp [login]
  (let [ ^SQLr sql (.newSimpleSQLr ^DBAPI @DB)
         o1 (.findOne sql (mkt "Employee") {:login login} ) ]
    o1))

(defn- change-emp [login]
  (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB) ]
    (.execWith
      sql
      (fn [^SQLr tx]
        (let [ o1 (.findOne tx (mkt "Employee") {:login login} )
               o2 (-> o1 (dbio-set-fld :salary 99.9234)
                         (dbio-set-fld :iq 0)) ]
          (.update tx o2))))))

(defn- delete-emp [login]
  (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB) ]
    (.execWith
      sql
      (fn [^SQLr tx]
        (let [ o1 (.findOne tx (mkt "Employee") {:login login} ) ]
          (.delete tx o1))))
    (.execWith
      sql
      (fn [^SQLr tx]
        (.countAll tx (mkt "Employee"))))))

(defn- create-person [fname lname]
  (let [ p (-> (dbio-create-obj (mkt "Person"))
                (dbio-set-fld :first_name fname)
                (dbio-set-fld :last_name  lname)
                (dbio-set-fld :iq 100)
                (dbio-set-fld :bday (GregorianCalendar.))
                (dbio-set-fld :sex "female"))
         ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
         o2 (.execWith sql
             (fn [^SQLr tx]
               (.insert tx p))) ]
    o2))

(defn- wedlock []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           h (create-emp "joe" "blog" "joeb")
           w (create-person "mary" "lou")

           [h1 w1] (.execWith
                     sql
                     (fn [^SQLr tx] (dbio-set-o2o {:as :spouse :with tx } h w)))
           w2 (.execWith
                sql
                (fn [^SQLr tx] (dbio-get-o2o
                           {:as :spouse
                            :cast :testcljc.dbio.dbstuff/Person
                            :with tx } h1))) ]
      (and (not (nil? h))
           (not (nil? w))
           (not (nil? w2))))))


(defn- undo-wedlock []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           h (fetch-emp "joeb")
           w (.execWith
               sql
               (fn [^SQLr tx] (dbio-get-o2o
                          { :as :spouse
                            :with tx
                            :cast :testcljc.dbio.dbstuff/Person } h)))
           h1 (.execWith
                sql
                (fn [^SQLr tx] (dbio-clr-o2o
                           {:as :spouse
                             :with tx
                             :cast :testcljc.dbio.dbstuff/Person } h)))
           w1 (.execWith
                sql
                (fn [^SQLr tx] (dbio-get-o2o
                           { :as :spouse
                             :with tx
                             :cast :testcljc.dbio.dbstuff/Person } h1))) ]
      (and
        (not (nil? h))
        (not (nil? w))
        (not (nil? h1))
        (nil? w1)))) )

(defn- test-company []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           c (.execWith
               sql
               (fn [^SQLr tx]
                 (.insert tx (create-company "acme")))) ]
      (.execWith
         sql
         (fn [^SQLr tx]
           (dbio-set-o2m {:as :depts :with tx}
                             c (.insert tx (create-dept "d1")))))
      (.execWith
         sql
         (fn [^SQLr tx]
           (dbio-add-o2m {:as :depts :with tx}
                         c
                         [ (.insert tx (create-dept "d2"))
                           (.insert tx (create-dept "d3")) ] )))
      (.execWith
        sql
        (fn [^SQLr tx]
          (dbio-add-o2m
            {:as :emps :with tx }
            c
            [ (.insert tx (mkEmp "emp1" "ln1" "e1"))
              (.insert tx (mkEmp "emp2" "ln2" "e2"))
              (.insert tx (mkEmp "emp3" "ln3" "e3")) ])))

      (let [ ds (.execWith
                  sql
                  (fn [^SQLr tx] (dbio-get-o2m  {:as :depts :with tx} c)))
             es (.execWith
                  sql
                  (fn [^SQLr tx] (dbio-get-o2m  {:as :emps :with tx} c))) ]
        (and (= (count ds) 3)
             (= (count es) 3))) )))


(defn- test-m2m []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           c (.execWith
               sql
               (fn [^SQLr tx]
                 (.findOne tx :testcljc.dbio.dbstuff/Company {:cname "acme"} )))
           ds (.execWith
                sql
                (fn [^SQLr tx] (dbio-get-o2m {:as :depts :with tx} c)))
           es (.execWith
                sql
                (fn [^SQLr tx] (dbio-get-o2m {:as :emps :with tx} c))) ]
      (.execWith
        sql
        (fn [^SQLr tx]
          (doseq [ d (seq ds) ]
            (if (= (:dname d) "d2")
              (doseq [ e (seq es) ]
                (dbio-set-m2m {:as :emps :with tx} d e))))
          (doseq [ e (seq es) ]
            (if (= (:login e) "e2")
              (doseq [ d (seq ds) ]
                (dbio-set-m2m {:as :depts :with tx} e d)))) ))

      (let [ s1 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-m2m
                    {:as :emps :with tx}
                    (some (fn [d]
                              (if (= (:dname d) "d2") d nil)) ds) )))
             s2 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-m2m
                    {:as :depts :with tx}
                    (some (fn [e]
                              (if (= (:login e) "e2") e nil)) es) ))) ]
        (and (== (count s1) 3)
             (== (count s2) 3)) ))))

(defn- undo-m2m []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           d2 (.execWith
               sql
               (fn [^SQLr tx]
                 (.findOne tx :testcljc.dbio.dbstuff/Department {:dname "d2"} )))
           e2 (.execWith
               sql
               (fn [^SQLr tx]
                 (.findOne tx :testcljc.dbio.dbstuff/Employee {:login "e2"} ))) ]

      (.execWith
        sql
        (fn [^SQLr tx]
          (dbio-clr-m2m { :as :emps :with tx } d2)))

      (.execWith
        sql
        (fn [^SQLr tx]
          (dbio-clr-m2m { :as :depts :with tx } e2)))

      (let [ s1 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-m2m {:as :emps :with tx} d2)))

             s2 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-m2m {:as :depts :with tx} e2))) ]

        (and (== (count s1) 0)
             (== (count s2) 0)) ))))


(defn- undo-company []
  (binding [ *META-CACHE* (.getMetaCache ^DBAPI @DB) ]
    (let [ ^Transactable sql (.newCompositeSQLr ^DBAPI @DB)
           c (.execWith
               sql
               (fn [^SQLr tx]
                 (.findOne tx :testcljc.dbio.dbstuff/Company {:cname "acme"} ))) ]
      (.execWith
        sql
        (fn [^SQLr tx]
          (dbio-clr-o2m {:as :depts :with tx} c)))

      (.execWith
        sql
        (fn [^SQLr tx]
          (dbio-clr-o2m {:as :emps :with tx} c)))

      (let [ s1 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-o2m {:as :depts :with tx} c)))
             s2 (.execWith
                  sql
                  (fn [^SQLr tx]
                    (dbio-get-o2m {:as :emps :with tx} c))) ]

        (and (== (count s1) 0)
             (== (count s2) 0))))))



(deftest testdbio-dbstuff

  (is (do (init-test nil) true))

         ;; basic CRUD
         ;;
  (is (let [ m (meta (create-emp "joe" "blog" "joeb"))
             r (:rowid m)
             v (:verid m) ]
        (and (> r 0) (== v 0))))
  (is (let [ a (fetch-all-emps) ]
        (== (count a) 1)))
  (is (let [ a (fetch-emp "joeb" ) ]
        (not (nil? a))))
  (is (let [ a (change-emp "joeb" ) ]
        (not (nil? a))))
  (is (let [ rc (delete-emp "joeb") ]
        (== rc 0)))
  (is (let [ a (fetch-all-emps) ]
        (== (count a) 0)))

         ;; one to one assoc
         ;;
  (is (wedlock))
  (is (undo-wedlock))

         ;; one to many assocs
  (is (test-company))

         ;; m to m assocs
  (is (test-m2m))
  (is (undo-m2m))

  (is (undo-company))

)

;;(use-fixtures :each init-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dbstuff-eof nil)

;;(clojure.test/run-tests 'testcljc.dbio.dbstuff)

