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

  comzotohcljc.tardis.auth.shiro)


(import '(org.apache.shiro.authz AuthorizationException AuthorizationInfo))
(import '(org.apache.shiro.authc.credential CredentialsMatcher))
(import '(org.apache.shiro.realm AuthorizingRealm))
(import '(org.apache.shiro.authc
  AuthenticationException AuthenticationToken AuthenticationInfo SimpleAccount))
(import '(com.zotoh.frwk.dbio DBAPI))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.crypto.codec :only [pwdify] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype PwdMatcher [] CredentialsMatcher
  (doCredentialsMatch [_ token info]
    (let [ pwd (.getCredentials ^AuthenticationToken token)
           uid (.getPrincipal ^AuthenticationToken token)
           pc (.getCredentials ^AuthenticationInfo info)
           pu (-> (.getPrincipals ^AuthenticationInfo info)
                  (.getPrimaryPrincipal)) ]
      (and (= pu uid)
               (= pc (pwdify pwd ""))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private shiro-eof nil)



