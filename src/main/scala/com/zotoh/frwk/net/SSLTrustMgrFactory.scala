/*??
*
* Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
*
* This library is distributed in the hope that it will be useful
* but without any warranty; without even the implied warranty of
* merchantability or fitness for a particular purpose.
*
* The use and distribution terms for this software are covered by the
* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
* which can be found in the file epl-v10.html at the root of this distribution.
*
* By using this software in any fashion, you are agreeing to be bound by
* the terms of this license.
* You must not remove this notice, or any other, from this software.
*
 ??*/

package com.zotoh.frwk.net

import java.security.InvalidAlgorithmParameterException
import java.security.KeyStore
import java.security.KeyStoreException
import java.security.cert.CertificateException
import java.security.cert.{X509Certificate=>XCert}

import javax.net.ssl.ManagerFactoryParameters
import javax.net.ssl.TrustManager
import javax.net.ssl.TrustManagerFactorySpi
import javax.net.ssl.X509TrustManager

import org.slf4j._


/**
 * @author kenl
 *
 */
object SSLTrustMgrFactory {

  private val _log=LoggerFactory.getLogger(classOf[SSLTrustMgrFactory])
  def tlog() = _log

  def getTrustManagers() = Array[TrustManager]( new X509TrustManager() {
    def checkClientTrusted( chain:Array[XCert], authType:String) {
      tlog.warn("SkipCheck: CLIENT CERTIFICATE: {}" , chain(0).getSubjectDN)
    }
    def checkServerTrusted( chain:Array[XCert], authType:String) {
      tlog.warn("SkipCheck: SERVER CERTIFICATE: {}" , chain(0).getSubjectDN)
    }
    def getAcceptedIssuers() = Array[XCert]()
  })

}

/**
 * @author kenl
 *
 */
class SSLTrustMgrFactory extends TrustManagerFactorySpi {

  override def engineGetTrustManagers() = SSLTrustMgrFactory.getTrustManagers
  override def engineInit(ks:KeyStore) {}
  override def engineInit(p:ManagerFactoryParameters) {}
}

