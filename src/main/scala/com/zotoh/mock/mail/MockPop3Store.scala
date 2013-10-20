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


package com.zotoh.mock.mail

import javax.mail.Folder
import javax.mail.MessagingException
import javax.mail.Session
import javax.mail.Store
import javax.mail.URLName


/**
 * @author kenl
 *
 */
class MockPop3Store( s:Session,url:URLName) extends Store(s,url) {

  private val _name:String="pop3"
  protected var _dftPort = 110
  protected var _portNum = -1
  protected var _isSSL=false
  protected var _host =""
  protected var _user = ""
  protected var _pwd = ""

    /*
    if (url != null)
      name = url.getProtocol()
      */

  override def protocolConnect(host:String, portNum:Int,
          user:String, pwd:String) = synchronized  {

    if ((host == null) || (pwd == null) || (user == null)) false else {

      _portNum = if (portNum == -1) _dftPort else portNum
      _host = host
      _user = user
      _pwd = pwd

      true
    }
  }

  override def isConnected() = synchronized {
    if ( super.isConnected()) true else false
  }

  override def close() = synchronized  {
    super.close()
  }

  def getDefaultFolder() = {
    checkConnected()
    new DefaultFolder(this)
  }

  def getFolder(name:String) = {
    checkConnected()
    new MockPop3Folder(name,this)
  }

  def getFolder(url:URLName ) = {
    checkConnected()
    new MockPop3Folder( url.getFile(), this)
  }

  override def finalize() {
    super.finalize()
  }

  private def checkConnected() {
    if (!super.isConnected())
      throw new MessagingException("Not connected")
  }

}

