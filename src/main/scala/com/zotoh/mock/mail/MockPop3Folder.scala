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

import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Message
import javax.mail.MessagingException
import javax.mail.Store


/**
 * @author kenl
 *
 */
class MockPop3Folder(private val _name:String, s:Store) extends Folder(s) {

  private var _open = false
  private var _count = 1

  override def appendMessages(a:Array[Message] ) {}

  override def close(arg:Boolean) {
    _open=false
  }

  override def create(arg:Int) = false

  override def delete(arg:Boolean) = false

  override def exists() = true

  override def expunge() = null

  override def getFolder(arg:String) = null

  override def getFullName() = _name

  override def getMessage(pos:Int) = {
    if (pos < 1) throw new MessagingException("wrong message num: " + pos)
    new MockPop3Msg(this, pos).newMimeMsg()
  }

  override def getMessageCount() = _count

  override def getName() = _name

  override def getParent() = null

  override def getPermanentFlags() = null

  override def getSeparator() = 0

  override def getType() = 0

  override def hasNewMessages() = false

  override def isOpen() = _open

  override def list(arg:String) = null

  override def open(arg:Int) {
    _open=true
  }

  override def renameTo(arg:Folder) = false


}

