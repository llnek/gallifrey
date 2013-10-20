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
import javax.mail.MethodNotSupportedException
import javax.mail.Store


/**
 * @author kenl
 *
 */
class DefaultFolder protected[mail] (s:Store) extends Folder(s) {

  def getName() = ""

  def getFullName() = ""

  def getParent() = null

  def exists() = true

  def list(pn:String) = Array( getInbox() )

  def getSeparator() = '/'

  def getType()  = 2

  def create(t:Int) = false

  def hasNewMessages() = false

  def getFolder(name:String) = {
    if (!name.equalsIgnoreCase("INBOX")) {
      throw new MessagingException("Only INBOX is supported")
    }
    getInbox()
  }

  def getInbox() = getStore().getFolder("INBOX")

  def delete(recurse:Boolean) = {
    throw new MethodNotSupportedException("delete")
  }

  def renameTo(f:Folder) = {
    throw new MethodNotSupportedException("renameTo")
  }

  def open(mode:Int) {
    throw new MethodNotSupportedException("open")
  }

  def close(expunge:Boolean) {
    throw new MethodNotSupportedException("close")
  }

  def isOpen() = false

  def getPermanentFlags() = new Flags()

  def getMessageCount() = 0

  def getMessage(msgno:Int) = {
    throw new MethodNotSupportedException("getMessage")
  }

  def appendMessages(msgs:Array[Message] ) {
    throw new MethodNotSupportedException("Append not supported")
  }

  def expunge() = {
    throw new MethodNotSupportedException("expunge")
  }

}

