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


package com.zotoh.mock.jms

import javax.naming.Binding
import javax.naming.Context
import javax.naming.Name
import javax.naming.NameClassPair
import javax.naming.NameParser
import javax.naming.NamingEnumeration
import javax.naming.NamingException


/**
 * @author kenl
 *
 */
class MockContext extends Context {

  def lookup(n:Name) = null

  def lookup(name:String) = {
    name match {
      case "qcf" => new MockQueueConnFactory()
      case "tcf" => new MockTopicConnFactory()
      case "cf" => new MockConnFactory()
      case s:String if s.startsWith("queue.") => new MockQueue(name)
      case s:String if s.startsWith("topic.") => new MockTopic(name)
      case _ => null
    }
  }

  def bind(n:Name, o:Object) {}

  def bind(n:String , o:Object ) {}

  def rebind(n:Name , o:Object ) {}

  def rebind(n:String , o:Object ) {}

  def unbind(n:Name ) {}

  def unbind(n:String ) {}

  def rename(n:Name , nn:Name ) {}

  def rename(n:String , nn:String ) {}

  def list(n:Name ) = null

  def list(n:String ) = null

  def listBindings(n:Name ) = null

  def listBindings(n:String ) = null

  def destroySubcontext(n:Name ) {}

  def destroySubcontext(n:String ) {}

  def createSubcontext(n:Name ) = null

  def createSubcontext(n:String ) = null

  def lookupLink(n:Name ) = null

  def lookupLink(n:String ) = null

  def getNameParser(n:Name ) = null

  def getNameParser(n:String) = null

  def composeName(n:Name , pfx:Name ) = null

  def composeName(n:String , pfx:String) = ""

  def addToEnvironment(pn:String , pv:Object) = null

  def removeFromEnvironment(pn:String) = null

  def getEnvironment() = null

  def close() {}

  def getNameInNamespace() = ""

}
