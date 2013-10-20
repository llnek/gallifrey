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

import javax.jms.ConnectionConsumer
import javax.jms.ConnectionMetaData
import javax.jms.Destination
import javax.jms.ExceptionListener
import javax.jms.JMSException
import javax.jms.ServerSessionPool
import javax.jms.Session
import javax.jms.Topic
import javax.jms.TopicConnection
import javax.jms.TopicSession


/**
 * @author kenl
 *
 */
class MockTopicConnection(private val _user:String,private val _pwd:String) extends TopicConnection {

  @volatile private var _active = true

  def close() { stop() }

  def createConnectionConsumer(d:Destination , a1:String , p:ServerSessionPool , arg3:Int) = null

  def createSession(b:Boolean , arg1:Int) = null

  def getClientID() = ""

  def getExceptionListener() = null

  def getMetaData() = null

  def setClientID(a:String ) {}

  def setExceptionListener(e:ExceptionListener) {}

  def start() { _active=true }

  def stop() { _active=false }

  def createConnectionConsumer(t:Topic , a1:String , p:ServerSessionPool , arg3:Int) = null

  def createDurableConnectionConsumer(t:Topic ,
      a1:String , a2:String , p:ServerSessionPool , arg4:Int) = null

  def createTopicSession(tx:Boolean, ack:Int) = {

    val s= new MockTopicSession(this, tx, ack)
    s.run()
    s
  }

  /**
   * @return
   */
  def isActive() = _active

}
