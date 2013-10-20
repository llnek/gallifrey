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

import javax.jms.BytesMessage
import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.MapMessage
import javax.jms.Message
import javax.jms.MessageConsumer
import javax.jms.MessageListener
import javax.jms.MessageProducer
import javax.jms.ObjectMessage
import javax.jms.Queue
import javax.jms.QueueBrowser
import javax.jms.Session
import javax.jms.StreamMessage
import javax.jms.TemporaryQueue
import javax.jms.TemporaryTopic
import javax.jms.TextMessage
import javax.jms.Topic
import javax.jms.TopicSubscriber


/**
 * @author kenl
 *
 */
class MockSession(private val _tx:Boolean,private val _ack:Int) extends Session {

  def close() {}

  def commit() {}

  def createBrowser(q:Queue ) = null

  def createBrowser(q:Queue , a:String ) = null

  def createBytesMessage() = null

  def createConsumer(d:Destination ) = new MockMsgConsumer(d)

  def createConsumer(d:Destination , a:String ) = null

  def createConsumer(d:Destination , a:String ,
      a2:Boolean ) = null

  def createDurableSubscriber(t:Topic , a:String ) = null

  def createDurableSubscriber(t:Topic , a1:String , a2:String , a3:Boolean ) = null

  def createMapMessage() = null

  def createMessage() = null

  def createObjectMessage() = null

  def createObjectMessage(s:java.io.Serializable) = null

  def createProducer(d:Destination ) = null

  def createQueue(a:String ) = null

  def createStreamMessage() = null

  def createTemporaryQueue() = null

  def createTemporaryTopic() = null

  def createTextMessage() = null

  def createTextMessage(a:String) = null

  def createTopic(a:String ) = null

  def getAcknowledgeMode() = 0

  def getMessageListener() = null

  def getTransacted() = false

  def recover() {}

  def rollback() {}

  def run() {}

  def setMessageListener(ml:MessageListener ) {}

  def unsubscribe(a:String ) {}

}
