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

import scala.collection.mutable

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
import javax.jms.StreamMessage
import javax.jms.TemporaryQueue
import javax.jms.TemporaryTopic
import javax.jms.TextMessage
import javax.jms.Topic
import javax.jms.TopicPublisher
import javax.jms.TopicSession
import javax.jms.TopicSubscriber

import com.zotoh.mock.jms.MockUtils._


/**
 * @author kenl
 *
 */
class MockTopicSession( private val _conn:MockTopicConnection,
  private val _tx:Boolean , private val _ack:Int )
extends TopicSession {

  private val _subs = mutable.HashMap[String,TopicSubscriber]()
  @volatile private var _active = false

  def close() { _active=false }

  def commit() {}

  def createBrowser(q:Queue ) = null

  def createBrowser(q:Queue , a:String ) = null

  def createBytesMessage() = null

  def createConsumer(d:Destination ) = null

  def createConsumer(d:Destination , a:String) = null

  def createConsumer(d:Destination, a:String, a2:Boolean ) = null

  def createMapMessage() = null

  def createMessage() = null

  def createObjectMessage() = null

  def createObjectMessage(a:java.io.Serializable) = null

  def createProducer(d:Destination ) = null

  def createQueue(a:String ) = null

  def createStreamMessage() = null

  def createTemporaryQueue() = null

  def createTextMessage() = createTextMessage("")

  def createTextMessage(s:String ) = new MockTextMessage(s)

  def getAcknowledgeMode() = 0

  def getMessageListener() = null

  def getTransacted() = false

  def recover() {}

  def rollback() {}

  def run() {
    _active=true
    val me=this
    val t= new Thread(new Runnable() {
      def run() {
        Thread.sleep(3000)
        while (me._active && me._conn.isActive()) {
          me.trigger()
          Thread.sleep(3000)
        }
        me._active=false
      }
    })
    t.setDaemon(true)
    t.start()
  }

  private def trigger() {
    val m= createTextMessage(makeNewTextMsg_x() )
    m.setJMSType("Mock-Topic-Type")

    _subs.foreach { (t) =>
      val ml=t._2.getMessageListener()
      if (ml != null) {
        ml.onMessage(m)
      }
    }
  }

  def setMessageListener(ml:MessageListener ) {}

  def createDurableSubscriber(t:Topic, n:String ) = {

    val s= new MockTopicSubscriber(t, n)
    _subs += t.getTopicName() ->  s
    s
  }

  def createDurableSubscriber(t:Topic , n:String , a2:String , arg3:Boolean) = null

  def createPublisher(t:Topic ) = null

  def createSubscriber(t:Topic ) = {
    val s= new MockTopicSubscriber(t,"")
    _subs += t.getTopicName() -> s
    s
  }

  def createSubscriber(t:Topic , a:String, a2:Boolean ) = null

  def createTemporaryTopic() = null

  def createTopic(a:String ) = null

  def unsubscribe(a:String ) {}

}
