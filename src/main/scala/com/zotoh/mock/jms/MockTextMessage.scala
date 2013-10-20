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

import java.util.Random

import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.TextMessage

import com.zotoh.frwk.util.CoreUtils._

/**
 * @author kenl
 *
 */
class MockTextMessage(private var _text:String) extends TextMessage {

  private var _type="Mock-Text-Message"

  def acknowledge() {}

  def clearBody() {}

  def clearProperties() {}

  def getBooleanProperty(a:String) = false

  def getByteProperty(a:String) = 0

  def getDoubleProperty(a:String) = 0.toDouble

  def getFloatProperty(a:String ) = 0.toFloat

  def getIntProperty(a:String) = 0

  def getJMSCorrelationID() = {
    new Random().nextInt(Int.MaxValue).toString
  }

  def getJMSCorrelationIDAsBytes() = getJMSCorrelationID().getBytes()

  def getJMSDeliveryMode() = 0

  def getJMSDestination() = null

  def getJMSExpiration() = 0L

  def getJMSMessageID() =
    "msg-" + new Random().nextInt(Int.MaxValue)

  def getJMSPriority() = 0

  def getJMSRedelivered() = false

  def getJMSReplyTo() = null

  def getJMSTimestamp() = 0L

  def getJMSType() = _type

  def getLongProperty(a:String) = 0L

  def getObjectProperty(a:String ) = null

  def getPropertyNames() = null

  def getShortProperty(a:String ) = 0.toShort

  def getStringProperty(a:String ) = null

  def propertyExists(a:String ) = false

  def setBooleanProperty(a:String , b:Boolean) {}

  def setByteProperty(a:String , b:Byte ) {}

  def setDoubleProperty(a:String , d:Double ) {}

  def setFloatProperty(a:String , f:Float ) {}

  def setIntProperty(a:String , arg1:Int) {}

  def setJMSCorrelationID(a:String ) {}

  def setJMSCorrelationIDAsBytes(b:Array[Byte] ) {}

  def setJMSDeliveryMode(arg:Int) {}

  def setJMSDestination(d:Destination ) {}

  def setJMSExpiration(a:Long) {}

  def setJMSMessageID(a:String ) {}

  def setJMSPriority(a:Int) {}

  def setJMSRedelivered(a:Boolean) {}

  def setJMSReplyTo(d:Destination ) {}

  def setJMSTimestamp(arg:Long) {}

  def setJMSType(a:String ) {
    _type=a
  }

  def setLongProperty(a:String , arg:Long) {}

  def setObjectProperty(a:String , o:Object ) {}

  def setShortProperty(a:String , arg:Short) {}

  def setStringProperty(a:String , a1:String ) {}

  def getText() = _text

  def setText(t:String ) {
    _text=t
  }

}
