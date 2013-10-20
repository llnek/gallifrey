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

import javax.jms.JMSException
import javax.jms.Message
import javax.jms.MessageListener
import javax.jms.Queue
import javax.jms.QueueReceiver


/**
 * @author kenl
 *
 */
class MockQueueReceiver(private var _Q:Queue) extends QueueReceiver {

  private var _sub:MessageListener = null

  def close() {
    _sub=null
    _Q= null
  }

  def getMessageListener() = _sub

  def getMessageSelector() = ""

  def receive()  = null

  def receive(a:Long) = null

  def receiveNoWait() = null

  def setMessageListener(ml:MessageListener) {
    _sub=ml
  }

  def getQueue() = _Q

}
