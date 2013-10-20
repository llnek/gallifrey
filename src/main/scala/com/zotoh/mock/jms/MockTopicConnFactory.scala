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

import javax.jms.Connection
import javax.jms.JMSException
import javax.jms.TopicConnection
import javax.jms.TopicConnectionFactory


/**
 * @author kenl
 *
 */
class MockTopicConnFactory extends TopicConnectionFactory {

  def createConnection() = createTopicConnection()

  def createConnection(user:String , pwd:String ) = createTopicConnection(user, pwd)

  def createTopicConnection() = new MockTopicConnection("","")

  def createTopicConnection(user:String , pwd:String ) = new MockTopicConnection(user, pwd)

}
