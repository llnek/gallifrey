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

package demo.pop3

import org.apache.commons.io.IOUtils
import java.util.concurrent.atomic.AtomicInteger
import javax.mail.Message
import javax.mail.Multipart
import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.EmailEvent
import org.json._

import com.zotoh.wflow.core.Job
import com.zotoh.wflow._
/**
 * @author kenl
 *
 */
class DemoMain extends AppMain {
  private val _PS= "com.zotoh.mock.mail.MockPop3Store"
  //private val _PV=new Provider(Provider.Type.STORE, "pop3s", _PS, "test", "1.0.0")
  System.setProperty("gallifrey.demo.pop3", _PS)
  def contextualize(c:Container) {
  }
  def initialize() {
      println("Demo receiving POP3 emails..." )
  }
  def configure(cfg:JSONObject) {
  }
  def start() {
  }
  def stop() {
  }
  def dispose() {
  }
}

object Demo {
  private val _count= new AtomicInteger(0)
  def count() = _count.incrementAndGet()
}

class Demo extends PipelineDelegate {

  import Demo._

  def getStartActivity(pipe:Pipeline) = new PTask(new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      val msg= job.event().asInstanceOf[EmailEvent].getMsg
      println("######################## (" + count + ")" )
      print( msg.getSubject() + "\r\n")
      print( msg.getFrom()(0).toString() + "\r\n")
      print(msg.getRecipients( Message.RecipientType.TO )(0).toString + "\r\n")
      print("\r\n")
      msg.getContent() match {
        case p:Multipart =>
          println ( IOUtils.toString( p.getBodyPart(0).getInputStream()), "utf-8")
        case _ =>
      }
    }
  })


  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

}

