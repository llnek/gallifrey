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


package demo.tcpip

import org.apache.commons.lang3.{StringUtils=>STU}
import java.net.Socket
import java.io.{DataOutputStream, OutputStream}
import java.util.{Date=>JDate}

import com.zotoh.frwk.util.CoreUtils._
import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container

import com.zotoh.wflow.core.Job
import com.zotoh.wflow._
import org.json.JSONObject
import com.zotoh.frwk.server.Service


/**
 * @author kenl
 *
 */
class DemoClient extends PipelineDelegate {

  private val _textMsg= "Hello World, time is ${TS} !"

  def onError(err:Throwable, curPt:FlowPoint) = null
  def onStop(pipe:Pipeline) {
  }

  def getStartActivity(pipe:Pipeline) = {
    // opens a socket and write something back to parent process
    val me=this
    new Delay(2000).chain( new PTask( new Work() {
      def perform(cur:FlowPoint, job:Job, arg:Any) = {
          pipe.container().getService("default-sample") match {
            case tcp:Service =>
              val s= STU.replace(_textMsg,"${TS}", new JDate().toString )
              println("TCP Client: about to send message" + s )
              val bits= s.getBytes("utf-8")
              val port= tcp.getv("port")
              val host= tcp.getv("host").toString()
              val soc=new Socket( host, port.asInstanceOf[Long].toInt)
              try {
                val os= soc.getOutputStream()
                val dos=new DataOutputStream(os)
                dos.writeInt(bits.length)
                os.write(bits)
                os.flush()
              } finally {
                soc.close()
              }
            case _ =>
          }
          null
        }
      })
    )
  }

}

