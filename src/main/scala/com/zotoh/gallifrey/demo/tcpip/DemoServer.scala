/*??
 * COPYRIGHT (C) 2012 CHERIMOIA LLC. ALL RIGHTS RESERVED.
 *
 * THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
 * MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE,
 * VERSION 2.0 (THE "LICENSE").
 *
 * THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
 * BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
 * AND LIMITATIONS UNDER THE LICENSE.
 *
 * You should have received a copy of the Apache License
 * along with this distribution; if not, you may obtain a copy of the
 * License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 ??*/

package demo.tcpip

import java.io.{DataInputStream, BufferedInputStream}
import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.{SocketEvent, TimerEvent}

import com.zotoh.wflow.core.Job
import com.zotoh.wflow._
import org.json.JSONObject


/**
 * @author kenl
 *
 */
class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
    println("Demo sending & receiving messages via sockets..." )
  }
  def configure(cfg:JSONObject) {
  }
  def start() {}
  def stop() {
  }
  def dispose() {
  }
}

class Demo extends PipelineDelegate {

  def onError(err:Throwable, curPt:FlowPoint) = null
  def onStop(pipe:Pipeline) {
  }

  private var _clientMsg=""

  val task1= new Work() {
      def perform(cur:FlowPoint, job:Job, arg:Any) = {

          val ev= job.event.asInstanceOf[SocketEvent]
          val sockBin = { (ev:SocketEvent)  =>
            val clen=new DataInputStream(ev.getSockIn).readInt()
            val bf= new BufferedInputStream( ev.getSockIn )
            val buf= new Array[Byte](clen)
            bf.read(buf)
            _clientMsg=new String(buf,"utf-8")
          }
          sockBin(ev)
          // add a delay into the workflow before next step
          new Delay(1500)
      }
  }

  val task2= new Work() {
      def perform(cur:FlowPoint, job:Job, arg:Any) = {
        println("Socket Server Received: " + _clientMsg )
        null
      }
  }

  def getStartActivity(pipe:Pipeline) = new PTask(task1).chain(new PTask(task2))

}

