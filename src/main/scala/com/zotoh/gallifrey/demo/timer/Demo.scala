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

package demo.timer

import java.util.concurrent.atomic.AtomicInteger
import java.util.{Date=>JDate}
import org.json._

import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.TimerEvent

import com.zotoh.wflow.core.Job
import com.zotoh.wflow._



class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
  }
  def configure(cfg:JSONObject) {
  }
  def start() {}
  def stop() {
  }
  def dispose() {
  }
}


object Demo {
  private val _count= new AtomicInteger(0)
  def count() = _count.incrementAndGet()
}

class Demo() extends PipelineDelegate {

  import Demo._

  def getStartActivity(pipe:Pipeline) = new PTask( new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      val ev= job.event.asInstanceOf[TimerEvent]
      if ( ev.isRepeating ) {
        println("-----> (" + count +  ") repeating-update: " + new JDate())
      } else {
        println("-----> once-only!!: " + new JDate())
      }
      null
    }
  })

  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

}





