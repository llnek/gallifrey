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

package demo.file

import java.util.concurrent.atomic.AtomicInteger
import java.util.{Date=>JDate}
import java.io.File
import org.apache.commons.io.{FileUtils=>FUT}
import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.FileEvent
import org.json._
import com.zotoh.frwk.server.Service

import com.zotoh.wflow.core.Scope
import com.zotoh.wflow._

/**
 * @author kenl
 */
class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
    println("Demo file directory monitoring - picking up new files")
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

object DemoGen {
  private val _count= new AtomicInteger(0)
  def count() = _count.incrementAndGet()
}

/**
 * @author kenl
 * Create a new file every n secs
 *
 */
class DemoGen extends PipelineDelegate {
  import DemoGen._

  def getStartActivity(pipe:Pipeline) = new PTask(new Work() {
    def perform(cur:FlowPoint, job:Scope, arg:Any) = {
          val s= "Current time is " + new JDate()
          pipe.container().getService("default-sample") match {
            case p:Service =>
              FUT.writeStringToFile( new File(p.getv("target").toString(), "ts-"+ count +".txt"),
                s, "utf-8")
            case _ =>
          }
    }
  })

  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null
}

