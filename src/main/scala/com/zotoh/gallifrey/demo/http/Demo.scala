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

package demo.http

import java.text.SimpleDateFormat
import java.util.{Date=>JDate}

import org.json._

import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.{HTTPEvent, TimerEvent}

import com.zotoh.wflow.core.Scope
import com.zotoh.wflow._


/**
 * @author kenl
 *
 */

class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
    println("Point your browser to http://localhost:8080/test/hello")
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

  private def fmtXml() = """
<?xml version = "1.0" encoding = "utf-8"?>
<hello xmlns="http://simple/">
<world>
  Holy Batman!
</world>
</hello>
  """

  val task1= new Work() {
    def perform(cur:FlowPoint, job:Scope, arg:Any) = {
        val ev= job.event.asInstanceOf[HTTPEvent]
        val res= ev.getResultObj
        /*
        val text= <html>
        <h1>The current date-time is:</h1>
        <p>
          { new SimpleDateFormat("yyyy/MM/dd' 'HH:mm:ss.SSSZ").format( new JDate() ) }
        </p>
        </html>.buildString(false)
*/
        // construct a simple html page back to caller
        // by wrapping it into a stream data object
        res.setHeader("content-type", "text/xml")
        res.setContent( fmtXml)
        res.setStatus(200)

        // associate this result with the orignal event
        // this will trigger the http response
        ev.replyResult()
        null
    }
  }

  def getStartActivity(pipe:Pipeline) = new PTask(task1)
  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

}

