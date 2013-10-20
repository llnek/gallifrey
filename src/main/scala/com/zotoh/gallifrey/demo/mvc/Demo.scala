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

package demo.mvc

import java.text.SimpleDateFormat
import java.util.{Date=>JDate}
import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import com.zotoh.gallifrey.io.{HTTPEvent,HTTPResult}
import org.json._
import com.zotoh.frwk.server.Service

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
    println("Point your browser to http://localhost:8000/test/hello")
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

class Demo extends PipelineDelegate {

  private def fmtHtml() = """
      <html><head>
      <title>Hohenheim: Test Web</title>
      <link rel="shortcut icon" href="/public/images/favicon.ico"/>
      <link type="text/css" rel="stylesheet" href="/public/styles/main.css"/>
      <script type="text/javascript" src="/public/scripts/test.js"></script>
      </head>
      <body><h1>Bonjour!</h1><br/>
      <button type="button" onclick="pop();">Click Me!</button>
      </body></html>
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
        res.setContent( fmtHtml )
        res.setStatus(200)

        // associate this result with the orignal event
        // this will trigger the http response
        ev.replyResult
    }
  }

  def getStartActivity(pipe:Pipeline) = new PTask(task1)

  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

}

