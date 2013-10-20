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

package demo.async

import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container
import org.json.JSONObject
import com.zotoh.wflow._
import scala.Some
import com.zotoh.wflow.core.Scope


/**
 * @author kenl
 *
 */
class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
    println("Demo calling an async java-api & resuming.")
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

  def getStartActivity(pipe:Pipeline) = new PTask( new Work() {
    def perform(cur:FlowPoint, job:Scope, arg:Any) = {
      val t= new AsyncResumeToken( cur )
      println("/* Calling a mock-webservice which takes a long time (10secs),")
      println("- since the call is *async*, event loop is not blocked.")
      println("- When we get a *call-back*, the normal processing will continue */")
      DemoAsyncWS.doLongAsyncCall(new AsyncCallback() {
        override def onSuccess(result:Any) {
          println("CB: Got WS callback: onSuccess")
          println("CB: Tell the scheduler to re-schedule the original process")
          // use the token to tell framework to restart the idled process
          t.resume(result)
        }
        override def onError(e:Throwable) {
          t.resume(e)
        }
        override def onTimeout() {
          onError( new Exception("time out"))
        }
      })

      println("\n\n")
      println("+ Just called the webservice, the process will be *idle* until")
      println("+ the websevice is done.")
      println("\n\n")

      new AsyncWait()
    }
  }).chain( new PTask(new Work() {
    def perform(cur:FlowPoint, job:Scope, arg:Any) = {
      println("-> The result from WS is: " + arg)
      null
    }
  }) )


  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

}

