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



import org.apache.commons.io.{FileUtils=>FUT}

import com.zotoh.gallifrey.io.FileEvent


import com.zotoh.wflow.core.Job
import com.zotoh.wflow._




/**
 * @author kenl
 *
 */
class Demo extends PipelineDelegate    {

  def getStartActivity(pipe:Pipeline) = new PTask(new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      val ev= job.event().asInstanceOf[FileEvent]
      val f=ev.getFile()
      println("Picked up new file: " + f)
      println("Content: " + FUT.readFileToString(f, "utf-8"))
      //FUT.deleteQuietly(f)
    }
  })


  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null


}


