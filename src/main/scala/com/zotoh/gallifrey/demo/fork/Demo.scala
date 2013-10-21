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

package demo.fork

import com.zotoh.gallifrey.runtime.AppMain
import com.zotoh.gallifrey.core.Container

import com.zotoh.wflow.core.Job
import com.zotoh.wflow._
import org.json.JSONObject

/**
 * @author kenl
 */
class DemoMain extends AppMain {
  def contextualize(c:Container) {
  }
  def initialize() {
    println("Demo fork(split)/join of tasks..." )
  }
  def configure(cfg:JSONObject) {
  }
  def start() {}
  def stop() {
  }
  def dispose() {
  }
}

/**
 * @author kenl
 *
    parent(s1) --> split&nowait
                   |-------------> child(s1)----> split&wait --> grand-child
                   |                              |                    |
                   |                              |<-------------------+
                   |                              |---> child(s2) -------> end
                   |
                   |-------> parent(s2)----> end
 */
class Demo extends PipelineDelegate {

    // split but no wait
    // parent continues;

  def getStartActivity(pipe:Pipeline) = new PTask( new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("I am the *Parent*")
      println("I am programmed to fork off a parallel child process, and continue my business.")
      null
    }
  }).
  chain( new Split().
    addSplit(new PTask(new Work() {
        def perform(cur:FlowPoint, job:Job, arg:Any) = {
          println("*Child*: will create my own child (blocking)")
          job.setv("rhs", 60)
          job.setv("lhs", 5)
          val p2= new PTask( new Work() {
            def perform(cur:FlowPoint, job:Job, arg:Any) = {
              println("*Child*: the result for (5 * 60) according to my own child is = "  +
                          job.getv("result"))
              println("*Child*: done.")
            }
          })
                  // split & wait
          new Split( new And(p2)).addSplit(new PTask(new Work() {
            def perform(cur:FlowPoint, job:Job, arg:Any) = {
              println("*Child->child*: taking some time to do this task... ( ~ 6secs)")
              for (i <- 1 to 6) {
                Thread.sleep(1000)
                print("...")
              }
              println("")
              println("*Child->child*: returning result back to *Child*.")
              job.setv("result",  job.getv("rhs").asInstanceOf[Int] * 
                  job.getv("lhs").asInstanceOf[Int]
              )
              println("*Child->child*: done.")
            }
          }))
        }
      }) )).
    chain(new PTask( new Work() {
        def perform(cur:FlowPoint, job:Job, arg:Any) = {
          println("*Parent*: after fork, continue to calculate fib(6)...")
          val b=new StringBuilder("*Parent*: ")
          for (i <- 1 to 6) {
            b.append( fib(i) + " ")
          }
          println(b.toString  + "\n" + "*Parent*: done.")
          null
        }
      }))

  def onStop(pipe:Pipeline) {}
  def onError(err:Throwable, curPt:FlowPoint) = null

  private def fib(n:Int):Int = {
      if (n <3) 1 else { fib(n-2) + fib(n-1) }
  }
}

