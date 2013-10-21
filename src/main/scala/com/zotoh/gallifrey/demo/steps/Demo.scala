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

package demo.steps

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
    println("Demo a set of workflow control features..." )
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
 * What this example demostrates is a webservice which takes in some user info, authenticate the
 * user, then perform some EC2 operations such as granting permission to access an AMI, and
 * permission to access/snapshot a given volume.  When all is done, a reply will be sent back
 * to the user.
 *
 * This flow showcases the use of conditional activities such a Switch() &amp; If().  Shows how to loop using
 * While(), and how to use Split &amp; Join.
 *
 * @author kenl
 *
 */
class Demo extends PipelineDelegate {
  import Auth._

  def onError(err:Throwable, curPt:FlowPoint) = null
  def onStop(pipe:Pipeline) {
    println("Finally, workflow is done.!")
  }

  // step1. choose a method to authenticate the user
  // here, we'll use a switch() to pick which method
  private val AuthUser = new Switch(new SwitchChoiceExpr() {
            def getChoice(j:Job) = {
              // hard code to use facebook in this example, but you
              // could check some data from the job, such as URI/Query params
              // and decide on which mth-value to switch() on.
              println("Step(1): Choose an authentication method.")
              "facebook"
            }
    }).
          withChoice("facebook", getAuthMtd("facebook")).
          withChoice("google+", getAuthMtd("google+")).
          withChoice("openid", getAuthMtd("openid")).
          withDef( getAuthMtd("db"))

  // step2.
  private val get_profile = new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("Step(2): Get user profile\n-> user is superuser.\n")
      null
    }
  }

  private val GetProfile = new PTask(get_profile)

  // step3. we are going to dummy up a retry of 2 times to simulate network/operation
  // issues encountered with EC2 while trying to grant permission.
  // so here , we are using a while() to do that.
  private val perm_ami = new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      job.getv("ami_count") match {
        case n:Int if (n == 2) =>
          println("Step(3): Granted permission for user to launch this ami(id).\n")
        case n:Int =>
          println("Step(3): Failed to contact ami- server, will retry again... ("+n+") ")
        case _ =>
      }
      null
    }
  }

  private val prov_ami = new While(new PTask(perm_ami),
      new BoolExpr() {
        def evaluate(j:Job) = {
          var c=0
          j.getv("ami_count") match {
            case n:Int =>
              // we are going to dummy up so it will retry 2 times
              c= n+1
            case _ =>
          }
          j.setv("ami_count", c)
          c < 3
        }
      })

  // step3'. we are going to dummy up a retry of 2 times to simulate network/operation
  // issues encountered with EC2 while trying to grant volume permission.
  // so here , we are using a while() to do that.
  private val perm_vol = new Work() {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      job.getv("vol_count") match {
        case n:Int if (n==2) =>
          println("Step(3'): Granted permission for user to access/snapshot this volume(id).\n")
        case n:Int =>
          println("Step(3'): Failed to contact vol- server, will retry again... ("+n+") ")
        case _ =>
      }
      null
    }
  }

  private val prov_vol = new While( new PTask(perm_vol),
    new BoolExpr() {
      def evaluate(j:Job) = {
        var c=0
        j.getv( "vol_count") match {
          case n:Int =>
            // we are going to dummy up so it will retry 2 times
             c = n +1
          case _ =>
        }
        j.setv("vol_count", c)
        c < 3
      }
    })

  // step4. pretend to write stuff to db. again, we are going to dummy up the case
  // where the db write fails a couple of times.
  // so again , we are using a while() to do that.
  private val write_db = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      job.getv("wdb_count") match {
        case n:Int if (n==2) =>
          println("Step(4): Wrote stuff to database successfully.\n")
        case n:Int =>
          println("Step(4): Failed to contact db- server, will retry again... ("+n+") ")
        case _ =>
      }
      null
    }
  }

  private val save_sdb = new While( new PTask(write_db),
    new BoolExpr() {
      def evaluate(j:Job) = {
        var c=0
        j.getv("wdb_count") match {
          case n:Int =>
            // we are going to dummy up so it will retry 2 times
             c= n+1
          case _ =>
        }
        j.setv("wdb_count", c)
        c < 3
      }
    })

  // this is the step where it will do the provisioning of the AMI and the EBS volume
  // in parallel.  To do that, we use a split-we want to fork off both tasks in parallel.  Since
  // we don't want to continue until both provisioning tasks are done. we use a AndJoin to hold/freeze
  // the workflow.
  private val Provision = new Split(new And(save_sdb)).addSplit(prov_ami).addSplit(prov_vol)

  // this is the final step, after all the work are done, reply back to the caller.
  // like, returning a 200-OK.
  private val reply_user = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("Step(5): We'd probably return a 200 OK back to caller here.\n")
      null
    }
  }

  private val ReplyUser = new PTask(reply_user)

  private val error_user = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("Step(5): We'd probably return a 200 OK but with errors.\n")
      null
    }
  }

  private val ErrorUser = new PTask(error_user)


  // do a final test to see what sort of response should we send back to the user.
  private val FinalTest = new If( ReplyUser, ErrorUser,
    new BoolExpr() {
      def evaluate(j:Job ) = {
        // we hard code that all things are well.
        true
      }
    })

  // returning the 1st step of the workflow.
  def getStartActivity(pipe:Pipeline) =
    // so, the workflow is a small (4 step) workflow, with the 3rd step (Provision) being
    // a split, which forks off more steps in parallel.
    new Block(AuthUser,GetProfile,Provision,FinalTest)

}

