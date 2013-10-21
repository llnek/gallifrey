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


import com.zotoh.wflow.core.Job
import com.zotoh.wflow._

object Auth {

  def getAuthMtd(t:String) = {

    t match {
      case "facebook" => new PTask(facebook_login)
      case "google+" => new PTask(gplus_login)
      case "openid" => new PTask(openid_login)
      case _ => new PTask(db_login)
    }

  }

  private val facebook_login = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("-> using facebook to login.\n")
    }
  }

  private val gplus_login = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("-> using google+ to login.\n")
    }
  }

  private val openid_login = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("-> using open-id to login.\n")
    }
  }

  private val db_login = new Work {
    def perform(cur:FlowPoint, job:Job, arg:Any) = {
      println("-> using internal db to login.\n")
    }
  }

}

