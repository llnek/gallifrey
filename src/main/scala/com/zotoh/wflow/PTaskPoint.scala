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


package com.zotoh.wflow

import com.zotoh.wflow.core.Job

/**
 * @author kenl
 *
 */
class PTaskPoint(s:FlowPoint, a:PTask ) extends FlowPoint(s,a) {

  private var _work:Work = null

  def withWork(w:Work): this.type = {
    _work=w
    this
  }

  def eval(j:Job ) = {
    //tlog.debug("PTaskPoint: {} about to perform work.", this.id )
    val a= _work.perform(this, j, popClosureArg())
    var rc= nextPoint()

    a match {
      case n:Nihil =>
        rc = new NihilPoint(flow() )
      case x:Activity =>
        rc = x.reify(rc)
      case _  =>
        if (rc != null) {
          rc.attachClosureArg(a)
        }
    }

    rc
  }


}

