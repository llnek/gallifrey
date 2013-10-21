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
import com.zotoh.frwk.server.ServerLike

/**
 * @author kenl
 *
 */
class SplitPoint(s:FlowPoint, a:Split) extends CompositePoint(s,a) {

  private var _fallThru=false

  def eval(j:Job ) = {
    val core = flow().container() match {
      case x:ServerLike => x.core()
      case _ => null
    }
    val c= getClosureArg()
    var rc:FlowPoint =null

    while ( !_inner.isEmpty() ) {
      rc = _inner.next()
      rc.attachClosureArg(c)
      core.run(rc)
    }

    realize()

    // should we also pass the closure to the next step ? not for now
    if (_fallThru) nextPoint() else null
  }

  def withBranches(w:Iter): this.type = {
    _inner=w
    this
  }

  def fallThrough(): this.type = {
    _fallThru=true
    this
  }

}

