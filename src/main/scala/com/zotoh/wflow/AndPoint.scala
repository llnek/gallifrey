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

import com.zotoh.frwk.util.CoreUtils._
import com.zotoh.wflow.core.Scope

/**
 * A "AND" join enforces that all bound activities must return before Join continues.
 *
 * @author kenl
 *
 */
class AndPoint protected[wflow](s:FlowPoint,a:And) extends JoinPoint(s,a) {

  def eval(j:Scope) = {
    val nv= _cntr.incrementAndGet()
    val c= getClosureArg()
    var rc:FlowPoint= null

    tlog().debug("AndPoint: size={}, cntr={}, join={}",
      asJObj( size()),  asJObj(nv), asJObj(this))

    // all branches have returned, proceed...
    if (nv == size() ) {
      rc= if (_body == null) nextPoint() else _body
      if (rc != null) { rc.attachClosureArg(c) }
      realize()
    }

    rc
  }

}
