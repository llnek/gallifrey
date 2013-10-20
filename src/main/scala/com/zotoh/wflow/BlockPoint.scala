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

import com.zotoh.wflow.core.Scope


/**
 * @author kenl
 *
 */
class BlockPoint protected[wflow](s:FlowPoint, a:Block) extends CompositePoint(s,a) {

  def eval(j:Scope ) = {
    val c= getClosureArg()   // data pass back from previous async call?
    var rc:FlowPoint =null

    if ( ! _inner.isEmpty()) {
      //tlog.debug("BlockPoint: {} element(s.)",  _inner.size )
      val n=_inner.next()
      n.attachClosureArg(c)
      rc = n.eval(j)
    } else {
      //tlog.debug("BlockPoint: no more elements.")
      rc=nextPoint()
      if (rc != null) {  rc.attachClosureArg(c) }
      realize()
    }

    rc
  }

}

