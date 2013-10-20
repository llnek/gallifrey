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
 * A For is treated sort of like a while with the test-condition being (i &lt; upperlimit).
 * 
 * @author kenl
 *
 */
class For(private val _loopCntr:ForLoopCountExpr,body:Activity) extends While(body) {

  override def reifyPoint(cur:FlowPoint) = new ForPoint(cur,this)

  override def realize(fp:FlowPoint) {
    val p=fp.asInstanceOf[ForPoint]
    super.realize(fp)
    p.withTest(new ForLoopExpr(p, _loopCntr))
  }

}

/**
 * @author kenl
 *
 */
class ForLoopExpr(
  private val _point:FlowPoint , 
  private val _cnt:ForLoopCountExpr ) extends BoolExpr {

  private var _started=false
  private var _loop=0

  def evaluate(j:Scope ) = {
    try {
      if (!_started) {
        _loop=_cnt.getCount(j)
        _started=true
      }
      _point.tlog().debug("ForLoopExpr: loop {}", _loop)
      _loop > 0

    } finally {
      _loop -= 1
    }
  }

}


