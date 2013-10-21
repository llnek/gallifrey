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
class Split(protected var _theJoin:Join) extends Composite {

  def this() {
    this(null)
  }

  def addSplit(a:Activity ): this.type = {
    add(a)
    this
  }

  def reifyPoint(cur:FlowPoint ) = new SplitPoint(cur, this)

  override def realize(fp:FlowPoint ) {
    val p=fp.asInstanceOf[SplitPoint]
    if ( _theJoin != null) {
      _theJoin.withBranches( size() )
    } else {
      _theJoin= new NullJoin()
    }

    val s = _theJoin.reify(p.nextPoint() )
    // note: get all *children* to come back to the join
    p.withBranches( new Iter(s, listChildren() ) )

    _theJoin match {
      case n:NullJoin =>
        p.fallThrough()
      case _ =>
    }
  }


}

/**
 * @author kenl
 *
 */
class NullJoin extends Join(null) {

  def reifyPoint(cur:FlowPoint ) = new NullJoinPoint(cur, this)

  def realize(cur:FlowPoint ) {}

}

/**
 * @author kenl
 *
 */
class NullJoinPoint(s:FlowPoint, a:Join ) extends JoinPoint(s,a) {

  def eval(j:Job ) = null

}
