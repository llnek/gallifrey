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



/**
 * A "AND" join enforces that all bound activities must return before Join continues.
 *
 * @author kenl
 *
 */
class And(body:Activity) extends Join(body) {

  def reifyPoint(cur:FlowPoint ) = new AndPoint(cur, this)

  def realize(fp:FlowPoint) {
    val s = fp.asInstanceOf[AndPoint]
    if (_body != null) {
      s.withBody( _body.reify( s.nextPoint()))
    }
    s.withBranches(_branches)
  }

}
