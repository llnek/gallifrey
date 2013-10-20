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

import java.util.concurrent.atomic.AtomicInteger

/**
 * @author kenl
 *
 */
abstract class JoinPoint protected[wflow](s:FlowPoint, a:Join) extends FlowPoint(s,a) {

  protected val _cntr=new AtomicInteger(0)
  protected var _body:FlowPoint= null
  private var _branches= 0

  def withBody(body:FlowPoint ): this.type = {
    _body=body
    this
  }

  def withBranches(n:Int): this.type = {
    _branches=n
    this
  }

  def size() = _branches

  override def postRealize() {
    _cntr.set(0)
  }

}
