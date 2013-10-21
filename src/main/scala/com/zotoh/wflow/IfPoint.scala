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
class IfPoint(s:FlowPoint , a:If) extends ConditionalPoint(s,a) {

  private var _then:FlowPoint = null
  private var _else:FlowPoint = null

  def withElse(s:FlowPoint ): this.type = {
    _else=s
    this
  }

  def withThen(s:FlowPoint ): this.type = {
    _then=s
    this
  }

  def eval(j:Job ) = {
    val c= getClosureArg()   // data pass back from previous async call?
    val b = test(j)
    tlog().debug("If: test {}", ( if(b) "OK" else "FALSE"))
    val rc = if (b) _then else _else
    if (rc != null) {
      rc.attachClosureArg(c)
    }
    realize()
    rc
  }

}
