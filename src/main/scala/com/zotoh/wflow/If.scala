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
 * @author kenl
 *
 */
class If(
  expr:BoolExpr,
  private val _thenCode:Activity,
  private val _elseCode:Activity
 ) extends Conditional(expr) {

  def this(expr:BoolExpr ,thenCode:Activity) {
    this(expr, thenCode, null )
  }

  def reifyPoint(cur:FlowPoint ) = new IfPoint(cur,this)

  def realize(fp:FlowPoint ) {
    val s=fp.asInstanceOf[IfPoint]
    val np= s.nextPoint()
    s.withElse( if (_elseCode ==null) np else _elseCode.reify(np) )
    s.withThen( _thenCode.reify(np))
    s.withTest( expr())
  }

}
