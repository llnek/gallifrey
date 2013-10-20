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

import scala.collection.JavaConversions._
import scala.collection.mutable


/**
 * @author kenl
 *
 */
class Switch(private var _expr:SwitchChoiceExpr ) extends Activity {

  private val _choices= mutable.HashMap[Any,Activity]()
  private var _def:Activity =null

  def withChoice(matcher:Any, body:Activity ): this.type = {
    _choices.put(matcher, body)
    this
  }

  def withDef(a:Activity ): this.type = {
    _def=a
    this
  }

  def reifyPoint(cur:FlowPoint ) = new SwitchPoint(cur, this)

  def realize(fp:FlowPoint ) {
    val t= mutable.HashMap[Any,FlowPoint]()
    val p=fp.asInstanceOf[SwitchPoint]
    val nxt= p.nextPoint()

    _choices.foreach { (en) =>
      t.put(en._1,
              en._2.reify(nxt))
    }

    p.withChoices(t.toMap)
    if (_def != null) {
      p.withDef( _def.reify(nxt))
    }
    p.withExpr(_expr)
  }








}
