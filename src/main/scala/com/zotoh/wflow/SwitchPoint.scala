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
import com.zotoh.wflow.core.Scope

/**
 * @author kenl
 *
 */
class SwitchPoint(s:FlowPoint , a:Activity ) extends FlowPoint(s,a) {

  private var _expr:SwitchChoiceExpr =null
  private var _cs:Map[Any,FlowPoint]=null
  private var _def:FlowPoint =null

  def withChoices(cs:Map[Any,FlowPoint] ): this.type = {
    _cs= cs.toMap
    this
  }

  def withDef(s:FlowPoint ): this.type = {
    _def=s
    this
  }

  def withExpr(e:SwitchChoiceExpr ): this.type = {
    _expr=e
    this
  }

  def choices() = _cs

  def defn() = _def

  def eval(j:Scope ) = {
    val c= popClosureArg()
    var a:FlowPoint = _expr.getChoice(j) match {
      case m if m != null => _cs.get(m).getOrElse(null)
      case _ => null
    }

    // if no match, try default?
    if (a == null) {
      a=_def
    }
    if (a != null) {
      a.attachClosureArg(c)
    }

    realize()
    a
  }

}
