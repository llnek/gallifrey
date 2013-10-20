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
abstract class Composite extends Activity {

  private val _children= mutable.ArrayBuffer[Activity]()

  def size() = _children.size()

  protected def add(a:Activity ) {
    _children += a
    onAdd(a)
  }

  protected def onAdd(a:Activity ) {}

  def listChildren() = _children.toSeq

  def realize(fp:FlowPoint) {
    val p=fp.asInstanceOf[CompositePoint]
    p.reifyInner( listChildren )
  }

}


abstract class CompositePoint(cur:FlowPoint,a:Activity) extends FlowPoint(cur,a) {
  protected var _inner:Iter = null
  def reifyInner(children:Seq[Activity]) {
    _inner=new Iter(this,children)
  }
  def inner() = _inner
}
