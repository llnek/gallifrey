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

import org.slf4j._

/**
 * An Activity is a definition of work - a task to be done.
 * At runtime, it has to be reified - make alive.  This process
 * turns an Activity into a Step in the Workflow.
 *
 * @author kenl
 *
 */
abstract class Activity protected[wflow]() {

  private val _log:Logger= LoggerFactory.getLogger(classOf[Activity])
  def tlog() = _log

  /**
   * Connect up another activity to make up a chain.
   *
   * @param a the unit of work to follow after this one.
   * @return an *ordered* list of work units.
   */
  def chain( a:Activity ) =  new Block(this, a)

  def +(a:Activity) = chain(a)

  /**
   * Instantiate a *live* version of this work unit as it becomes
   * part of the Workflow.
   *
   * @param cur current step.
   * @return a *live* version of this Activity.
   */
  def reify(cur:FlowPoint ):FlowPoint = {
    reifyPoint(cur).realize()
  }

  protected def reifyPoint(cur:FlowPoint) : FlowPoint

  /**
   * Configure the *live* version of this Activity.
   *
   *
   */
//  protected[wflow] def realize[T <: FlowPoint](p:T):Unit
  protected[wflow] def realize(p:FlowPoint):Unit

//  override def finalize() {
//    super.finalize()
//    println("=========================> Activity: " + getClass.getName + " finz'ed")
//  }

}
