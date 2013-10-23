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
import java.util.concurrent.atomic.AtomicLong
import com.zotoh.wflow.core.Job
import com.zotoh.frwk.server.ServerLike
import com.zotoh.frwk.util.RunnableWithId

object FlowPoint {
  private val _sn= new AtomicLong(0)
  private def nextID() = _sn.incrementAndGet()
}
/**
 * @author kenl
 *
 */
abstract class FlowPoint protected[wflow](protected val _parent:Pipeline) extends  RunnableWithId {

  private val _log:Logger= LoggerFactory.getLogger(classOf[FlowPoint])
  def tlog() = _log

  import FlowPoint._

  private var _nextPtr:FlowPoint = null
  private var _defn:Activity = null
  private var _closure:Any= null
  private val _pid=nextID()

  /**
   * @param s
   * @param a
   */
  protected def this(s:FlowPoint, a:Activity) {
    this(s.flow)
    _nextPtr=s
    _defn=a
  }

  def eval(j:Job ):FlowPoint

  def getId() = _pid

  def nextPoint() = _nextPtr
  def getDef() = _defn

  def attachClosureArg(c:Any) {
    _closure=c
  }

  def realize() : this.type = {
    getDef().realize(this)
    clsClosure()
    postRealize()
    this
  }

  protected def postRealize() {}

  protected def clsClosure() { _closure=null }
  def getClosureArg() = _closure
  def popClosureArg() = {
    try { val c=_closure; c }
    finally 
    { _closure=null }
  }

  def forceNext(n:FlowPoint) {
    _nextPtr=n
  }
  
  def flow() = _parent

  def rerun() {
    flow().container() match {
      case x:ServerLike => x.core().reschedule(this)
      case _ =>
    }
  }

  def run() {
    var rc:FlowPoint = null
    var err:Activity =null
    val f= flow()

    f.container() match {
      case x:ServerLike => x.core().dequeue(this)
    }
    try {
      f.job.clrLastResult()
      rc=eval( f.job )
    } catch {
      case e:Throwable =>
        err= flow().onError(e, this)
    }

    if (err != null) { rc= err.reify(new NihilPoint(flow())) }
    if (rc==null) {
      tlog().debug("FlowPoint: rc==null => skip.")
      // indicate skip, happens with joins
    } else {
      runAfter(f,rc)
    }

  }

  private def runAfter(f:Pipeline, rc:FlowPoint) {
    val ct= f.container() match {
      case x:ServerLike => x.core()
      case _ => null
    }
    val np= rc.nextPoint()
    rc match {
      case x:DelayPoint => ct.postpone(np, x.delayMillis())
      case x:AsyncWaitPoint => ct.hold( np)
      case x:NihilPoint => f.stop()
      case _ => ct.run(rc)
    }
  }

//  override def finalize() {
//    super.finalize()
//    println("=========================> FlowPoint: " + getClass.getName + " finz'ed")
//  }

}

