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

import com.zotoh.frwk.util.CoreUtils._
import org.slf4j._
import com.zotoh.frwk.util.Schedulable
import java.util.concurrent.atomic.AtomicLong
import com.zotoh.wflow.core.Scope
import com.zotoh.frwk.core.Startable
import com.zotoh.frwk.server.ServerLike


trait PipelineDelegate {

  def getStartActivity(pipe:Pipeline) : Activity
  def onStop(pipe:Pipeline) : Unit
  def onError(err:Throwable, curPt:FlowPoint) : Activity

}



object Pipeline {
  private val _sn= new AtomicLong(0L)
  private def nextId() = _sn.incrementAndGet()
}

/**
 * @author kenl
 *
 */
class Pipeline (private val _theScope:Scope, private val _delegateClass:String) extends Startable {

  private val _log:Logger= LoggerFactory.getLogger(classOf[Pipeline] )
  def tlog() = _log

  import Pipeline._

  private val _delegate = Thread.currentThread().getContextClassLoader().loadClass(_delegateClass).getDeclaredConstructor().newInstance() match {
    case x:PipelineDelegate => x
    case _ =>
    throw new ClassCastException("Class " + _delegateClass + " must implement PipelineDelegate.")
  }

  private val _pid= nextId()
  private var _active=false

  tlog().debug("{}: {} => pid : {}" , "Pipeline", getClass().getName() , asJObj(_pid))
  require(_theScope != null, "Scope is null.")

  def core() : Schedulable = container() match {
    case x:ServerLike => x.core()
    case _ => null
  }

  def container() = _theScope.container()
  def isActive() = _active
  def job() = _theScope
  def getPID() = _pid

  protected def onEnd() {
    _delegate.onStop(this)
  }

  protected[wflow] def onError(e:Throwable, cur:FlowPoint) : Activity = {
    tlog().error("", e)
    _delegate.onError(e,cur) match {
      case x:Activity => x
      case _ => new Nihil()
    }
  }

  protected def onStart():Activity = {
    _delegate.getStartActivity(this) match {
      case x:Activity => x
      case _ => new Nihil()
    }
  }

  def start() {
    tlog().debug("{}: {} => pid : {} => starting" , "Pipeline", _delegateClass , asJObj(_pid))

    try {
      val f1= onStart().reify( new NihilPoint(this))
      _active=true
      core().run(f1)
    } catch {
      case e:Throwable =>
        tlog().error("", e)
        stop()
    }
  }

  def stop() {
    try {
      onEnd()
    } catch {
      case e:Throwable =>
      tlog().error("",e)
    }

    tlog().debug("{}: {} => pid : {} => end" , "Pipeline", _delegateClass , asJObj(_pid))
  }

  override def toString() = {
    _delegateClass + "(" + _pid + ")"
  }

//  override def finalize() {
//    super.finalize()
//    println("=========================> Pipeline: " + getClass.getName + " finz'ed")
//  }
  
}
