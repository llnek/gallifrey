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


package com.zotoh.frwk.util

import java.util.concurrent.{Executors,ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The default thread factory - from javasoft code.  The reason why
 * we cloned this is so that we can control how the thread-id is
 * traced out. (we want some meaninful thread name).
 *
 * @author kenl
 */
class TFac(private val _pfx:String) extends ThreadFactory {
  private val _cl = Thread.currentThread().getContextClassLoader
  private val _seq= new AtomicInteger(0)
  private val _fac = Executors.defaultThreadFactory()

  private val _group = System.getSecurityManager() match {
    case sm:SecurityManager => sm.getThreadGroup()
    case _ => Thread.currentThread().getThreadGroup
  }

  def newThread(r:Runnable) = {
    val t = _fac.newThread(r)
    t.setName(mkTname)
    t.setPriority(Thread.NORM_PRIORITY)
    t.setDaemon(false)
    t.setContextClassLoader(_cl)
    t
  }

  def XXnewThread(r:Runnable) = {
    val t = new Thread(_group, r, mkTname(), 0)
    t.setPriority(Thread.NORM_PRIORITY)
    t.setDaemon(false)
    t.setContextClassLoader(_cl)
    t
  }

  private def mkTname() = { _pfx + _seq.incrementAndGet }

}
