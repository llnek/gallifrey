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

/**
 * @author kenl
 */
trait Schedulable {

  def dequeue(w:Runnable) : Unit
  def run(w:Runnable) : Unit
  def postpone(w:Runnable, delayMillis:Long) : Unit
  def hold(pid:Any, w:Runnable) : Unit
  def hold(w:Runnable) : Unit

  def dispose() : Unit

  def wakeup(w:Runnable) : Unit
  def wakeAndRun(pid:Any,w:Runnable) : Unit
  def reschedule(w:Runnable) : Unit

}


