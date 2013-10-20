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

import scala.math._
import java.util.concurrent.{LinkedBlockingQueue,RejectedExecutionHandler}
import java.util.concurrent.{ThreadPoolExecutor,TimeUnit}
import org.slf4j._
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

/**
 * @author kenl
 */
object TCore {
  private var _log:Logger = LoggerFactory.getLogger(classOf[TCore])
}

/**
 * @author kenl
 */
class TCore(private val _id:String, tds:Int) extends RejectedExecutionHandler {

  //private val serialVersionUID = 404521678153694367L

  private var _scd:ExecutorService= null
  def tlog() = TCore._log
  private val _tds = max(4, tds)

  def start() { activate }

  def stop() {}

  def dispose() {
    //_scd.shutdownNow()
    _scd.shutdown()
  }

  def schedule(work:Runnable) {
    _scd.execute(work)
  }

  def rejectedExecution(r:Runnable, x:ThreadPoolExecutor) {
    //TODO: deal with too much work for the core...
    tlog.error("\"{}\" rejected work - threads/queue are max'ed out" , _id);
  }
  
  private def activate() {
//    _scd= Executors.newCachedThreadPool( new TFac(_id) )
    _scd= new ThreadPoolExecutor( _tds, _tds, 5000,
        TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](),
        new TFac(_id) , this )
    tlog.info("Core \"{}\" activated with threads = {}" , _id , "" + _tds, "")
  }

}

