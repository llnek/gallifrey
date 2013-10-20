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

import scala.collection.JavaConversions._
import org.slf4j._
import java.io.File
import org.apache.commons.io.FileUtils
import org.json.JSONObject
import org.json.JSONTokener
import org.apache.commons.lang3.StringUtils

object CoreUtils {
  
  private val _log= LoggerFactory.getLogger(classOf[CoreUtils])
  
  def main(args:Array[String]) {
    println(shuffle("0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"))
  }

  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = {
    try {
      f(param)
    } catch {
      case e:Throwable => _log.warn("", e); throw e
    } finally {
      tryc { () => param.close }
    }
  }

  def tryc ( f:  ()  => Unit ) {
    try {
      f()
    } catch { case e:Throwable =>  }
  }
  
  def blockAndWait(lock:AnyRef, waitMillis:Long) {
    lock.synchronized {
      tryc { () =>
        if (waitMillis > 0L) { lock.wait(waitMillis) } else { lock.wait() }
      }
    }
  }
    
  def unblock(lock:AnyRef) {
    lock.synchronized {
      tryc { () => lock.notifyAll }
    }
  }
  
  def asJObj(a:Any) : Object = a match {
    case x:Object => x
    case _ => null
  }
  
  def nsb(x:Any) = if (x==null) "" else x.toString()
  
  def shuffle(s:String) = {
    val lst= new java.util.ArrayList(s.toCharArray.toSeq)
    java.util.Collections.shuffle(lst)
    new String(lst.toList.toArray[Char])    
  }
 
  def blockForever() {
    while (true) tryc { () =>
      Thread.sleep(8000)
    }
  }
  
  def readJson(f:File) : JSONObject = readJson( FileUtils.readFileToString(f, "utf-8"))
  
  def readJson(s:String) : JSONObject = new JSONObject( new JSONTokener(s))

  def splitNull(s:String) = {
    StringUtils.split( nsb(s), "\u0000")
  }
}

sealed class CoreUtils {}