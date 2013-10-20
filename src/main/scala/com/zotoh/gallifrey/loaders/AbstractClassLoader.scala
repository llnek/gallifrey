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


package com.zotoh.gallifrey.loaders

import scala.collection.JavaConversions._
import scala.collection.mutable
import java.net.URLClassLoader
import java.io.File
import java.net.URL
import java.io.FilenameFilter

/**
 * @author kenl
 */
abstract class AbstractClassLoader(par:ClassLoader) extends URLClassLoader( Array[URL]() ,par) {

  protected var _loaded=false

  def findUrls(dir:File): this.type = {
    if (dir.exists ) {
      val seq = dir.listFiles( new FilenameFilter() {
        def accept(f:File,n:String) = n.endsWith(".jar")
      })
      seq.foreach( addUrl _ )
    }
    this
  }

  def addUrl(f:File): this.type = {
    addURL( f.toURI.toURL)
    this
  }

}

