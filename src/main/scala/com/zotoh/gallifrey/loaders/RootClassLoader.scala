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


/**
 * @author kenl
 */
class RootClassLoader(par:ClassLoader) extends AbstractClassLoader( par) {

  val base=System.getProperty("gallifrey.home","")
  if (base.length > 0) { load(base) }

  def configure(baseDir:String) {
    load( baseDir)
  }

  private def load(baseDir:String) {
    val d= new File(baseDir, "dist/exec")
    val p= new File(baseDir, "patch")
    val b= new File(baseDir, "lib")

    if (!_loaded) {
      findUrls(p).findUrls(d).findUrls(b)
    }

    _loaded=true
  }

}

/**
 * @author kenl
 */
class ExecClassLoader(par:ClassLoader) extends AbstractClassLoader( new RootClassLoader( par)) {

  val base=System.getProperty("gallifrey.home","")
  if (base.length > 0) { load(base) }

  private def load(base:String) {
    val p= new File(base, "exec")

    if (p.exists() && !_loaded) {
      findUrls(p)
    }

    _loaded=true
  }

  def configure(baseDir:String) {
    load(baseDir)
  }

}

