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
//import org.apache.commons.io.{FileUtils=>FUT}
import java.net.URLClassLoader
import java.io.File
import java.net.URL

/**
 * @author kenl
 */
class AppClassLoader(par:RootClassLoader) extends AbstractClassLoader(par) {

  def configure(appDir:String) {
    val c= new File(appDir, "POD-INF/classes")
    val p= new File(appDir, "POD-INF/patch")
    val b= new File(appDir, "POD-INF/lib")
    if (!_loaded) {
      findUrls(p)
      addUrl(c)
      findUrls(b)
      if ( new File(appDir, "WEB-INF").exists() ) {
        addUrl( new File(appDir, "WEB-INF/classes"))
        findUrls(new File(appDir, "WEB-INF/lib"))
      }
    }
    _loaded=true
  }

}
