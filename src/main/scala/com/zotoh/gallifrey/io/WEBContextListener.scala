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


package com.zotoh.gallifrey.io

import java.io.{File,InputStream}
import java.util.{Properties=>JPS}
import javax.servlet.ServletContext
import javax.servlet.ServletContextEvent
import javax.servlet.ServletContextListener
import org.slf4j._
import org.eclipse.jetty.webapp.WebAppContext


/**
 * @author kenl
 */
class WEBContextListener extends ServletContextListener {

  private var _log=LoggerFactory.getLogger(classOf[WEBContextListener])
  private var _src:Any = null
  def tlog() = _log

  override def contextInitialized(evt:ServletContextEvent) {

    tlog().info("WEBContextListener: contextInitialized()")

    val x= evt.getServletContext()
    var ctx=""
    val m= x.getMajorVersion()
    val n= x.getMinorVersion()

    tlog.info("Servlet-Context: major version {}, minor version {}", m, n)

    if (m > 2 || ( m==2 && n > 4)) {
      ctx= x.getContextPath()
    }

    try {
      inizAsJ2EE(x, ctx)
    } catch {
      case e:Throwable => 
        tlog().error("", e)
        throw e
    }
  }

  override def contextDestroyed(e:ServletContextEvent) {
    tlog().info("WEBContextListener: contextDestroyed()")
    if (_src!=null) {
      //_src.container.dispose()
    }
    _src=null
  }

  private def inizAsJ2EE(ctx:ServletContext, ctxPath:String) {
    tlog().info("inizAsJ2EE - setting up context-path: {}", ctxPath)
  }

}
