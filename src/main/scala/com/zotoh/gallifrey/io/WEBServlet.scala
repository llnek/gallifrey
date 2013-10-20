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

import scala.collection.mutable
import java.io.IOException
import org.slf4j._
import javax.servlet.ServletConfig
import javax.servlet.ServletContext
import javax.servlet.ServletException
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.continuation.Continuation
import org.eclipse.jetty.continuation.ContinuationSupport
import com.zotoh.frwk.io.XData

/**
 * @author kenl
 *
 */
@SerialVersionUID(-3862652820921092885L) class WEBServlet(private var _src:ServletEmitter ) extends HttpServlet
with Serializable {

  private val _log = LoggerFactory.getLogger(classOf[WEBServlet])
  def tlog() = _log

  def this() {
    this(null)
  }
  
  override def destroy() {
    tlog().debug("WEBServlet: destroy()")
  }

  override def service(request:ServletRequest, response:ServletResponse) {
    val rsp= response.asInstanceOf[HttpServletResponse]
    val req= request.asInstanceOf[HttpServletRequest]

    tlog().debug("{}\n{}\n{}",
    "********************************************************************",
      req.getRequestURL(),
      "********************************************************************")

    val c = ContinuationSupport.getContinuation(req)
    if (c.isInitial ) try {
      _src.doService(req,rsp)
    } catch {
      case e:Throwable => tlog().error("",e)
    }
  }

  override def init(config:ServletConfig) {
    super.init(config)

    val ctx= config.getServletContext()
    ctx.getAttribute( "czchhhiojetty") match {
      case x:ServletEmitter => 
        _src = x
      case _ =>
    }

    try {
      tlog().debug("{}\n{}{}\n{}\n{}{}",
        "********************************************************************",
        "Servlet Container: ",
        ctx.getServerInfo(),
        "********************************************************************",
        "Servlet:iniz() - servlet:" ,
        getServletName())
    } catch {
      case e:Throwable =>
    }

  }

}
