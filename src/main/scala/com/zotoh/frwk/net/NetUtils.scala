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


package com.zotoh.frwk.net

import org.jboss.netty.channel.{ChannelFutureListener,ChannelPipeline}
import org.jboss.netty.channel.{ChannelHandlerContext,ChannelFuture,Channel}
import java.io.{OutputStream}
import org.jboss.netty.buffer.ChannelBuffer
import org.slf4j._
import com.zotoh.frwk.io.{IOUtils,XData}
import org.apache.http.impl.client.{DefaultRedirectStrategy, AbstractHttpClient}
import org.apache.http.{ProtocolException, HttpResponse, HttpRequest}
import org.apache.http.protocol.HttpContext

/**
 * @author kenl
 */
object NetUtils {
  
  private val _log=LoggerFactory.getLogger(classOf[NetUtils])


  def cfgForRedirect(cli:AbstractHttpClient)  {
    cli.setRedirectStrategy(new DefaultRedirectStrategy() {
      override def isRedirected(request:HttpRequest, response:HttpResponse , context:HttpContext )  = {
        var isRedirect=false
        try {
          isRedirect = super.isRedirected(request, response, context)
        } catch {
          case e:ProtocolException =>
          _log.warn("",e)
        }
        if (!isRedirect) {
          val responseCode = response.getStatusLine().getStatusCode()
          if (responseCode == 301 || responseCode == 302 || responseCode == 307 || responseCode == 308) {
            isRedirect= true
          }
        }
        isRedirect
      }
    })
  }

  def dbgNettyDone(msg:String) = new ChannelFutureListener() {
      def operationComplete(fff:ChannelFuture) {  
          _log.debug("netty-op-complete: {}", msg)        
      }
  }

  def getPipeline(ctx:ChannelHandlerContext) : ChannelPipeline = ctx.getPipeline()
  def getPipeline(ch:Channel) : ChannelPipeline = ch.getPipeline()
  def wrtFlush(ch:Channel, obj:Any) = ch.write(obj)
  //def flush(ch:Channel) { ch.flush() }
  def writeOnly(ch:Channel, obj:Any) = ch.write(obj)
  def closeChannel(ch:Channel) {
    ch.close()
  }

  def sockItDown(cbuf:ChannelBuffer, out:OutputStream, lastSum:Long ) = {
    val cnt= if (cbuf==null) 0 else cbuf.readableBytes()
    if (cnt > 0) {
      val bits= new Array[Byte](4096)
      var total=cnt
      while (total > 0) {
        val len = Math.min(4096, total)
        cbuf.readBytes(bits, 0, len)
        out.write(bits, 0, len)
        total -= len
      }
      out.flush()
    }
    lastSum + cnt
  }

  def swapFileBacked(x:XData, out:OutputStream, lastSum:Long) = {
    if (lastSum > IOUtils.streamLimit) {
      val (f,os) = IOUtils.newTempFile(true)
      x.resetContent(f)
      os
    } else {
      out
    }    
  }
  
  
}

sealed class NetUtils {}
