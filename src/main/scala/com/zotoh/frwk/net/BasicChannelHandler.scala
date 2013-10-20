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

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.math._

import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names.COOKIE

import java.io.{ByteArrayOutputStream=>ByteArrayOS,OutputStream}
import java.io.{IOException,File}
import java.util.{Set,Properties=>JPS}

import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.channel.Channel
import org.jboss.netty.channel.ChannelEvent
import org.jboss.netty.channel.ChannelHandlerContext
import org.jboss.netty.channel.ChannelStateEvent
import org.jboss.netty.channel.ExceptionEvent
import org.jboss.netty.channel.MessageEvent
import org.jboss.netty.channel.SimpleChannelHandler
import org.jboss.netty.channel.group.ChannelGroup
import org.jboss.netty.handler.codec.http.Cookie
import org.jboss.netty.handler.codec.http.CookieDecoder
import org.jboss.netty.handler.codec.http.CookieEncoder
import org.jboss.netty.handler.codec.http.DefaultHttpResponse
import org.jboss.netty.handler.codec.http.HttpChunk
import org.jboss.netty.handler.codec.http.HttpHeaders
import org.jboss.netty.handler.codec.http.HttpMessage
import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpResponse
import org.jboss.netty.handler.codec.http.HttpResponseStatus
import org.jboss.netty.handler.codec.http.HttpVersion

import org.apache.commons.lang3.StringUtils
import com.zotoh.frwk.io.XData
import org.json._
import org.slf4j._
import org.apache.commons.io.IOUtils
import com.zotoh.frwk.util.{CoreUtils=>CU}
import com.zotoh.frwk.io.{IOUtils=>IO}


/**
 * @author kenl
 *
 */
object BasicChannelHandler {
  private val _log= LoggerFactory.getLogger(classOf[BasicChannelHandler])
}

/**
 * @author kenl
 *
 */
class BasicChannelHandler( private var _grp:ChannelGroup) extends SimpleChannelHandler {

  private var _thold= com.zotoh.frwk.io.IOUtils.streamLimit()
  private var _props= new JSONObject()
  private var _clen=0L
  private var _keepAlive=false

  private var _cookies:CookieEncoder= null
  private var _fOut:File = null
  private var _os:OutputStream = null

  def tlog() = BasicChannelHandler._log
  def isKeepAlive() = _keepAlive

  override def channelClosed(ctx:ChannelHandlerContext, ev:ChannelStateEvent) {
    val c= maybeGetChannel(ctx,ev)
    if (c != null) { _grp.remove(c) }
    super.channelClosed(ctx, ev)
    tlog.debug("BasicChannelHandler: channelClosed - ctx {}, channel {}",  ctx, if(c==null) "?" else c , "")
  }

  override def channelOpen(ctx:ChannelHandlerContext, ev:ChannelStateEvent) {
    val c= maybeGetChannel(ctx,ev)
    if (c != null) { _grp.add(c) }
    super.channelOpen(ctx, ev)
    tlog().debug("BasicChannelHandler: channelOpen - ctx {}, channel {}", ctx, if (c==null) "?" else c, "")
  }

  override def exceptionCaught(ctx:ChannelHandlerContext, ev:ExceptionEvent) {
    tlog().error("", ev.getCause)
    val c= maybeGetChannel(ctx, ev)
    if (c != null) try {
        c.close()
    } finally {
        _grp.remove(c)
    }
//    super.exceptionCaught(ctx, e)
  }

  // false to stop further processing
  protected def onRecvRequest(msgInfo:JSONObject) = true

  override def messageReceived(ctx:ChannelHandlerContext, ev:MessageEvent) {

    val msg = ev.getMessage()
    val msgType= if (msg==null) "???" else msg.getClass().getName()
    
    msg match {
      case x:HttpMessage =>
        _os= new ByteArrayOS(4096)
        _props= new JSONObject()
        msg_recv_0(x)
      case _ =>
    }

    msg match {
      case res:HttpResponse =>
        val s= res.getStatus()
        val r= s.getReasonPhrase()
        val c= s.getCode()
        tlog().debug("BasicChannelHandler: got a response: code {} {}", CU.asJObj(c), CU.asJObj(r), "")        
        _props.put("headers", iterHeaders(res) )
        _props.put("reason", r)
        _props.put("dir", -1)
        _props.put("code", c)
        if (c >= 200 && c < 300) {
          onRes(ctx,ev,s,res)
        } else if (c >= 300 && c < 400) {
          // TODO: handle redirect
          handleResError(ctx,ev)
        } else {
          handleResError(ctx,ev)
        }
      case req:HttpRequest =>
        tlog().debug("BasicChannelHandler: got a request: ")
        if (is100ContinueExpected(req)) {
          send100Continue(ev )
        }
        _keepAlive = HttpHeaders.isKeepAlive(req)
        onReqIniz(ctx,ev, req)
        _props.put("method", req.getMethod.getName)
        _props.put("uri", req.getUri)
        _props.put("headers", iterHeaders(req) )
        _props.put("dir", 1)
        if ( onRecvRequest(_props) ) {
          onReq(ctx,ev,req)
        } else {
          send403(ev)
        }
      case x:HttpChunk => onChunk(ctx,ev,x)
      case _ =>
        throw new IOException( "BasicChannelHandler:  unexpected msg type: " + msgType)            
    }
  }

  private def send100Continue(e:MessageEvent) {
    import org.jboss.netty.handler.codec.http.HttpResponseStatus._
    import  org.jboss.netty.handler.codec.http.HttpVersion._
    e.getChannel().write( new DefaultHttpResponse(HTTP_1_1, CONTINUE))
  }

  private def send403(e:MessageEvent) {
    import org.jboss.netty.handler.codec.http.HttpResponseStatus._
    import  org.jboss.netty.handler.codec.http.HttpVersion._
    e.getChannel().write( new DefaultHttpResponse(HTTP_1_1, FORBIDDEN))
    throw new IOException("403 Forbidden")
  }

  protected def onReq(ctx:ChannelHandlerContext, ev:MessageEvent, msg:HttpRequest) {
    if (msg.isChunked) {
      tlog.debug("BasicChannelHandler: request is chunked")
    } else {
      sockBytes(msg.getContent)
      onMsgFinal(ctx,ev)
    }
  }

  private def onRes(ctx:ChannelHandlerContext, ev:MessageEvent, rc:HttpResponseStatus, msg:HttpResponse) {
    onResIniz(ctx,ev,msg)
    if (msg.isChunked) {
      tlog.debug("BasicChannelHandler: response is chunked")
    } else {
      sockBytes(msg.getContent)
      onMsgFinal(ctx,ev)
    }
  }

  protected def onReqIniz(ctx:ChannelHandlerContext, ev:MessageEvent, msg:HttpRequest ) {
    onReqPreamble( _props )
  }

  protected def onResIniz(ctx:ChannelHandlerContext, ev:MessageEvent, msg:HttpResponse ) {
    onResPreamble( _props)
  }

  protected def onReqPreamble(msgInfo:JSONObject) {
    tlog.debug("BasicChannelHandler: onReqIniz: Method {}, Uri {}",
        CU.nsb( msgInfo.optString("method")),
        CU.nsb( msgInfo.optString("uri")),
        "")
  }

  protected def onResPreamble(msgInfo:JSONObject) {}

  protected def doReqFinal(msgInfo:JSONObject , out:XData) {}
  protected def doResFinal(msgInfo:JSONObject , out:XData) {}
  protected def onResError(code:Int, r:String) {}

  private def handleResError(ctx:ChannelHandlerContext, ev:MessageEvent) {
    val cc= maybeGetChannel(ctx,ev)
    onResError( _props.optInt("code"), _props.optString("reason"))
    if ( !isKeepAlive && cc != null) {
      cc.close()
    }
  }

  private def sockBytes(cb:ChannelBuffer) {
    var loop=true
    if (cb != null) while (loop) {
      loop = cb.readableBytes() match {
        case c if c > 0 =>
          sockit(cb,c)
          true
        case _ => false
      }
    }
  }

  private def sockit(cb:ChannelBuffer, count:Int) {

    val bits= new Array[Byte](4096)
    var total=count

    while (total > 0) {
      val len = min(4096, total)
      cb.readBytes(bits, 0, len)
      _os.write(bits, 0, len)
      total -= len
    }

    _os.flush()

    if (_clen >= 0L) { _clen += count }
    if (_clen > 0L && _clen > _thold) {
      swap()
    }
  }

  private def swap() {
    _os match {
      case baos:ByteArrayOS =>
        val t= IO.newTempFile(true)
        t._2.write(baos.toByteArray)
        t._2.flush()
        _fOut= t._1
        _os=t._2
        _clen= -1L
    }
  }

  protected def doReplyError(ctx:ChannelHandlerContext, ev:MessageEvent, err:HttpResponseStatus) {
    doReplyXXX(ctx,ev,err)
  }

  private def doReplyXXX(ctx:ChannelHandlerContext, ev:MessageEvent, s:HttpResponseStatus) {
    val res= new DefaultHttpResponse(HttpVersion.HTTP_1_1, s)
    val c= maybeGetChannel(ctx,ev)
    res.setChunked(false)
    res.setHeader("content-length", "0")
    c.write(res)
    if ( ! isKeepAlive && c != null ) {
      c.close()
    }
  }

  protected def replyRequest(ctx:ChannelHandlerContext, ev:MessageEvent, data:XData) {
    doReplyXXX(ctx,ev,HttpResponseStatus.OK)
  }

  protected def replyResponse(ctx:ChannelHandlerContext, ev:MessageEvent, data:XData) {
    val c= maybeGetChannel(ctx,ev)
    if ( ! isKeepAlive && c != null ) {
      c.close()
    }
  }

  private def onMsgFinal(ctx:ChannelHandlerContext, ev:MessageEvent) {
    val dir = _props.optInt("dir")
    val out= on_msg_final(ev)
    if ( dir > 0) {
      replyRequest(ctx,ev,out)
      doReqFinal( _props, out)
    } else if (dir < 0) {
      doResFinal( _props, out)
    }
  }

  private def on_msg_final(ev:MessageEvent) = {
    val data= new XData() 
    if (_fOut != null) {
      data.resetContent(_fOut)
    } else {
      data.resetContent(_os)
    }
    IOUtils.closeQuietly(_os)
    _fOut=null
    _os=null
    data
  }

  protected def maybeGetChannel(ctx:ChannelHandlerContext, ev:ChannelEvent) = {
    val cc= ev.getChannel
    if (cc==null) ctx.getChannel else cc
  }

  private def msg_recv_0(msg:HttpMessage) {
    val s= msg.getHeader(COOKIE)
    if ( ! StringUtils.isEmpty(s)) {
      val cookies = new CookieDecoder().decode(s)
      val enc = new CookieEncoder(true)
      cookies.foreach { (c) =>  enc.addCookie(c)  }
      _cookies= enc
    }
  }

  private def onChunk(ctx:ChannelHandlerContext, ev:MessageEvent, msg:HttpChunk ) {
    sockBytes(msg.getContent)
    if (msg.isLast) {
      onMsgFinal(ctx,ev)
    }
  }

  protected def iterHeaders(msg:HttpMessage) = {
    val hdrs= new JSONObject()
    msg.getHeaderNames().foreach { (n) =>
      val arr=msg.getHeaders(n).foldLeft(new JSONArray() ) { (arr, h) => arr.put(h) ; arr } 
      hdrs.put(n, arr)
    }
    tlog.debug("BasicChannelHandler: headers\n{}", hdrs.toString(2) )
    hdrs
  }

}


