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

import org.jboss.netty.channel.group.ChannelGroup
import com.zotoh.frwk.io.XData

/**
 * @author kenl
 *
 */
class HTTPResponseHdlr(g:ChannelGroup) extends BasicChannelHandler(g) {

//  private var _cb:HTTPMsgIO = null
//  def bind(cb:HTTPMsgIO): this.type = {
//    _cb= cb; this
//  }
//  override def doResFinal(ctx:HTTPMsgInfo , out:XData) {
//    if (_cb != null)  {
//      _cb.onOK( ctx, out)
//    }
//  }
//  override def onResError(code:Int, reason:String) {
//    if (_cb != null) {
//      _cb.onError(code, reason)
//    }
//  }
}


