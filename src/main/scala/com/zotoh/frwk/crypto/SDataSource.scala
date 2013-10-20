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

package com.zotoh.frwk.crypto

import java.io.{File,IOException,InputStream,OutputStream,ByteArrayInputStream}
import javax.activation.DataSource
import com.zotoh.frwk.util.{CoreUtils=>CU}
import com.zotoh.frwk.io.XStream

/**
 * @author kenl
 *
 */
class SDataSource extends DataSource {

  private var _bits:Array[Byte]= null
  private var _ctype:String=""
  private var _fn:File=null

  /**
   * @param content
   * @param contentType
   */
  def this(content:File, contentType:String) {
    this()
    _ctype= CU.nsb(contentType)
    _fn= content
  }

  /**
   * @param content
   * @param contentType
   */
  def this(content:Array[Byte], contentType:String) {
    this()
    _ctype= CU.nsb(contentType)
    _bits= content
  }


  /**
   * @param content
   */
  def this(content:File) {
    this(content, "")
  }


  /**
   * @param content
   */
  def this(content:Array[Byte]) {
    this(content, "")
  }


  override def getContentType() = _ctype

  override def getInputStream() = {
    if (_fn==null) new ByteArrayInputStream(_bits) else new XStream(_fn)
  }

  override def getName() = "Unknown"

  override def getOutputStream() = throw new IOException("Not implemented")

}

