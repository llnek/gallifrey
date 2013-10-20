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

package com.zotoh.frwk.io

import java.io.{ IOException, CharArrayWriter, ByteArrayOutputStream=>ByteArrayOS,File,InputStream,ByteArrayInputStream=>ByteArrayIS}
import org.apache.commons.lang3.{StringUtils=>STU}
import org.apache.commons.io.{FileUtils=>FUT,IOUtils=>IOU}
import org.slf4j._


/**
 * Wrapper structure to abstract a piece of data which can be a file
 * or a memory byte[].  If the data is byte[], it will also be
 * compressed if a certain threshold is exceeded.
 *
 * @author kenl
 *
 */

object XData {
  private val _log= LoggerFactory.getLogger(classOf[XData])
}

@SerialVersionUID(-8637175588593032279L) class XData() extends Serializable {

  private var _encoding ="utf-8"
  private var _data:Any = null
  private var _cls=true

  def tlog() = XData._log

  def this(p:Any) {
    this()
    resetContent(p)
  }

  def encoding_=(enc:String) { _encoding=enc }
  def encoding = _encoding

  /**
   * Control the internal file.
   *
   * @param del true to delete, false ignore.
   */
  def setDeleteFile(del:Boolean): this.type = { _cls= del; this }
  def isDeleteFile() = _cls

  def destroy() {
    _data match {
      case x:File if isDeleteFile => FUT.deleteQuietly( x)
      case _ =>
    }
    reset()
  }

  def isDiskFile() = _data match {
    case x:File => true
    case _ => false
  }

  def resetContent(obj:Any, delIfFile:Boolean): this.type = {
    destroy()
    obj match {
      case wtr: CharArrayWriter => _data= new String( wtr.toCharArray)
      case baos: ByteArrayOS => _data = baos.toByteArray
      case bits: Array[Byte] => _data= bits
      case fa:Array[File] => if (fa.length > 0) _data= fa(0)
      case f:File => _data = f
      case s:String => _data = s
      case _ => _data= obj
    }
    setDeleteFile(delIfFile)
  }

  def resetContent(obj:Any): XData = resetContent(obj, true)

  def content() = _data

  def hasContent() = _data != null

  def javaBytes() : Array[Byte] = _data match {
    case x:File => IOU.toByteArray(x.toURI.toURL)
    case x:String => x.getBytes(_encoding)
    case x:Array[Byte] => x
    case _ => Array()
  }

  def fileRef() =  _data match {
    case x:File => x
    case _ => null
  }

  def filePath() =  _data match {
    case x:File => x.getCanonicalPath
    case _ => ""
  }

  def size() : Long = _data match {
    case x:File => x.length()
    case _ => javaBytes().length
  }

  override def finalize() {
    destroy()
  }

  def stringify() = {
    new String ( javaBytes(), _encoding )
  }

  def stream(): InputStream = _data match {
    case x:File => new XStream(x)
    case x if x != null => new ByteArrayIS( javaBytes() )
    case _ => null
  }

  private def reset() {
    _encoding= "utf-8"
    _cls=true
    _data=null
  }

}

