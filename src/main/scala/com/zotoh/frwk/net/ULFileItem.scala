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

import org.apache.commons.io.{IOUtils=>IOU}
import com.zotoh.frwk.util.CoreUtils._
import com.zotoh.frwk.io.IOUtils._
import com.zotoh.frwk.io.XData

import java.io.{OutputStream, InputStream, ByteArrayOutputStream=>ByteArrayOS}
import java.io.{File,FileOutputStream,IOException}

import org.apache.commons.fileupload._
import org.slf4j._

object ULFileItem {
  private val _log= LoggerFactory.getLogger(classOf[ULFileItem])
}

/**
 * @author kenl
 *
 */
@SerialVersionUID( 2214937997601489203L)
class ULFileItem extends FileItem with Serializable {

  def tlog() = ULFileItem._log

  @transient private var _os:OutputStream = null
  private var _fieldBits:Array[Byte] = null
  private var _filename = ""
  private var _field=""
  private var _ctype=""
  private var _ds:XData= null
  private var _ff = false

  /**
   * @param field
   * @param contentType
   * @param isFormField
   * @param fileName
   */
  def this(field:String, contentType:String, isFormField:Boolean, fileName:String)   {
    this()
    _ctype= nsb(contentType)
    _field= field
    _ff= isFormField
    _filename= fileName
  }

  def this(field:String, contentType:String, fileName:String, file:XData)   {
    this()
    _ctype= nsb(contentType)
    _field= field
    _ff= false
    _filename= fileName
    _ds=file
  }

  def this(field:String, value:Array[Byte]) {
    this()
    _ctype= ""
    _field= field
    _ff= true
    _filename= ""
    _os=iniz()
    _os.write(value)
  }

  override def delete()  {
    IOU.closeQuietly(_os)
    if (_ds!=null) {
      ;;_ds.setDeleteFile(true)
      _ds.destroy()
    }
    _ds=null
  }

  override def getHeaders()  = throw new IOException("not implemented")
  override def setHeaders(h:FileItemHeaders) {
    throw new IOException("not implemented")
  }
    
  override def getContentType() = _ctype

  override def get() = null

  override def getFieldName()  = _field

  override def getInputStream() = throw new IOException("not implemented")

  override def getName() = _filename

  override def getOutputStream() = {
    if (_os==null) iniz() else _os
  }

  override def getSize() = 0L

  def fileData() = _ds

  override def getString() = getString("UTF-8")

  override def getString(charset:String) = {
    if (maybeGetBits() == null) null else new String(_fieldBits, charset)
  }

  override def isFormField() = _ff

  override def isInMemory() = false

  override def setFieldName(s:String) {
    _field=s
  }

  override def setFormField(b:Boolean)  {
    _ff= b
  }

  override def write(fp:File) { }

  def cleanup()  {
    if (_fieldBits == null) {  maybeGetBits() }
    IOU.closeQuietly(_os)
    _os=null
  }

  override def finalize() {
    IOU.closeQuietly(_os)
    super.finalize()
  }

  override def toString() = {
    val s= "field name= " + getFieldName() + "\n" +  
    "formfield= " + isFormField() + "\n" + 
    "filename= " + getName() + "\n"  
    
    val s2= if (_ds != null) {
      "filepath = " + _ds.filePath
    } else {
      "field-value = " + getString()
    }
    
    s+s2 + "\n"    
  }
  
  private def maybeGetBits() = {
    _os match {
      case baos:ByteArrayOS => _fieldBits= baos.toByteArray()
      case _ =>
    }
    _fieldBits
  }

  private def iniz() = {

    if (_ff) {
      _os= new ByteArrayOS(1024)
    } else {
      _ds= new XData()
      try {
        val t= newTempFile(true)
        _ds.resetContent( t._1)
        _ds.setDeleteFile(true)
        _os = t._2
      } catch {
        case e:Throwable => tlog.error("", e)
      }
    }

    _os
  }

}
