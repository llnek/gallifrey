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

import org.apache.commons.lang3.StringUtils
import java.io._
import org.slf4j._
import org.apache.commons.io.FileUtils

object IOUtils {
  private val _DFT= 1L * 1024 * 1024 * 10 // 10M
  private var _streamLimit= _DFT

  def streamLimit() = _streamLimit
  def setStreamLimit(n:Long) {
    _streamLimit= if (n <= 0) _DFT else n
  }
  
  private var _wd = new File( System.getProperty("java.io.tmpdir"))
  def workDir() = _wd
  def setWorkDir(fpDir:File) { 
    _wd= fpDir
    _wd.mkdirs() 
  }
  
  def newTempFile(open:Boolean=false): (File,OutputStream) = {
    val f= mkTempFile()
    (f, if (open) new FileOutputStream(f) else null)
  }
  
  def mkTempFile(pfx:String="", sux:String=""): File = {
    File.createTempFile(
      if ( StringUtils.isEmpty(pfx)) "temp-" else pfx,
      if ( StringUtils.isEmpty(sux)) ".dat" else sux,
      workDir)
  }

  def listFiles(dir:File, ext:String, recurse:Boolean) = {
    FileUtils.listFiles(dir, Array(ext),recurse)
  }

}


sealed class IOUtils {}


