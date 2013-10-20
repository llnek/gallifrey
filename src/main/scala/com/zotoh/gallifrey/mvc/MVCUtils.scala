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



package com.zotoh.gallifrey.mvc

import java.text.SimpleDateFormat
import java.util.{TimeZone, Locale}


object MVCUtils {

  private val _fmt = new ThreadLocal[SimpleDateFormat]() {
    override def initialValue() = {
      val f=new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.US)
      f.setTimeZone(TimeZone.getTimeZone("GMT"))
      f
    }
  }

  def getSDF() = _fmt.get

}
