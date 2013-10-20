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

package com.zotoh.frwk.mime

import scala.collection.JavaConversions._

import java.io.{ByteArrayInputStream,InputStream}
import java.util.{Properties=>JPS}
import org.apache.commons.lang3.StringUtils._
import javax.activation.MimetypesFileTypeMap

/**
 * @author kenl
 */
object MimeFileTypes {

  def makeMimeFileTypes(props:JPS) = {
    val b= props.entrySet().foldLeft(new StringBuilder ) { (sum, en) =>
      sum.append(  trim( en.getValue().toString() )  + "  " + trim(en.getKey().toString() )  + "\n")
    }.toString()
    new MimetypesFileTypeMap( new ByteArrayInputStream( b.getBytes("utf-8")) )
  }  
  
}

  