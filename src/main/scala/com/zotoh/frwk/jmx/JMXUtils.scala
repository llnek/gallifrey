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


package com.zotoh.frwk.jmx

import java.util.Arrays

object JMXUtils {
}

class NameParams(private val _name:String, private val _pms:Array[String]) {

  override def toString() = {
    "" + _name + "/" + _pms.mkString("#")
  }

  override def hashCode() = {
    var hash= 31 * (31 + _name.hashCode)
    if (_pms != null) {
      hash += Arrays.hashCode(_pms.toArray[Object] )
    }
    hash
  }

  override def equals(obj:Any) = {
    if (obj == null || getClass() != obj.getClass) false else {
      val other = obj.asInstanceOf[NameParams]
      if ( _name != other._name) false else {
        Arrays.equals(_pms.toArray[Object], other._pms.toArray[Object])
      }
    }
  }

}
