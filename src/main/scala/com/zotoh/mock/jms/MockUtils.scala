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



package com.zotoh.mock.jms

import java.util.Random


/**
 * @author kenl
 *
 */
object MockUtils {

  /**
   * @return
   */
  def makeNewTextMsg_plus() = {
    val r= new Random()
    val a=r.nextInt(100)
    val b=r.nextInt(100)
    val c= 0L + a + b
    "Calc.  " + a + " + " + b + " = " + c
  }

  /**
   * @return
   */
  def makeNewTextMsg_x() = {
    val r= new Random()
    val a=r.nextInt(100)
    val b=r.nextInt(100)
    val c= 1L * a * b
    "Calc.  " + a + " * " + b + " = " + c
  }

}
