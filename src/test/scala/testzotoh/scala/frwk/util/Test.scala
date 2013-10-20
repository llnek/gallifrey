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


package testzotoh.scala.frwk.util

import org.scalatest.Assertions._
import org.scalatest._


    import org.apache.shiro.config._
    import org.apache.shiro.realm._
import org.apache.shiro.authc.credential._
    
    import org.apache.shiro.authc._
    import org.apache.shiro.subject._
import org.apache.shiro.SecurityUtils
import org.apache.shiro.subject.Subject

   
    
class Test  extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  override def beforeAll(configMap: Map[String, Any]) {
  }

  override def afterAll(configMap: Map[String, Any]) {
  }

  override def beforeEach() { }

  override def afterEach() { }

  test("testDummy") {
    assert(true)
  }

}
