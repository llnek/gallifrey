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


package testzotoh.java.frwk.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class JUTest {

  //public JUTest() {}

  public static junit.framework.Test suite()     {
        return
        new JUnit4TestAdapter(JUTest.class);
    }

    @BeforeClass
    public static void iniz() throws Exception    {
    }

    @AfterClass
    public static void finz()    {
    }

    @Before
    public void open() throws Exception    {
    }

    @After
    public void close() throws Exception    {
    }

    @Test
    public void testDummy() throws Exception {
        assertEquals(1, 1);
    }

}

