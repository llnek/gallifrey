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

import mikera.cljunit.ClojureTest;

import java.util.Arrays;
import java.util.List;

public class ClojureJUnit extends ClojureTest {
    @Override
    public List<String> namespaces() {
        return Arrays.asList(new String[]{
                "testcljc.util.byteutils",
                "testcljc.util.codes",
                "testcljc.util.coreutils",
                "testcljc.util.dateutils",
                "testcljc.util.fileutils",
                "testcljc.util.guids",
                "testcljc.util.ioutils",
                "testcljc.util.metautils",
                "testcljc.util.mimeutils",
                "testcljc.util.procutils",
                "testcljc.util.seqnumgen",
                "testcljc.util.strutils",
                "testcljc.util.win32ini",
                "testcljc.net.netstuff",
                "testcljc.i18n.i18nstuff",
                "testcljc.crypto.cryptostuff",
                "testcljc.crypto.mimestuff",
                "testcljc.dbio.dbstuff",
                "testcljc.tardis.auth"

        });
    }
}


