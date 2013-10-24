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


package com.zotoh.gallifrey.core


import com.zotoh.gallifrey.io.IOEvent

import com.zotoh.frwk.core.Disposable
import java.io.File
import com.zotoh.frwk.server.ServerLike
import com.zotoh.frwk.dbio.DBAPI

trait Container extends ServerLike with Disposable {

  def notifyObservers(evt:IOEvent, options:Any ) : Unit
  def getAppKey() : String
  def getAppDir() : File

  def acquireJdbc(groupid:String) : DBAPI
}

