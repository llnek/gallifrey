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


package com.zotoh.wflow


/**
 * In cases where an async call has been made, this *token* is needed to resume the original processing, which has been
 * put on hold until the async call returns.
 *
 * @author kenl
 */
abstract class FAsyncResumeToken(protected var _proc:FlowPoint) {

  def resume(resultArg:Any): Unit
}

