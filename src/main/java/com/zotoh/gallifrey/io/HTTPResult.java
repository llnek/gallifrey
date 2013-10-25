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


package com.zotoh.gallifrey.io;



import java.net.HttpCookie;
import java.net.URL;

public interface HTTPResult extends IOResult {

  public void setRedirect(URL location);

  public void setProtocolVersion(String ver);
  public void setStatus(int code);
  public void addCookie(HttpCookie c);

  public void containsHeader(String name);
  public void removeHeader(String name);
  public void clearHeaders();

  public void addHeader(String name, String value);
  public void setHeader(String name, String value);

  public void setChunked(boolean c);
  public void setContent(Object data);

  public int getStatus();
  
}



