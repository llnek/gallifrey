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


import java.util.List;
import java.util.Set;
import java.util.Map;
import java.net.HttpCookie;


public interface HTTPEvent  extends IOEvent {

  public List<HttpCookie> getCookies();

  public HttpCookie getCookie(String name);

  public boolean isKeepAlive();

  public Object data();

  public boolean hasData();

  public long contentLength();

  public String contentType();

  public String encoding();

  public String contextPath();

  public List<String> getHeaderValues(String nm);
  public Set<String> getHeaders();
  public String getHeaderValue(String nm);

  public List<String> getParameterValues(String nm);
  public Set<String> getParameters();
  public String getParameterValue(String nm);

  public String localAddr();

  public String localHost();

  public int localPort();

  public String method();

  public String protocol();
  public String host();

  public String queryString();

  public String remoteAddr();

  public String remoteHost();

  public int remotePort();

  public String scheme();

  public String serverName();

  public int serverPort();


  public boolean isSSL();

  public String getUri();

  public String getRequestURL();

  //------------

  public HTTPResult getResultObj();
  public void replyResult();
}



