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


package com.zotoh.frwk.util;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.LinkedHashMap;

/**
 * A map that has case-ignored string keys. 
 *
 * @author kenl
 *
 * @param <T>
 */
public class NCOrderedMap<T> extends NCMap<T>  {
  
  private static final long serialVersionUID = -3637175588593032279L;  
  private Map<String,T> _map= new LinkedHashMap<String,T>();
  
  public Set<Map.Entry<String,T>> entrySet() {
	  return _map.entrySet();
  }
  
  public T put(String key, T value) {
	  _map.put( key.toLowerCase(), value);
	  return super.put(key, value);
  }
  
  public T remove(String key) {
	  _map.remove(key.toLowerCase());
	  return super.remove(key);
  }
  
  public Set<String> keySet() {
	  return _map.keySet();
  }
  
  public void clear() {
	  _map.clear();
	  super.clear();
  }
  
  public Collection<T> values() {
	  return _map.values();
  }
  
  public void putAll(Map<? extends String,? extends T> m) {
	  _map.putAll(m);
	  super.putAll(m);
  }
  
}

