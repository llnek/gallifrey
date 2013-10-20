
package com.zotoh.frwk.server

import com.zotoh.frwk.util.Schedulable

trait ServerLike {

  def hasService( serviceId:Any) : Boolean
  def getService( serviceId:Any) : Service
  def core() : Schedulable

}
