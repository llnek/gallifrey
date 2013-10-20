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

package com.zotoh.tpcl.axis

import java.util.{Calendar,Date=>JDate,TimeZone}
import java.text.SimpleDateFormat

import org.apache.axis.encoding.ser.CalendarDeserializer
import org.apache.axis.i18n.Messages
import javax.xml.namespace.QName
import org.slf4j._


object CalDeserializer  {
  private val _log= LoggerFactory.getLogger(classOf[CalDeserializer])
}

// axis gives the  wrong timezone, this fixes it
// and get the right timezone
class CalDeserializer(javaType:Class[_], xmlType:QName) extends CalendarDeserializer(javaType,xmlType) {

  import CalDeserializer._
  def tlog() = _log

  override def makeValue(source:String) : Object = {
    try {
      _makeValue(source)
    } catch {
      case e:Throwable =>
        tlog().error("", e)
        throw e
    }
  }

  private def _makeValue(srcString:String) : Object = {
    val calendar = Calendar.getInstance()
    var source =srcString
    var bc = false

    if (source == null || source.length() == 0)
        throw new NumberFormatException(Messages.getMessage("badDateTime00"))

    if (source.charAt(0) == '+')
        source = source.substring(1)

    if (source.charAt(0) == '-')  {
        source = source.substring(1)
        bc = true
    }

    if (source.length() < 19)
      throw new NumberFormatException(Messages.getMessage("badDateTime00"))

    if (source.charAt(4) != '-' || source.charAt(7) != '-' || source.charAt(10) != 'T')
      throw new NumberFormatException(Messages.getMessage("badDate00"))

    if (source.charAt(13) != ':' || source.charAt(16) != ':')
      throw new NumberFormatException(Messages.getMessage("badTime00"))

    var tz= TimeZone.getTimeZone("GMT")
    var tzStr=""
    var pos=0
    var date:JDate = null
    val zulu = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

    try {
      zulu.setTimeZone(TimeZone.getTimeZone("GMT"))
      date = zulu.parse(source.substring(0, 19) + ".000Z")
      pos= source.indexOf('+', 19)
      if (pos != -1) {
        tzStr= source.substring(pos)
      }
      pos= source.indexOf('-', 19)
      if (pos != -1) {
        tzStr= source.substring(pos)
      }
      pos=tzStr.indexOf(':')
      if (pos != -1) {
        tzStr= tzStr.substring(0,pos) + tzStr.substring(pos+1)
      }
    } catch {
      case e:Throwable => throw new NumberFormatException(e.toString())
    }

    pos = 19
    if (pos < source.length() && source.charAt(pos) == '.') {
      var milliseconds = 0
      pos += 1
      val start = pos

      while ( pos < source.length() && Character.isDigit(source.charAt(pos))) {
        pos += 1
      }

      val decimal = source.substring(start, pos)
      if (decimal.length() == 3) {
        milliseconds = Integer.parseInt(decimal) 
      }
      else
      if (decimal.length() < 3) {
        milliseconds = Integer.parseInt((decimal + "000").substring(0, 3))
      }
      else {
        milliseconds = Integer.parseInt(decimal.substring(0, 3))
        if (decimal.charAt(3) >= '5') {
          milliseconds += 1
        }
      }
      date.setTime(date.getTime() + milliseconds)
    }

    if (pos + 5 < source.length() &&
        (source.charAt(pos) == '+' || source.charAt(pos) == '-')) {
      if (!Character.isDigit(source.charAt(pos + 1)) || 
          !Character.isDigit(source.charAt(pos + 2)) || 
          source.charAt(pos + 3) != ':' || 
          !Character.isDigit(source.charAt(pos + 4)) || 
          !Character.isDigit(source.charAt(pos + 5)))
            throw new NumberFormatException(Messages.getMessage("badTimezone00"))

        val hours = ((source.charAt(pos + 1) - 48) * 10 + source.charAt(pos + 2)) - 48
        val mins = ((source.charAt(pos + 4) - 48) * 10 + source.charAt(pos + 5)) - 48
        var milliseconds = (hours * 60 + mins) * 60 * 1000
        if (source.charAt(pos) == '+') {
          milliseconds = -milliseconds
        }
        date.setTime(date.getTime() + milliseconds)
        pos += 6
        tz= TimeZone.getTimeZone("GMT"+ tzStr)
    }

    if (pos < source.length() && source.charAt(pos) == 'Z') {
      pos += 1
    }

    calendar.setTimeZone(tz)

    if (pos < source.length())  {
      throw new NumberFormatException(Messages.getMessage("badChars00"))
    }

    calendar.setTime(date)

    if (bc) {  calendar.set(0, 0) }
    if (this.javaType == classOf[JDate]) {
      date
    } else {
      calendar
    }
  }

}
