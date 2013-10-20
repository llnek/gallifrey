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



package com.zotoh.mock.mail

import scala.collection.JavaConversions._
import java.io.IOException
import java.io.{InputStream,ByteArrayInputStream=>ByteArrayIS}
import java.util.{Date=>JDate}
import java.util.Enumeration
import java.util.Random
import java.util.Vector
import javax.mail.Folder
import javax.mail.MessagingException
import javax.mail.internet.MimeMessage
import javax.mail.Session
import javax.mail.Message
import javax.mail.Multipart
import org.apache.commons.lang3.{StringUtils=>STU}

/**
 * @author kenl
 */
object MockPop3Msg {
  
  private val _mime=
"From: Some One <someone@example.com>\r\n"+
"To: Some Body <somebody@ex.com>\r\n"+
"Subject: Hello Jack\r\n"+
"MIME-Version: 1.0\r\n"+
"Content-Type: multipart/mixed;boundary=\"XXXXboundary text\"\r\n"+
"This is a multipart message in MIME format.\r\n"+
"\r\n"+
"--XXXXboundary text\r\n"+
"Content-Type: text/plain\r\n"+
"\r\n"+
"this is the time ${TS}\r\n"+
"\r\n"+
"--XXXXboundary text\r\n"+
"Content-Type: text/plain\r\n"+
"Content-Disposition: attachment; filename=\"test.txt\"\r\n"+
"\r\n"+
"this is the attachment text\r\n"+
"\r\n"+
"--XXXXboundary text--\r\n"    
    
  def main(args:Array[String]) {
    val m=new MimeMessage( Session.getInstance(System.getProperties()) , 
        new ByteArrayIS(_mime.getBytes("utf-8")))
    m.saveChanges()
    val h=m.getAllHeaderLines()
    val ct=m.getContentType()
    val x=m.getContent() match {
      case p:Multipart =>
        var c=p.getCount()
        val pp=p.getBodyPart(0)
        c=0
      case _ =>
    }
    val s= m.getFrom()(0)
    val r= m.getRecipients(Message.RecipientType.TO)(0)
    val n=m.getMessageNumber()
    return
  }
  
  
}

/**
 * @author kenl
 *
 */
class MockPop3Msg (f:Folder, m:Int)  {

  import MockPop3Msg._

  def newMimeMsg() = {
    val s=STU.replace(_mime, "${TS}",  new JDate().toString )
      val m= new MimeMessage( Session.getInstance(System.getProperties()) , 
        new ByteArrayIS( s.getBytes("utf-8")))
      m.saveChanges()
      m
  }
  
  
  
}


