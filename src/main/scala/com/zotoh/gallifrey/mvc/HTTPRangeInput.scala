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



package com.zotoh.gallifrey.mvc

import scala.collection.JavaConversions._
import scala.collection.mutable

import java.io.{IOException, RandomAccessFile}
import java.util.{Arrays,Comparator}

import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpResponse
import org.jboss.netty.handler.codec.http.HttpResponseStatus
import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.buffer.ChannelBuffers._

import com.zotoh.frwk.util.CoreUtils
import org.slf4j._

class ByteRange(private val _file:RandomAccessFile,
  private var _start:Long, private var _end:Long,
  private val _cType:String, private val _incHeader:Boolean) {

  private var _servedHeader = 0
  private var _servedRange = 0
  private val _header= if ( _incHeader) {
    fmtRangeHeader( _start, _end, _file.length , _cType, "DEFAULT_SEPARATOR"  )
  } else {
    Array[Byte]()
  }

  def start= _start
  def end= _end

  def size() = _end - _start + 1

  def remaining() = _end - _start + 1 - _servedRange

  def computeTotalLengh = size() + _header.length

  def fill(out:Array[Byte] , offset:Int) = {
    var count = 0
    var pos=offset
    while ( pos < out.length && _servedHeader < _header.length ) {
        out(pos) = _header( _servedHeader)
        pos += 1
        _servedHeader += 1
        count += 1
    }

    if ( pos < out.length) {
      _file.seek( _start + _servedRange)
      var maxToRead = if ( remaining() > ( out.length - pos) ) (out.length - pos) else remaining()
      if (maxToRead > Int.MaxValue) {
          maxToRead = Int.MaxValue.toLong
      }
      val c = _file.read( out, pos, maxToRead.toInt)
      if(c < 0) {
          throw new IOException("error while reading file : no more to read ! length=" + _file.length() +
              ", seek=" + ( _start + _servedRange))
      }
      _servedRange += c
      count += c
    }
    count
  }

  private def fmtRangeHeader(start:Long, end:Long, flen:Long, cType:String, boundary:String) = {
    val s= "--" + boundary + "\r\n" + "content-type: " + cType + "\r\n" +
    "content-range: bytes " + start + "-" + end + "/" + flen + "\r\n" +
    "\r\n"
    s.getBytes("utf-8")
  }

}

object HTTPRangeInput {
  def accepts(req:HttpRequest) = req.containsHeader("range")
}

class HTTPRangeInput(private val _file:RandomAccessFile,
  private val _contentType:String,
  private val _req:HttpRequest) extends ChunkedInput {

  private val _log= LoggerFactory.getLogger(classOf[HTTPRangeInput])
  def tlog() = _log

  private var _ranges:Array[ByteRange] = null
  private val _chunkSize = 8096
  private var _currentByteRange = 0
  private var _unsatisfiable = false
  private val _clen= _file.length
  initRanges()

  def prepareNettyResponse(rsp:HttpResponse) {
    rsp.addHeader("accept-ranges", "bytes")
    if (_unsatisfiable) {
      rsp.setStatus(HttpResponseStatus.REQUESTED_RANGE_NOT_SATISFIABLE)
      rsp.setHeader("content-range", "bytes " + "0-" + (_clen-1) + "/" + _clen)
      rsp.setHeader("content-length", 0)
    } else {
      rsp.setStatus(HttpResponseStatus.PARTIAL_CONTENT)
      if(_ranges.length == 1) {
        val r= _ranges(0)
        rsp.setHeader("content-range", "bytes " + r.start + "-" + r.end + "/" + _clen)
      } else {
        rsp.setHeader("content-type", "multipart/byteranges; boundary="+ "DEFAULT_SEPARATOR")
      }
      val len = _ranges.foldLeft(0L) { (rc, r) =>
        rc + r.computeTotalLengh
      }
      rsp.setHeader("content-length", len.toString )
    }
  }

  override def nextChunk() = {

    val buff= new Array[Byte]( _chunkSize )
    var count = 0

    while ( count < _chunkSize && _currentByteRange < _ranges.length &&
        _ranges(_currentByteRange) != null) {
      if ( _ranges(_currentByteRange).remaining() > 0) {
          count += _ranges(_currentByteRange).fill(buff, count)
      } else {
          _currentByteRange += 1
      }
    }
    if (count == 0) null else wrappedBuffer(buff)
  }

  override def hasNextChunk() = {
    _currentByteRange < _ranges.size && _ranges(_currentByteRange).remaining() > 0
  }

  override def isEndOfInput() = !hasNextChunk()

  override def close() {
    _file.close()
  }

  private def initRanges() {
    try {
      val ranges = mutable.ArrayBuffer[ (Long,Long) ]()
      // strip off "bytes="
      val s = CoreUtils.nsb(_req.getHeader("range") )
      val pos= s.indexOf("bytes=")
      val rvs= if (pos == -1) null else {
        s.substring(pos+6).trim().split(",")
      }

      if (rvs != null) rvs.foreach { (rv) =>
        val rs= rv.trim
        var start=0L
        var end=0L
        if (rs.startsWith("-")) {
          start = _clen - 1 -  rs.substring(1).trim.toLong
          end = _clen - 1
        } else {
          val range = rs.split("-")
          start = range(0).trim.toLong
          end = if (range.size > 1) range(1).trim.toLong else _clen - 1
        }
        if(end > (_clen - 1)) { end = _clen - 1 }
        if(start <= end) { ranges.add( ( start, end ) ) }
      }

      val bytes = mutable.ArrayBuffer[ByteRange]()
      val nrs = normalize(ranges.toArray)
      nrs.foreach { (rr) =>
        bytes += new ByteRange( _file, rr._1, rr._2, _contentType, nrs.size > 1)
      }
      _ranges = bytes.toArray
      _unsatisfiable = (_ranges.size == 0)
    } catch {
      case e:Throwable =>
        _unsatisfiable = true
        tlog.error("",e)
    }
  }

  private def maybeIntersect(r1:(Long,Long), r2:(Long,Long) ) = {
    r1._1 >= r2._1 && r1._1 <= r2._2 || r1._2 >= r2._1 && r1._1 <= r2._2
  }

  private def mergeRanges(r1:(Long,Long), r2:(Long,Long) ) = {
    ( if(r1._1 < r2._1) r1._1 else r2._1,
              if (r1._2 > r2._2) r1._2 else r2._2 )
  }

  private def normalize( chunks:Array[(Long,Long)]): Array[(Long,Long) ] = {

    if (chunks.length == 0) { return Array[ (Long,Long) ]() }

    def sortedChunks = Array[(Long,Long)]() ++ chunks

    Arrays.sort(sortedChunks, new Comparator[ (Long,Long)] () {
        def compare( t1:(Long,Long), t2:(Long,Long)) = {
          t1._1.compareTo(t2._1)
        }
    })

    val rc= mutable.ArrayBuffer[(Long,Long)]()
    rc.add(sortedChunks(0))
    for (i <- 1 until sortedChunks.length) {
        val c1 = sortedChunks(i)
        val r1 = rc.get(rc.size() - 1)
        if ( maybeIntersect(c1, r1)) {
            rc.set(rc.size() - 1, mergeRanges(c1, r1))
        } else {
            rc.add(c1)
        }
    }
    rc.toArray
  }

}

