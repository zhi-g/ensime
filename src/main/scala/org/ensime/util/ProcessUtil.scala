package org.ensime.util

import java.io._


object ProcessUtil {

  def readAllOutput(proc: Process): Either[Throwable, (String,String)] = {
    try {            
      val errorStream = proc.getErrorStream()
      val errorReaderThread = new StreamReaderThread(errorStream)
      val outputStream = proc.getInputStream()
      val outputReaderThread = new StreamReaderThread(outputStream)
      errorReaderThread.start()
      outputReaderThread.start()
      val exitVal = proc.waitFor()
      errorReaderThread.join()
      outputReaderThread.join()
      errorStream.close()
      outputStream.close()
      if(exitVal != 0) {
	throw new RuntimeException(
	  "Non-zero exit value for sbt process: " + exitVal)
      }
      Right(outputReaderThread.getString(), errorReaderThread.getString())
    } catch {
      case  t: Throwable => Left(t)
    }
  }


  class StreamReaderThread(is: InputStream) extends Thread {
    private val sb = new StringBuffer()
    def getString() = sb.toString()
    
    override def run() {
      try{
	val isr = new InputStreamReader(is);
	val br = new BufferedReader(isr);
	var line = null
	while ((line = br.readLine()) != null) {
	  sb.append(line)
	}
      } catch {
	case ioe: IOException => ioe.printStackTrace()
      }
    }


  }

}
