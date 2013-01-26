/**
  *  Copyright (c) 2010, Aemon Cannon
  *  All rights reserved.
  *
  *  Redistribution and use in source and binary forms, with or without
  *  modification, are permitted provided that the following conditions are met:
  *      * Redistributions of source code must retain the above copyright
  *        notice, this list of conditions and the following disclaimer.
  *      * Redistributions in binary form must reproduce the above copyright
  *        notice, this list of conditions and the following disclaimer in the
  *        documentation and/or other materials provided with the distribution.
  *      * Neither the name of ENSIME nor the
  *        names of its contributors may be used to endorse or promote products
  *        derived from this software without specific prior written permission.
  *
  *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
  *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

package org.ensime.server

import java.io.File
import org.ensime.util.FileUtils
import org.ensime.util.Profiling
import org.ensime.config.ProjectConfig
import org.ensime.indexer.ClassFileIndex
import org.ensime.indexer.LuceneIndex
import org.ensime.protocol.ProtocolConversions

import org.objectweb.asm.ClassReader

import org.neo4j.graphdb.DynamicRelationshipType;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import scala.collection.immutable.Vector
import scala.collection.immutable.IndexedSeq

import scala.tools.nsc.interactive.{ CompilerControl, Global }
import scala.tools.nsc.util.{ SourceFile, BatchSourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{Reporter, StoreReporter}
import java.util.concurrent.{Executors, ExecutorService}


import scala.actors._
import scala.actors.Actor._

class RoundRobin[T](items: IndexedSeq[T]) {
  var i = 0
  def next():T = {
    val item = items(i)
    i = (i + 1) % items.length
    item
  }
  def foreach(action: T => Unit) = items.foreach(action)
}

trait PIGIndex {

  val PropName = "name"
  val RelMemberOf = DynamicRelationshipType.withName("memberOf")

  protected def graphDb: GraphDatabaseService

  protected def shutdown = { graphDb.shutdown }

  private def doTx(f: GraphDatabaseService => Unit) = {
    val tx = graphDb.beginTx()
    try {
      f(graphDb)
      tx.success()
    } finally {
      tx.finish()
    }
  }

  case class Die()
  case class ParseFile(f:File)

  class Worker(compiler:Global) extends Actor {
    def act() {
      loop {
        receive {
          case ParseFile(f:File) => {
            val sf = compiler.getSourceFile(f.getAbsolutePath())
            val tree = compiler.parseTree(sf)
            println("parsed " + f)
            doTx { db =>
              //      val tpe = db.createNode()
              //      tpe.setProperty(PropName, "MyType")
              //      val method = db.createNode()
              //      method.createRelationshipTo(tpe, RelMemberOf)
            }
            reply(f)
          }
          case Die() => exit()
        }
      }
    }
  }

  def index(files: Set[File]) {
    val MaxCompilers = 5
    val MaxWorkers = 5
    var i = 0

    def newCompiler():Global = {
      val settings = new Settings(Console.println)
      settings.usejavacp.value = false
      settings.classpath.value = "dist_2.10.0/lib/scala-library.jar"
      val reporter = new StoreReporter()
      new Global(settings, reporter)
    }

    val compilers = new RoundRobin(
      (0 until MaxCompilers).map{i => newCompiler()})

    val workers = new RoundRobin(
      (0 until MaxWorkers).map{i =>
        val w = new Worker(compilers.next())
        w.start()
      })

    val futures = files.map { f => workers.next() !! ParseFile(f) }

    val results = futures.map{f => f()}

    compilers.foreach{ _.askShutdown() }
    workers.foreach { _ ! Die() }
  }

  def indexDirectories(root: File, sourceRoots: Iterable[File]) {
    index(FileUtils.expandRecursively(
      root, sourceRoots, FileUtils.isValidSourceFile _).toSet)
  }

}


case class ShutdownReq()

class PIG(
  project: Project,
  protocol: ProtocolConversions,
  config: ProjectConfig) extends Actor with PIGIndex{

  import org.ensime.protocol.ProtocolConst._
  import protocol._

  var graphDb: GraphDatabaseService = null

  def act() {

    Runtime.getRuntime.addShutdownHook(new Thread() {override def run = shutdown})
    val factory = new GraphDatabaseFactory()
    graphDb = factory.newEmbeddedDatabase("neoj4-db");

    indexDirectories(config.root, config.sourceRoots)
    loop {
      try {
        receive {
          case ShutdownReq() => {
            exit('stop)
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case ImportSuggestionsReq(
                  file: File,
                  point: Int,
                  names: List[String],
                  maxResults: Int) => {
                  //                  val suggestions = ImportSuggestions()
                  //                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(
                  keywords: List[String],
                  maxResults: Int) => {
                  //                  val suggestions = SymbolSearchResults()
                  //                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
              }
            } catch {
              case e: Exception =>
                {
                  System.err.println("Error handling RPC: " +
                    e + " :\n" + e.getStackTraceString)
                  project.sendRPCError(ErrExceptionInIndexer,
                    Some("Error occurred in PIG. Check the server log."),
                    callId)
                }
            }
          }
          case other =>
            {
              println("PIG: WTF, what's " + other)
            }
        }

      } catch {
        case e: Exception => {
          System.err.println("Error at PIG message loop: " +
            e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing Indexer actor.")
  }

}

object PIG extends PIGIndex {
  var graphDb: GraphDatabaseService = null
  def main(args: Array[String]) {
    graphDb = (new GraphDatabaseFactory()).newEmbeddedDatabase("neoj4-db");
    Profiling.time {
      indexDirectories(new File("."), args.map(new File(_)))
    }
  }

}
