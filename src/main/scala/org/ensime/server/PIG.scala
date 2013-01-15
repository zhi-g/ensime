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
import org.ensime.config.ProjectConfig
import org.ensime.indexer.ClassFileIndex
import org.ensime.indexer.LuceneIndex
import org.ensime.protocol.ProtocolConversions

import org.objectweb.asm.ClassReader

import org.neo4j.graphdb.DynamicRelationshipType;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;

import scala.actors._
import scala.actors.Actor._

case class ShutdownReq()

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

  def index(files: Set[File]) {
    doTx { db =>

      val tpe = db.createNode()
      tpe.setProperty(PropName, "MyType")

      val method = db.createNode()
      method.createRelationshipTo(tpe, RelMemberOf)

    }
  }

  def indexDirectories(root: File, sourceRoots: Iterable[File]) {
    index(FileUtils.expandRecursively(
      root, sourceRoots, FileUtils.isValidSourceFile _).toSet)
  }

}

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
    indexDirectories(new File("."), args.map(new File(_)))
  }

}
