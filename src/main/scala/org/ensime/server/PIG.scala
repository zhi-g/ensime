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
import org.ensime.indexer.Tokens
import org.ensime.protocol.ProtocolConversions
import org.neo4j.cypher.ExecutionEngine
import org.neo4j.helpers.collection.IteratorUtil

import org.objectweb.asm.ClassReader

import org.neo4j.graphdb.DynamicRelationshipType
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.index.Index
import org.neo4j.graphdb.index.IndexHits
import org.neo4j.graphdb.index.IndexManager
import org.neo4j.helpers.collection.MapUtil
import scala.collection.immutable.Vector
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayStack

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
  val PropNameTokens = "nameTokens"
  val PropPath = "path"
  val PropNodeType = "nodeType"

  val NodeTypeFile = "file"
  val NodeTypePackage = "package"
  val NodeTypeType = "type"
  val NodeTypeMethod = "method"
  val NodeTypeImport = "import"
  val NodeTypeParam = "param"
  val NodeTypeVarDef = "varDef"
  val NodeTypeValDef = "valDef"
  val NodeTypeVarField = "varField"
  val NodeTypeValField = "valField"

  val RelMemberOf = DynamicRelationshipType.withName("memberOf")
  val RelInFile = DynamicRelationshipType.withName("inFile")
  val RelContainedBy = DynamicRelationshipType.withName("containedBy")
  val RelParamOf = DynamicRelationshipType.withName("paramOf")
  val RelFileOf = DynamicRelationshipType.withName("fileOf")

  protected def graphDb: GraphDatabaseService
  protected def fileIndex : Index[Node]
  protected def tpeIndex : Index[Node]

  protected def createDefaultGraphDb =
    (new GraphDatabaseFactory()).newEmbeddedDatabase("neo4j-db")

  protected def createDefaultFileIndex(db: GraphDatabaseService) =
    db.index().forNodes("fileIndex")

  protected def createDefaultTypeIndex(db: GraphDatabaseService) =
    db.index().forNodes("tpeIndex",
      MapUtil.stringMap( IndexManager.PROVIDER, "lucene", "type", "fulltext" )
    )

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

  private def foreachHit(hits: IndexHits[Node]) (f: Node => Unit) = {
    import scala.collection.JavaConversions
    try {
      JavaConversions.iterableAsScalaIterable(hits).foreach(f)
    } finally {
      hits.close()
    }
  }

  private def findFileNode(db: GraphDatabaseService, f: File): Node = {
    import scala.collection.JavaConversions
    val fileNode = JavaConversions.iterableAsScalaIterable(
      fileIndex.get("path", f.getAbsolutePath)).headOption
    fileNode match {
      case Some(node) => node
      case None => {
        val node = db.createNode()
        val refNode = db.getReferenceNode
        node.createRelationshipTo(refNode, RelFileOf)
        println("reference ref node form: " + f)
        node.setProperty(PropPath, f.getAbsolutePath)
        node.setProperty(PropNodeType, NodeTypeFile)
        fileIndex.putIfAbsent(node, "path", f.getAbsolutePath)
        node
      }
    }
  }

  trait Extensions { self: Global =>
    import scala.tools.nsc.symtab.Flags._
    import scala.tools.nsc.util.RangePosition
    class TreeTraverser(f:File, db:GraphDatabaseService) extends Traverser {


      override def traverse(t: Tree) {

        val fileNode = findFileNode(db, f)
        val stack = new ArrayStack[Node]
        stack.push(fileNode)

        val treeP = t.pos
        if (!treeP.isTransparent) {
          try {
            t match {

              case PackageDef(pid, stats) => {
                val node = db.createNode()
                node.setProperty(PropNodeType, NodeTypePackage)
                node.createRelationshipTo(stack.top, RelContainedBy)
                stack.push(node)
                super.traverse(t)
                stack.pop()
              }

              case Import(expr, selectors) => {
                for (impSel <- selectors) {
                  val node = db.createNode()
                  node.setProperty(PropNodeType, NodeTypeImport)
                  node.setProperty(PropName, impSel.name.decode)
                  node.createRelationshipTo(stack.top, RelContainedBy)
                }
              }

              case ClassDef(mods, name, tparams, impl) => {
                val localName = name.decode
                val node = db.createNode()
                node.setProperty(PropNodeType, NodeTypeType)
                node.setProperty(PropName, localName)
                node.createRelationshipTo(stack.top, RelContainedBy)
//                tpeIndex.putIfAbsent(node, PropName, localName)
                tpeIndex.add(
                  node, PropNameTokens, Tokens.tokenizeCamelCaseName(localName))
                stack.push(node)
                super.traverse(t)
                stack.pop()
              }

              case ModuleDef(mods, name, impl) => {
                val localName = name.decode
                val node = db.createNode()
                node.setProperty(PropNodeType, NodeTypeType)
                node.setProperty(PropName, name.decode)
                node.createRelationshipTo(stack.top, RelContainedBy)
//                tpeIndex.putIfAbsent(node, PropName, localName)
                tpeIndex.add(
                  node, PropNameTokens, Tokens.tokenizeCamelCaseName(localName))
                stack.push(node)
                super.traverse(t)
                stack.pop()
              }

              case ValDef(mods, name, tpt, rhs) => {
                val node = db.createNode()
                node.setProperty(PropNodeType, NodeTypeType)
                node.setProperty(PropName, name.decode)
                node.createRelationshipTo(stack.top, RelContainedBy)
                val isField =
                  stack.top.getProperty(PropNodeType) == NodeTypeType
                if (mods.hasFlag(PARAM)) {
                  node.setProperty(PropNodeType, NodeTypeParam)
                  node.createRelationshipTo(stack.top, RelParamOf)
                } else if (mods.hasFlag(MUTABLE) && !isField) {
                  node.setProperty(PropNodeType, NodeTypeVarDef)
                } else if (!isField) {
                  node.setProperty(PropNodeType, NodeTypeValDef)
                } else if (mods.hasFlag(MUTABLE) && isField) {
                  node.setProperty(PropNodeType, NodeTypeVarField)
                } else if (isField) {
                  node.setProperty(PropNodeType, NodeTypeValField)
                }
                //stack.push(node)
                //super.traverse(t)
                //stack.pop()
              }

              case DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                val node = db.createNode()
                node.setProperty(PropNodeType, NodeTypeMethod)
                node.setProperty(PropName, name.decode)
                node.createRelationshipTo(stack.top, RelContainedBy)
                stack.push(node)
                super.traverse(t)
                stack.pop()
              }

              case TypeDef(mods, name, tparams, rhs) => {
              }

              case LabelDef(name, params, rhs) => {
              }

              case _ => { super.traverse(t) }
            }
          }
          catch{
            case e : Throwable => {
              System.err.println("Error in AST traverse:")
              e.printStackTrace(System.err)
            }
          }
        }
      }
    }
  }

  case class Die()
  case class ParseFile(f:File)

  class Worker(compiler:Global with Extensions) extends Actor {
    def act() {
      loop {
        receive {
          case ParseFile(f:File) => {
            val sf = compiler.getSourceFile(f.getAbsolutePath())
            val tree = compiler.parseTree(sf)
            println("parsed " + f)
            doTx { db =>
              val traverser = new compiler.TreeTraverser(f, db)
              traverser.traverse(tree)
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

    def newCompiler(): Global with Extensions = {
      val settings = new Settings(Console.println)
      settings.usejavacp.value = false
      settings.classpath.value = "dist_2.10.0/lib/scala-library.jar"
      val reporter = new StoreReporter()
      new Global(settings, reporter) with Extensions {}
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

  def indexDirectories(sourceRoots: Iterable[File]) {
    index(FileUtils.expandRecursively(
      new File("."), sourceRoots, FileUtils.isValidSourceFile _).toSet)
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
  var fileIndex: Index[Node] = null
  var tpeIndex: Index[Node] = null

  def act() {
    val factory = new GraphDatabaseFactory()
    graphDb = createDefaultGraphDb
    fileIndex = createDefaultFileIndex(graphDb)
    tpeIndex = createDefaultTypeIndex(graphDb)
    indexDirectories(config.sourceRoots)
    loop {
      try {
        receive {
          case ShutdownReq() => {
            graphDb.shutdown()
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
                  // Let's execute a query now
                  val engine = new ExecutionEngine(graphDb);
                  val result = engine.execute(
                    "START n=node:tpeIndex(\"nameTokens:class\") MATCH n-[:containedBy*1..5]->container WHERE container.nodeType=\"file\" RETURN n,container")
                  val n_column = result.columnAs("n");
                  for (node:Node <- n_column) {
                    // note: we're grabbing the name property from the node,
                    // not from the n.name in this case.
                    println(node + ": " + node.getProperty( "name" ));
                  }
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
  var graphDb: GraphDatabaseService = createDefaultGraphDb
  var fileIndex: Index[Node] = createDefaultFileIndex(graphDb)
  var tpeIndex: Index[Node] = createDefaultTypeIndex(graphDb)

  def main(args: Array[String]) {
    System.setProperty("actors.corePoolSize", "10")
    System.setProperty("actors.maxPoolSize", "100")
    Profiling.time {
      indexDirectories(args.map(new File(_)))
    }
    graphDb.shutdown()
  }

}
