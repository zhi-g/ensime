package org.ensime.config
import expectj.{ExpectJ, Spawn, Executor}
import java.io.File
import java.util.regex.Pattern
import scala.util.matching._
import org.ensime.util._
import scala.collection.mutable.ArrayBuffer

case class SbtSubproject(name: String, deps: List[String])

object Sbt extends ExternalConfigurator {

  
  trait SbtVersion {

    def pluginSourceFilename:String

    def pluginFilename:String
    
    def taskName:String

    def getConfigurationDump(baseDir: File, project: Option[String]):Either[Throwable, String]

    def runTask(baseDir: File, jar: File, project: Option[String]): Either[Throwable, String] = {

      val task = project match {
	case Some(p) => "\"" + taskName + " " + p + "\""
	case None => taskName
      }

      val pb = new ProcessBuilder("java", "-Dsbt.log.noformat=true", "-jar",
	jar.getCanonicalPath(), task)
      pb.directory(baseDir)
      ProcessUtil.readAllOutput(pb.start()) match{
	case Right((stdout, stderr)) => Right(stdout)
	case Left(t:Throwable) => Left(t)
      }
    }
  }


  object Sbt10 extends SbtVersion{

    def pluginSourceFilename:String = "EnsimePlugin_Sbt10.scala"

    def pluginFilename:String = "plugin.scala"

    def taskName:String = "ensime-dump-config"

    def getConfigurationDump(baseDir: File, project: Option[String]):Either[Throwable, String] = {
      runTask(baseDir, new File(".", "bin/sbt-launch-0.10.1.jar"), project)
    }

  }


  object Sbt7 extends SbtVersion{

    def pluginSourceFilename:String = "EnsimePlugin_Sbt7.scala"

    def pluginFilename:String = "Ensime_Sbt7_" +  + ".scala"

    def taskName:String = "ensime-dump-config"

    def getConfigurationDump(baseDir: File, project: Option[String]):Either[Throwable, String] = {
      runTask(baseDir, new File(".", "bin/sbt-launch-0.7.7.jar"), project)
    }
  }


  def getSbt(baseDir: File): SbtVersion = {
    val props = JavaProperties.load(new File(baseDir, "project/build.properties"))
    props.get("sbt.version") match {
      case Some(v:String) => {
	if(v.startsWith("0.7")) Sbt7
	else Sbt10
      }
      case None => Sbt10
    }
  }


  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val sbt = getSbt(baseDir)
    ensurePluginExists(baseDir, sbt) match{
      case Right(_) => {
	sbt.getConfigurationDump(
	  baseDir, conf.sbtActiveSubproject.map(_.name)) match{
	  case Right(src) => parseConfig(src, baseDir, conf)
	  case Left(err) => Left(err)
	}
      }
      case Left(err) => Left(err)
    }
  }

  def ensurePluginExists(baseDir: File, 
    sbt: SbtVersion): Either[Throwable, File] = {
    try{

      val plugs = new File(baseDir, "./project/plugins")
      if(!plugs.exists()){
	val success = plugs.mkdir()
	if(!success) throw new RuntimeException(
	  "Failed to create directory: " + plugs)
      }

      val plugin = new File(baseDir, 
	"./project/plugins/" + sbt.pluginFilename)
      if(!plugin.exists()){
	val pluginSrc = new File("./etc/" + sbt.pluginSourceFilename)
	FileUtil.copyFile(pluginSrc, plugin) match{
	  case Left(t) => {
	    t.printStackTrace()
	    throw new RuntimeException(
	      "Failed to copy " + pluginSrc + " to " + plugin)
	  }
	  case _ => {}
	}
      }

      Right(plugin)
    }
    catch{
      case t:Throwable => Left(t)
    }
  }

  def parseConfig(src: String, baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {

    SExp.read(new input.CharArrayReader(buf)) match{
      case Left(errMsg) => Left(new RuntimeException(errMsg))
      case Right(sexp:SExpList) => {
	val name = sexp.getStr("name")
	val org = sexp.getStr("organization")
	val projectVersion = sexp.getStr("project-version")
	val projectVersion = sexp.getStr("scala-build-version")
	val compileDeps = sexp.getStrList("compile-deps")
	val testDeps = sexp.getStrList("test-deps")
	val runtimeDeps = sexp.getStrList("runtime-deps")
	val sourceRoots = sexp.getStrList("source-roots")
	val target = sexp.getStrList("target")

	import FileUtils._
	val compileDepFiles = maybeFiles(compileDeps, baseDir)
	val testDepFiles = maybeFiles(testDeps, baseDir)
	val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir)
	val sourceRootFiles = maybeDirs(sourceRoots, baseDir)
	val f = new File(baseDir, target)
	val targetDir = if (f.exists) { Some(toCanonFile(f)) } else { None }

	Right(ExternalConfig(Some(name), sourceRootFiles,
            runtimeDepFiles, compileDepFiles, testDepFiles,
            targetDir))
      }

      case Right(_) => Left(new RuntimeException(
	  "Failed to parse SExpList from sbt output."))
    }
  }

  def main(args: Array[String]) {
    //    println(getConfig(new File("."), None))
  }
}
