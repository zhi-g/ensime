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
import scala.io.Codec
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util.NoPosition
import scala.util.parsing.input.CharSequenceReader
import play.templates.ScalaTemplateCompiler._
import play.templates.TemplateCompilationError
import org.ensime.util.FileUtils._
import org.ensime.util.PresentationReporter
import scala.tools.nsc.util.{ SourceFile, BatchSourceFile }
import scala.tools.nsc.util.Position

object PlayTemplates {

  def generateSources(reporter: PresentationReporter,
    templates: Iterable[File]): List[SourceFile] = {
    (for (t <- templates) yield {
      createSourceFile(t) match {
        case Right(sf) => Some(sf)
        case Left((line, col, msg)) => {
          reporter.error(t.getPath, line, col, msg)
          None
        }
      }
    }).flatten.toList
  }

  val DefaultTemplateImports = List(
    "models._",
    "controllers._",
    "play.api.i18n._",
    "play.api.mvc._",
    "play.api.data._",
    "views.%format%._")

  def createSourceFile(source: File): Either[(Int, Int, String), SourceFile] = {
    val (templateName, generatedSource) = generatedFile(
      source, source.getParentFile, new File("."))
    val extension = fileExtension(source)
    val (resultType, formatterType) = extension match {
      case "xml" => ("play.api.templates.Xml", "play.api.templates.XmlFormat")
      case "txt" => ("play.api.templates.Txt", "play.api.templates.TxtFormat")
      case _ => ("play.api.templates.Html", "play.api.templates.HtmlFormat")
    }
    implicit val codec = Codec("UTF-8")
    readFile(source) match {
      case Right(contents) => {
        templateParser.parser(
          new CharSequenceReader(contents)) match {
            case templateParser.Success(parsed, rest) if rest.atEnd => {
              val finalContents = generateFinalTemplate(
                source,
                templateName.dropRight(1).mkString("."),
                templateName.takeRight(1).mkString,
                parsed,
                resultType,
                formatterType,
                DefaultTemplateImports.map("import " +
                  _.replace("%format%", extension)).mkString("\n"))
              println(finalContents)
              Right(new BatchSourceFile(
                source.getAbsolutePath, finalContents))
            }
            case templateParser.Success(_, rest) => {
              Left((rest.pos.line, rest.pos.column, "Template parse failed."))
            }
            case templateParser.NoSuccess(message, input) => {
              Left((input.rest.pos.line, input.rest.pos.column,
                "Template parse failed. " + message))
            }
            case _ => Left((-1, -1, "Unexpected parser error."))
          }
      }
      case Left(ioException) => Left((-1, -1, ioException.toString))
    }
  }
}
