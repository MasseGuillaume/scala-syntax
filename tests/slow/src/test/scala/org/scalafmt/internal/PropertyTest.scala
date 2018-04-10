package org.scalafmt.internal

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger

import me.tongfei.progressbar.{ProgressBar => PB, ProgressBarStyle}

import org.scalafmt.tests.BaseScalaPrinterTest
import org.scalameta.logger

import scala.meta._
import scala.meta.testkit.Corpus

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

sealed trait PropertyResult
case object Success extends PropertyResult
case class Failure(explanation: String) extends PropertyResult

abstract class PropertyTest(name: String) extends BaseScalaPrinterTest {

  def check(file: Input.File, relativePath: String): PropertyResult

  private val failed = TrieMap.empty[File, Boolean]
  private val regressions = TrieMap.empty[File, Boolean]
  private val nl = "\n"
  private val prefix = "target/repos/"

  private val coverageFile = Paths.get(s"coverage-${name}.txt")
  private val todoFile = Paths.get(s"todo-${name}.diff")

  if (Files.exists(todoFile)) {
    Files.delete(todoFile)
  }

  private val previouslyFailed: Set[File] =
    if (Files.exists(coverageFile)) {
      val input = new String(Files.readAllBytes(coverageFile))
      input.split(nl).filterNot(_ == "").map(f => new File(prefix + f)).toSet
    } else {
      Set()
    }

  private def fileList(in: TrieMap[File, Boolean], sep: String): String =
    in.keys
      .map(_.toString.drop(prefix.size))
      .toList
      .sorted
      .mkString("", sep, sep)

  val doit = Set(
    "akka/akka-distributed-data/src/test/scala/akka/cluster/ddata/ORSetSpec.scala",
    // "akka/akka-docs/rst/scala/code/docs/http/scaladsl/MarshalSpec.scala",
    // "akka/akka-docs/rst/scala/code/docs/http/scaladsl/server/directives/ExecutionDirectivesExamplesSpec.scala",
    // "akka/akka-docs/rst/scala/code/docs/http/scaladsl/UnmarshalSpec.scala",
    // "akka/akka-http-core/src/main/scala/akka/http/impl/model/parser/UriParser.scala",
    // "akka/akka-http-core/src/test/scala/akka/http/impl/engine/client/ConnectionPoolSpec.scala",
    // "akka/akka-http-core/src/test/scala/akka/http/impl/engine/client/HttpConfigurationSpec.scala",
    // "akka/akka-http-core/src/test/scala/akka/http/impl/engine/ws/EchoTestClientApp.scala",
    // "akka/akka-http-core/src/test/scala/akka/http/impl/util/One2OneBidiFlowSpec.scala",
    // "akka/akka-http-core/src/test/scala/akka/http/scaladsl/TightRequestTimeoutSpec.scala",
    // "akka/akka-persistence-query/src/test/scala/akka/persistence/query/journal/leveldb/EventsByPersistenceIdSpec.scala",
    // "akka/akka-persistence-query/src/test/scala/akka/persistence/query/journal/leveldb/EventsByTagSpec.scala",
    // "akka/akka-persistence/src/test/scala/akka/persistence/PersistentActorDeleteFailureSpec.scala",
    // "akka/akka-remote/src/test/scala/akka/remote/DeadlineFailureDetectorSpec.scala",
    // "akka/akka-remote/src/test/scala/akka/remote/FailureDetectorRegistrySpec.scala",
    // "akka/akka-remote/src/test/scala/akka/remote/serialization/ProtobufSerializerSpec.scala",
    // "akka/akka-stream-tests/src/test/scala/akka/stream/scaladsl/ActorRefSourceSpec.scala",
    // "akka/akka-stream-tests/src/test/scala/akka/stream/scaladsl/FlowSpec.scala",
    // "akka/akka-stream-tests/src/test/scala/akka/stream/scaladsl/FlowSplitWhenSpec.scala",
    // "akka/akka-typed/src/test/scala/akka/typed/ActorContextSpec.scala",
    // "ensime-server/s-express/src/main/scala/org/ensime/sexp/SexpParser.scala",
    // "ensime-server/s-express/src/test/scala/org/ensime/sexp/formats/CollectionFormatsSpec.scala",
    // "finagle/finagle-example/src/main/scala/com/twitter/finagle/example/kestrel/KestrelClient.scala",
    // "framework/core/json/src/main/scala/net/liftweb/json/Meta.scala",
    // "framework/core/util/src/main/scala/net/liftweb/util/EnumWithDescription.scala",
    // "lila/modules/timeline/src/main/Env.scala",
    // "marathon/src/test/scala/mesosphere/marathon/core/task/tracker/impl/TaskOpProcessorImplTest.scala",
    // "marathon/src/test/scala/mesosphere/marathon/integration/ArtifactsIntegrationTest.scala",
    // "marathon/src/test/scala/mesosphere/marathon/integration/TaskQueueIntegrationTest.scala",
    // "platform/bifrost/src/main/scala/com/precog/bifrost/PerAccountThreadPoolModule.scala",
    // "playframework/documentation/manual/working/scalaGuide/main/tests/code/tests/guice/ScalaGuiceApplicationBuilderSpec.scala",
    // "playframework/framework/src/play-integration-test/src/test/scala/play/it/http/ScalaResultsHandlingSpec.scala",
    // "playframework/framework/src/play/src/test/scala/play/api/inject/guice/GuiceInjectorBuilderSpec.scala",
    // "scala/src/interactive/scala/tools/nsc/interactive/tests/core/PresentationCompilerRequestsWorkingMode.scala",
    // "scala/test/files/pos/t4547.scala",
    // "scalatra/core/src/test/scala/org/scalatra/RouteConcurrencySpec.scala",
    // "spark/core/src/test/scala/org/apache/spark/status/api/v1/SimpleDateParamSuite.scala",
    // "util/util-logging/src/test/scala/com/twitter/logging/QueueingHandlerTest.scala"
  )
   
  test(name) {
    val failureCount = new AtomicInteger(0)
    val successCount = new AtomicInteger(0)

    val corpus = Corpus
      .files(Corpus.fastparse)
      .filter{f => 
        val file = f.jFile.toString.drop("target/repos/".size)
        doit.contains(file)
      }
      .toBuffer
      .par

    val progress = new PB(
      "Formatting",
      corpus.size,
      1000,
      System.out,
      ProgressBarStyle.UNICODE_BLOCK
    )
    progress.start()

    SyntaxAnalysis.run[Unit](corpus) { file =>
      try {
        val jFile = file.jFile
        val input = Input.File(jFile, StandardCharsets.UTF_8)
        val relativePath = "tests/slow/" + jFile.toString
        // println(jFile.toString)

        check(input, relativePath) match {
          case Success => successCount.incrementAndGet()
          case Failure(explanation) => {
            val failures = failureCount.incrementAndGet()
            failed += jFile -> true

            if (failures < 100) {
              logger.elem(explanation)
            }

            if (!previouslyFailed.contains(file.jFile)) {
              regressions += file.jFile -> true
              print(Console.RED)
              println("*************************")
              println("Regression: " + file.jFile)
              println("*************************")
              print(Console.RESET)
            } else {
              Files.write(
                todoFile,
                (explanation + nl).toString.getBytes("utf-8"),
                StandardOpenOption.CREATE,
                StandardOpenOption.APPEND
              )
            }
          }
        }
      } catch {
        case NonFatal(_) => ()
      }

      progress.synchronized {
        progress.step()

        val currentFailures = failureCount.get
        val currentSuccess = successCount.get

        val progressMessage =
          if (currentFailures > 100) {
            val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
            f"Success: ${rate}%.2f%%"
          } else {
            s"Failures: $currentFailures"
          }

        progress.setExtraMessage(progressMessage)
      }
    }

    progress.stop()

    if (regressions.isEmpty) {
      Files.write(
        coverageFile,
        fileList(failed, nl).getBytes("utf-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    if (regressions.nonEmpty) {
      val sep = nl + "  "
      val regressionList = fileList(regressions, sep)
      sys.error("Regressions:" + sep + regressionList)
    }
  }
}
