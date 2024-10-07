import scala.io.Source
import scala.util.{Failure, Success}
import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._

object Main{
  val plainParser = new PlainJSONParser
  val packratParser = new PackratJSONParser
  val meerkatParser = new MeerkatJSONParser

  def main(args: Array[String]): Unit = {

    // Print that it is starting the profiling and benchmarking
    println("Running profiling and benchmarking...")

    // Read the specific JSON file
    val json = Source.fromResource("complex10k.json").getLines().mkString

    val parserName = args(0)

    val parser = parserName match {
      case "plain" => timePlainParser _
      case "packrat" => timePackratParser _
      case "meerkat" => timeMeerkatParser _
      case _ => throw new IllegalArgumentException("Invalid parser type")
    }

    println(s"Running benchmarks for $parserName parser...")
    runBenchmark(parserName, () => parser(json))
  }

  def readTestData(name: String): String = {
    Source.fromResource(name).getLines().mkString
  }

  // Function to time the plain parser
  def timePlainParser(json: String): Unit = {
    plainParser.parseAll(plainParser.obj, json)
  }

  // Function to time the packrat parser
  def timePackratParser(json: String): Unit = {
    packratParser.parseAll(packratParser.obj, json)
  }
  // Function to time the Meerkat parser
  def timeMeerkatParser(json: String): Unit = {
    exec(meerkatParser.value, json)
  }

  def compareParsers(input: String): Boolean = {
    val expected = plainParser.parseAll(plainParser.value, input) match {
      case plainParser.Success(result, _) => result
      case _ => ???  // should not happen
    }

    val actual: JSONValue = ??? // run meerkat parser

    expected == actual
  }

  // Error handling
  def parseWithPackrat(input: String): Unit = {
    packratParser.parseAll(packratParser.obj, input) match {
      case packratParser.Success(result, _) =>
        println(s"Packrat parse succeeded: $result")
      case packratParser.Failure(err, _) =>
        println(s"Packrat parse failed: $err")
      case packratParser.Error(err, _) =>
        println(s"Packrat parse error: $err")
    }
  }

  // measure time and memory usage
  def profileParser(parserType: String, json: String, parsingFunc: String => Unit): Unit = {
    println(s"\nProfiling $parserType parser...")
    val memBefore = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val timeTaken = parsingFunc(json)
    val memAfter = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val memUsed = (memAfter - memBefore) / (1024 * 1024) // Convert to MB

    println(s"$parserType parser took $timeTaken ms and used $memUsed MB of memory.")
  }

  // Run warm-up and valid runs for 3 parsers
  def runBenchmark[T](parserName: String, parserFunc: ()  => Unit): Unit = {
    val warmUpRuns = 200
    println(s"Running $warmUpRuns warm-up runs for $parserName parser...")
    1 to warmUpRuns foreach { _ =>
      parserFunc()
    }

    val runs = 500

    val start = System.nanoTime()
    (1 to runs).foreach { _ => parserFunc() }
    val end = System.nanoTime()
    val avgTime = (end - start) / 1000000.0 / runs
    println(f"$parserName average time after warm-up: $avgTime%.3f ms")
  }
}
