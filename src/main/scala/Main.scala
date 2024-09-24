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

    profileParser("Plain", json, timePlainParser)
    profileParser("Packrat", json, timePackratParser)
    profileParser("Meerkat", json, timeMeerkatParser)

    println("\nRunning benchmarks for Plain Parser:")
    runBenchmark("Plain Parser", timePlainParser(json))

    println("\nRunning benchmarks for Packrat Parser:")
    runBenchmark("Packrat Parser", timePackratParser(json))

    println("\nRunning benchmarks for Meerkat Parser:")
    runBenchmark("Meerkat Parser", timeMeerkatParser(json))
  }

  // Function to time the plain parser
  def timePlainParser(json: String): Long = {
    val start = System.nanoTime()
    plainParser.parseAll(plainParser.obj, json)
    val end = System.nanoTime()
    (end - start) / 1000000
  }

  // Function to time the packrat parser
  def timePackratParser(json: String): Long = {
    val start = System.nanoTime()
    packratParser.parseAll(packratParser.obj, json)
    val end = System.nanoTime()
    (end - start) / 1000000
  }
  // Function to time the Meerkat parser
  def timeMeerkatParser(json: String): Long = {
    val start = System.nanoTime()
    exec(meerkatParser.value, json)
    val end = System.nanoTime()
    (end - start) / 1000000
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
  def profileParser(parserType: String, json: String, parsingFunc: String => Long): Unit = {
    println(s"\nProfiling $parserType parser...")
    val memBefore = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val timeTaken = parsingFunc(json)
    val memAfter = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val memUsed = (memAfter - memBefore) / (1024 * 1024) // Convert to MB

    println(s"$parserType parser took $timeTaken ms and used $memUsed MB of memory.")
  }

  // Run warm-up and valid runs for 3 parsers
  def runBenchmark[T](parserName: String, parserFunc: => Long): Unit = {
    val times = (1 to 10).flatMap { run =>
      val timeTaken = parserFunc
      if (run <= 3) {
        println(s"$parserName warm-up run $run: $timeTaken ms")
        None // Discard first 3 runs
      } else {
        println(s"$parserName valid run $run: $timeTaken ms")
        Some(timeTaken)
      }
    }
    val avgTime = times.sum.toDouble / times.length
    println(s"$parserName average time after warm-up: $avgTime ms")
  }
}