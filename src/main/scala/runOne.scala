import scala.io.Source
import scala.util.{Failure, Success}
import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.meerkat.util.visualization._
import Parsers._

object runOne {
  val plainParser = new PlainJSONParser
  val packratParser = new PackratJSONParser
  val meerkatParser = new MeerkatJSONParser

  def main(args: Array[String]): Unit = {

    // Print that it is starting the profiling and benchmarking
    println("Running profiling and benchmarking...")

    // Read the specific JSON file
    val json = Source.fromResource("small.json").mkString
    println(json)

    val parserName = args(0)

    val result = parserName match {
      case "plain" => plainParser.parseAll(plainParser.obj, json)
      case "packrat" => packratParser.parseAll(packratParser.obj, json)
      case "meerkat" => {
        val parseTree = parse(meerkatParser.value, json).asSuccess.root
        visualize(parseTree, "sppf")
        exec(meerkatParser.value, json)
      }
      case _ => throw new IllegalArgumentException("Invalid parser type")
    }

    println(result)
  }
}