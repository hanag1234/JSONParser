
import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._

class MeerkatJSONParser{
  val num: Nonterminal = syn("[0-9]+".r)
  val str: Nonterminal = syn("\"[^\"]*\"".r)
  val `null`: Nonterminal = syn("null")
  val bool: Nonterminal = syn("true" | "false")
  val obj: Nonterminal = syn { "{" ~ maybe(keyPair ~ keyPairs) ~ "}" }
  val arr: Nonterminal = syn { "[" ~ maybe(value ~ values) ~ "]" }
  val value: Nonterminal = syn { obj | arr | str | num | bool | `null` }
  def values: Nonterminal = syn {"," ~ value ~ values | epsilon}
  def keyPairs: Nonterminal = syn {"," ~ keyPair ~ keyPairs | epsilon}
  val keyPair: Nonterminal = syn {str ~ ":" ~ value}
  def maybe(p: SequenceBuilder[NoValue]): Nonterminal = syn { p | epsilon }

  // Function to parse
  def parse(input: String): Either[String, Any] = {
    exec(value, input) match {
      case Left(error) =>
        println(s"Meerkat parse error: $error") // Log the error
        Left(s"Parse error: $error")            // Return the error as a Left
      case Right(result) =>
        println(s"Meerkat parse succeeded: $result") // Log the success
        Right(result)                                // Return the successful result as a Right
    }
  }
}