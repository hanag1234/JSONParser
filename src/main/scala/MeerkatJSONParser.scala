
import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._

//class MeerkatJSONParser{
//  //val num: Nonterminal = syn("[0-9]+".r)
//  val num: Nonterminal = syn("[0-9]+".r) & { case s: String => JSONNumber(s.toDouble) }
//  //val str: Nonterminal = syn("\"[^\"]*\"".r)
//  val str: Nonterminal = syn("\"[^\"]*\"".r) & { case s: String => JSONString(s.substring(1, s.length - 1)) }
//  //val `null`: Nonterminal = syn("null")
//  val `null`: Nonterminal = syn("null") & { _ => JSONNull }
//  //val bool: Nonterminal = syn("true" | "false")
//  val bool: Nonterminal = syn("true" | "false") & { case s: String => JSONBoolean(s.toBoolean) }
//  //val obj: Nonterminal = syn { "{" ~ maybe(keyPair ~ keyPairs) ~ "}" }
//  val obj: Nonterminal = syn { "{" ~ maybe(keyPair ~ keyPairs) ~ "}" } & {
//    case _ ~ pairsOpt ~ _ =>
//      pairsOpt match {
//        case Some(pair ~ rest) => JSONObject((Seq(pair) ++ rest).toMap)
//        case None              => JSONObject(Map.empty)
//      }
//  }
//  //val arr: Nonterminal = syn { "[" ~ maybe(value ~ values) ~ "]" }
//  val arr: Nonterminal = syn { "[" ~ maybe(value ~ values) ~ "]" } & {
//    case _ ~ Some(firstValue ~ restValues) ~ _ => JSONArray(firstValue +: restValues)
//    case _ ~ None ~ _                          => JSONArray(Seq.empty)
//  }
//  val value: Nonterminal = syn { obj | arr | str | num | bool | `null` }
//  //def values: Nonterminal = syn {"," ~ value ~ values | epsilon}
//  def values: Nonterminal = syn { "," ~ value ~ values | epsilon } & { case _ ~ v ~ vs => v +: vs }
//  //def keyPairs: Nonterminal = syn {"," ~ keyPair ~ keyPairs | epsilon}
//  def keyPairs: Nonterminal = syn { "," ~ keyPair ~ keyPairs | epsilon } & {
//    case _ ~ kp ~ kps => kp +: kps
//    case _            => Seq.empty
//  }
//  //val keyPair: Nonterminal = syn {str ~ ":" ~ value}
//  val keyPair: Nonterminal = syn { str ~ ":" ~ value } & { case key ~ _ ~ v => key.value -> v }
//  def maybe(p: SequenceBuilder[NoValue]): Nonterminal = syn { p | epsilon }
//
//  // Function to parse
//  def parse(input: String): Either[String, Any] = {
//    exec(value, input) match {
//      case Left(error) =>
//        println(s"Meerkat parse error: $error") // Log the error
//        Left(s"Parse error: $error")            // Return the error as a Left
//      case Right(result) =>
//        println(s"Meerkat parse succeeded: $result") // Log the success
//        Right(result)                                // Return the successful result as a Right
//    }
//  }
//}



class MeerkatJSONParser {

  // JSONNumber AST nodes
  val num: Nonterminal & JSONNumber = syn("[0-9]+".r) & ((s: String) => JSONNumber(s.toDouble))
  //val num: Nonterminal & JSONNumber = syn("[0-9]+".r) ^ ((s: String) => JSONNumber(s.toDouble))\

  // JSONString AST nodes
  val str: Nonterminal & JSONString = syn("\"[^\"]*\"".r) & ((s: String) => JSONString(s.substring(1, s.length - 1)))

  // JSONNull AST node
  val `null`: Nonterminal & JSONNull.type = syn("null") & (_ => JSONNull)

  // JSONBoolean AST nodes
  val bool: Nonterminal & JSONBoolean = syn("true" | "false") & ((s: String) => JSONBoolean(s.toBoolean))

  // JSON objects
  val keyPair: Nonterminal & (String, JSONValue) = syn { str ~ ":" ~ value } & { case key ~ _ ~ v => key.value -> v }

  // JSONObject AST nodes
  val obj: Nonterminal & JSONObject = syn { "{" ~ maybe(keyPair ~ keyPairs) ~ "}" } & {
    case _ ~ Some(firstPair ~ restPairs) ~ _ => JSONObject((Seq(firstPair) ++ restPairs).toMap)
    case _ ~ None ~ _                        => JSONObject(Map.empty)
  }

  // JSONArray AST nodes
  val arr: Nonterminal & JSONArray = syn { "[" ~ maybe(value ~ values) ~ "]" } & {
    case _ ~ Some(firstValue ~ restValues) ~ _ => JSONArray(firstValue +: restValues)
    case _ ~ None ~ _                          => JSONArray(Seq.empty)
  }
  // Values: objects, arrays, strings, numbers, booleans, or null
  val value: Nonterminal & JSONValue = syn { obj | arr | str | num | bool | `null` }

  //csv in arrays
  def values: Nonterminal & Seq[JSONValue] = syn { "," ~ value ~ values | epsilon } & {
    case _ ~ v ~ vs => v +: vs
    case _          => Seq.empty
  }

  // csv pairs in objects
  def keyPairs: Nonterminal & Seq[(String, JSONValue)] = syn { "," ~ keyPair ~ keyPairs | epsilon } & {
    case _ ~ kp ~ kps => kp +: kps
    case _            => Seq.empty
  }

  def maybe(p: SequenceBuilder[NoValue]): Nonterminal = syn { p | epsilon }

  def parse(input: String): Either[String, JSONValue] = {
    exec(value, input) match {
      case Left(error) =>
        println(s"Meerkat parse error: $error") // Log the error
        Left(s"Parse error: $error")            // Return the error as a Left
      case Right(result) =>
        println(s"Meerkat parse succeeded: $result") // Log the success
        Right(result.asInstanceOf[ParseSuccess].value) // Return the successful result as a Right
    }
  }
}