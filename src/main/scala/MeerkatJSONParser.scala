import org.meerkat.Syntax._
import org.meerkat.parsers.{Seq => _, _}
import Parsers._

class MeerkatJSONParser {

  // JSONNumber AST nodes
  val num: Nonterminal & JSONNumber = syn(
    "[0-9]+".r ^ ((s: String) => JSONNumber(s.toDouble))
  )

  // JSONString AST nodes
  val str: Nonterminal & JSONString = syn(
    "\"[^\"]*\"".r ^ ((s: String) => JSONString(s.substring(1, s.length - 1)))
  )

  // JSONNull AST node
  val `null`: Nonterminal & JSONNull.type = syn("null" & (_ => JSONNull))

  // JSONBoolean AST nodes
  val bool: Nonterminal & JSONBoolean = syn(
    "true" & (_ => JSONBoolean(true))
    | "false" & (_ => JSONBoolean(false))
  )

  // Semantic action for building empty sequences out of literals
  def emptySeqF[T] = (_:String) => Seq.empty[T]

  // This is an alternative to the tuple type because the `convert` function in SPPFVisitor rewrites tuples.
  // In general, we cannot have `()` or pairs as parser return types (even if they are inside sequences).
  //
  // This is a Meerkat bug, so we should modify it to use custom types rather than tuples.
  case class T[+A,+B](_1: A, _2: B) {
    def toTuple: (A, B) = (_1, _2)
  }

  // JSON objects
  val keyPair: Nonterminal & T[String, JSONValue] = syn (
    (str ~ ":" ~ value) & { case key ~ v => T(key.value, v) }
  )

  // JSONObject AST nodes
  val obj: Nonterminal & JSONObject = syn (
    ("{" ~ maybeKeyPairs ~ "}") & {
      case Some(firstPair ~ restPairs) => {
        JSONObject((firstPair +: restPairs).map(_.toTuple).toMap)
      }
      case None => JSONObject(Map.empty)
    }
  )

  // JSONArray AST nodes
  val arr: Nonterminal & JSONArray = syn {
    ("[" ~ maybeValues ~ "]") & {
      case Some(firstValue ~ restValues) =>
        JSONArray(firstValue +: restValues)
      case None => JSONArray(Seq.empty)
    }
  }
  // Values: objects, arrays, strings, numbers, booleans, or null
  val value: Nonterminal & JSONValue = syn {
    obj | arr | str | num | bool | `null`
  }

  // csv in arrays
  val values: Nonterminal & Seq[JSONValue] = syn {
    ("," ~ value ~ values & { case v ~ vs => v +: vs }) | (epsilon ^ emptySeqF)
  }

  // csv pairs in objects
  val keyPairs: Nonterminal & Seq[T[String, JSONValue]] = syn {
    ("," ~ keyPair ~ keyPairs & { case kp ~ kps => {
      kp +: kps }}) | (epsilon ^ emptySeqF)
  }

  val maybeKeyPairs = syn (
    (keyPair ~ keyPairs) & { Some(_) }
    | epsilon ^ { case _ => None }
  )
  val maybeValues = syn {
    (value ~ values) & { Some(_) } | epsilon ^ { case _ => None }
  }

  def parse(input: String): Either[String, JSONValue] = {
    exec(value, input) match {
      case Left(error) =>
        println(s"Meerkat parse error: $error") // Log the error
        Left(s"Parse error: $error") // Return the error as a Left
      case Right(result) =>
        println(s"Meerkat parse succeeded: $result") // Log the success
        Right(result)
    }
  }
}
