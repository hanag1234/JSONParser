import scala.util.parsing.combinator._

//class PlainJSONParser extends JavaTokenParsers {
//  def string = stringLiteral ^^ { JSONString.apply }
//  def number = floatingPointNumber ^^ { s => JSONNumber(s.toDouble) }
//  def boolean = ("true" | "false") ^^ { s => JSONBoolean(s.toBoolean) }
//  def nullValue = "null" ^^ { _ => JSONNull }
//  def value: Parser[JSONValue] = obj | arr | string | number | boolean | nullValue
//  def obj: Parser[JSONObject] = "{" ~> repsep(member, ",") <~ "}" ^^ { pairs => JSONObject(pairs.toMap) }
//  def member: Parser[(String, JSONValue)] = stringLiteral ~ (":" ~> value) ^^ { case name ~ value => (name, value) }
//  def arr: Parser[JSONArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JSONArray(values) }
//}

class PlainJSONParser extends JavaTokenParsers {
  def string: Parser[JSONString] = stringLiteral ^^ { s => JSONString(s.substring(1, s.length - 1)) }
  def number: Parser[JSONNumber] = floatingPointNumber ^^ { s => JSONNumber(s.toDouble) }
  def boolean :Parser[JSONBoolean]= ("true" | "false") ^^ { s => JSONBoolean(s.toBoolean) }
  def nullValue: Parser[JSONNull.type] = "null" ^^ { _ => JSONNull }
  def value: Parser[JSONValue] = obj | arr | string | number | boolean | nullValue
  def obj: Parser[JSONObject] = "{" ~> repsep(member, ",") <~ "}" ^^ { pairs => JSONObject(pairs.toMap) }
  def member: Parser[(String, JSONValue)] = stringLiteral ~ (":" ~> value) ^^ { case name ~ value => (name.substring(1, name.length - 1), value) }
  def arr: Parser[JSONArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JSONArray(values) }

  def parse(input: String): Either[String, JSONValue] = parseAll(value, input) match {
    case Success(result, _) => Right(result)
    case NoSuccess(msg, _) => Left(s"Parse error: $msg")
  }
}

