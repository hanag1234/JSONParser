import scala.util.parsing.combinator._

class PlainJSONParser extends JavaTokenParsers {
  def string = stringLiteral ^^ { JSONString.apply }
  def number = floatingPointNumber ^^ { s => JSONNumber(s.toDouble) }
  def boolean = ("true" | "false") ^^ { s => JSONBoolean(s.toBoolean) }
  def nullValue = "null" ^^ { _ => JSONNull }
  def value: Parser[JSONValue] = obj | arr | string | number | boolean | nullValue
  def obj: Parser[JSONObject] = "{" ~> repsep(member, ",") <~ "}" ^^ { pairs => JSONObject(pairs.toMap) }
  def member: Parser[(String, JSONValue)] = stringLiteral ~ (":" ~> value) ^^ { case name ~ value => (name, value) }
  def arr: Parser[JSONArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JSONArray(values) }
}
