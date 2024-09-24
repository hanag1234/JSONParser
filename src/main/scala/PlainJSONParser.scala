import scala.util.parsing.combinator._

class PlainJSONParser extends JavaTokenParsers {
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
  def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
}
