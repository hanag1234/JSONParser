import scala.util.parsing.combinator._

class PackratJSONParser extends JavaTokenParsers with PackratParsers {
  lazy val value: PackratParser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
  lazy val obj: PackratParser[Any] = "{" ~ repsep(member, ",") ~ "}"
  lazy val member: PackratParser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
  lazy val arr: PackratParser[Any] = "[" ~ repsep(value, ",") ~ "]"
}