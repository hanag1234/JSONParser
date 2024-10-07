sealed trait JSONValue
case class JSONObject(pairs: Map[String, JSONValue]) extends JSONValue
case class JSONArray(values: Seq[JSONValue]) extends JSONValue
case class JSONString(value: String) extends JSONValue
case class JSONNumber(value: Double) extends JSONValue
case class JSONBoolean(value: Boolean) extends JSONValue
case object JSONNull extends JSONValue

