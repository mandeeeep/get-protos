package json

import play.api.libs.json.{JsArray, JsLookupResult, JsNumber, JsObject, JsValue, Json}


object ConvertPlays  {

  val json = Json.parse(""" { "data": { "lead_value": { "num": 1000 }, "foo": "bar" }, "parb":{"account_value": { "num": 100 } } } """)
  val path = "data/lead_value/num" // json path in string
  val path2 = "parb/account_value/num" // json path in string

  def getNewKey(oldKey: String, newKey: String): String = {
    if (oldKey.nonEmpty) oldKey + "." + newKey else newKey
  }

  def flatten(js: JsValue, prefix: String = ""): JsObject = {
    if (!js.isInstanceOf[JsObject]) return Json.obj(prefix -> js)
    js.as[JsObject].fields.foldLeft(Json.obj()) {
      case (o, (k, value)) => {
        o.deepMerge(value match {
          case x: JsArray => x.as[Seq[JsValue]].zipWithIndex.foldLeft(o) {
            case (o, (n, i: Int)) => o.deepMerge(
              flatten(n.as[JsValue], getNewKey(prefix, k) + s"[$i]")
            )
          }
          case x: JsObject => flatten(x, getNewKey(prefix, k))
          case x => Json.obj(getNewKey(prefix, k) -> x.as[JsValue])
        })
      }
    }
  }

  /**
   * Implicit class defined to accommodate additional Json4s helpers utils for Avenue
   *
   * @param json
   */
  implicit class PlayJsonBigDecimalHelper(json: JsValue) {

    /**
     * Basically converts BigDecimalProxy type to BigDecimal
     *
     * @param list List of path that has BigDecimalProxy type has to be explicitly provided. Individual json path should be
     *             separated by slash "/" for e.g. data/price/num
     */
    def expandBigDecimalProxy(list: Seq[String] = Nil) = {

    }

    json.expandBigDecimalProxy()

  }

}
