package json

import json.Convert4s.path
import org.json4s._
import org.json4s.native.JsonMethods._

//https://stackoverflow.com/questions/45240109/jsonpath-and-json4s
//https://stackoverflow.com/questions/39378000/update-json-values-with-json4s-for-a-json-path
//https://stackoverflow.com/questions/42856002/convert-jvalue-to-json-string
//https://stackoverflow.com/questions/39127039/how-do-i-generate-pretty-json-with-json4s

object Convert4s extends App {

  val json = parse(""" { "data": { "lead_value": { "num": 1000 }, "foo": "bar" }, "parb":[{"account_value": { "num": 100 } }] } """)
  val path = "data/lead_value/num" // json path in string
  val path2 = "parb/account_value/num" // json path in string

  /**
   * Implicit class defined to accommodate additional Json4s helpers utils for Avenue
   * @param json
   */
  implicit class Json4sBigDecimalHelper(json: JValue) {

    /**
     * Basically converts BigDecimalProxy type to BigDecimal
     * @param list List of path that has BigDecimalProxy type has to be explicitly provided. Individual json path should be
     *             separated by slash "/" for e.g. data/price/num
     */
    def compressBigDecimalProxy(list: Seq[String] = Nil) = {
      implicit val formats = DefaultFormats

      def decimalFixerProto(jsonE: JValue, path: String, decimalValue: JDecimal): JValue = {
        jsonE.replace(path.split("/").toList.init, decimalValue)
      }

      val finalJValue = list.foldLeft(json)((p, i) => {
        p.customExtract[String](i).map(hasValue => {
          decimalFixerProto(p, i, JDecimal(BigDecimal(hasValue)))
        }).getOrElse(p)
      })
      println(compact(render(finalJValue)))
    }

    def customExtract[T](path: String)(implicit formats: Formats, mf: Manifest[T]) = {
      path.split('/').foldLeft(json)({ case (acc: JValue, node: String) => acc \ node }).extractOpt[T]
    }

  }

  json.compressBigDecimalProxy(Seq(path2))

}
