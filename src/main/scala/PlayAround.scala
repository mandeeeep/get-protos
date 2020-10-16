import play.api.libs.json.{JsArray, JsNumber, JsObject, Json, __}

object PlayAround {

  val infectedJson = Json.parse("""{"created":100,"invited_at":{"$long": 101010101010}}""")

  val transformer = (__ \ "invited_at" \ "$long").json.pick[JsNumber]

  val transform = __.json.update(
    (__ \ "invited_at").json.put(
      JsNumber(
        (infectedJson \ "invited_at" \ "$long").as[Long]
      )
    )
  )

  //  val transformAnother = (__ \ "invited_at").json.pickBranch(
  //    (__ \ "$long").json.update()
  //  )

  val transformCopy = (__ \ "invited_at").json.copyFrom((__ \ "invited_at" \ "$long").json.pick)
  val a = {
    println(infectedJson.as[JsObject] - "invited_at" ++ infectedJson.transform(transformCopy).get.as[JsObject])
  }

  val name: Seq[String] = Seq("")
  def tosn(name: String) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    "_" + m.group(0).toLowerCase()
  }).toUpperCase()

  //name.map(x=> println("("+"\""+x+"\""+","+tosn(x)+")"))
  name.map(x=> println(tosn(x)))

}
