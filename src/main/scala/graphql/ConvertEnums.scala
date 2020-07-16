package graphql

import graphql.GQLUtil.ClassInfo

import scala.io.Source

object ConvertEnums extends App {
  def convert() = {
    val appendSuffix = true
    val lines: Seq[String] = Source.fromFile("target.in").getLines()
      .toSeq
    //First line is always the class name
    //We aren't over checking the lines, for e.g just took the 2nd item in the array without checking
    //if elements exist
    val head: ClassInfo = GQLUtil.cleanProtoTypeNames(lines.head.split(" ")(1))
    //println(head.protoName, head.domainName, head.gqlName)
    val body = lines.drop(1).dropRight(1)
    val header = "EnumType(" + "\"" + head.gqlName + "\",Some(\"\"), List("
    val ss: String = body
      .map(a => a.replaceAll(";", ""))
      .map(b => GQLUtil.removeComments(b))
      .map(c => GQLUtil.removeDefaultValues(c).trim)
      .map(d => {
        val mid = head.protoName + "." + d
        "EnumValue(" + mid + ".name, value = " + mid + ".name),"
      }).mkString("\n")
    println(header)
    println(ss)
    println("))")
    //    implicit val VisibilityType = {
    //      EnumType("VisibilityType", Some("Visibility Types"), List(
    //        EnumValue(VisibilityTypeProto.VISIBILITY_TYPE_UNKNOWN.name, value = VisibilityTypeProto.VISIBILITY_TYPE_UNKNOWN.name),
    //        EnumValue(VisibilityTypeProto.PRIVATE.name, value = VisibilityTypeProto.PRIVATE.name),
    //        EnumValue(VisibilityTypeProto.EVERYONE.name, value = VisibilityTypeProto.EVERYONE.name),
    //        EnumValue(VisibilityTypeProto.INDIVIDUAL.name, value = VisibilityTypeProto.INDIVIDUAL.name),
    //        EnumValue(VisibilityTypeProto.TEAMS.name, value = VisibilityTypeProto.TEAMS.name)
    //      )
    //      )
    //    }
    Nil
  }

  convert()
}
