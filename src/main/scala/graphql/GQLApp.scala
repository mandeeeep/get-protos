package graphql


import java.io.FileNotFoundException

import graphql.GQLUtil.ClassInfo
import proto.Util

import scala.io.Source

object GQLApp extends App {

  def convert(): String = {
    val appendSuffix = true
    val lines: Seq[String] = Source.fromFile("target.in").getLines()
      .toSeq
    //First line is always the class name
    //We aren't over checking the lines, for e.g just took the 2nd item in the array without checking
    //if elements exist
    val dd = lines.head.replaceAll("\\{", "").split(" ")
    val head: ClassInfo = GQLUtil.cleanProtoTypeNames(lines.head.replaceAll("\\{", "").split(" ")(1))
    //println(head.protoName, head.domainName, head.gqlName)
    val body = lines.drop(1).dropRight(1)
    val typee: Seq[String] = body
      .map(x => GQLUtil.cleanCodeLine(x)).flatten
      .map(y => GQLUtil.typeDefiners(y)).flatten

    //    def AvPostType = ObjectType("AvPost",
    //      fields[BaseQuery, AvPostProto](

    val header = "def " + head.gqlName + " = ObjectType(\n" + "\"" + head.domainName + "\",\n" + "\"\",\n" + "fields[BaseQuery, " + head.protoName + "]("
    val fin = Seq(header) ++ typee.init :+ typee.last.dropRight(1) :+ "))"
    fin.mkString("\n")

  }

  println(convert())

  case class Field(fieldName: String, fieldType: String, fieldSerial: String) {
    override def toString() = {
      fieldType + " " + fieldName + " " + fieldSerial
    }
  }

  //  try {
  //    Util.writeFile("source.out", convert().mkString("\n"))
  //  } catch {
  //    case e: FileNotFoundException => {
  //      println("Cannot find target input file!")
  //    }
  //    case e: Exception => {
  //      println("Something went wrong!")
  //    }
  //  }

}
