package graphql


import java.io.FileNotFoundException

import graphql.GQLUtil.ClassInfo
import proto.Util

import scala.io.Source

object GQLApp {

  def convert(): Seq[Field] = {
    val appendSuffix = true
    val lines: Seq[String] = Source.fromFile("target.in").getLines()
      .toSeq
    //First line is always the class name
    //We aren't over checking the lines, for e.g just took the 2nd item in the array without checking
    //if elements exist
    val head: ClassInfo = GQLUtil.cleanProtoTypeNames(lines.head.split(" ")(1))
    println(head.protoName,head.domainName,head.gqlName)
    val body = lines.drop(1).dropRight(1)
    val ss: String = body
      .map(x => GQLUtil.cleanCodeLine(x)).flatten
      .map(y => GQLUtil.typeDefiners(y)).flatten.mkString("\n")

    println(ss)
    Nil
  }

  case class Field(fieldName: String, fieldType: String, fieldSerial: String) {
    override def toString() = {
      fieldType + " " + fieldName + " " + fieldSerial
    }
  }

  try {
    Util.writeFile("source.out", convert().mkString("\n"))
  } catch {
    case e: FileNotFoundException => {
      println("Cannot find target input file!")
    }
    case e: Exception => {
      println("Something went wrong!")
    }
  }

}
