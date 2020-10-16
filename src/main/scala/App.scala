
import java.io.FileNotFoundException

import proto.Util

import scala.io.Source

object App {

  def convert(): Seq[Field] = {
    val appendProto = true
    val lines: Seq[Field] = Source.fromFile("target.in").getLines()
      .toSeq
      .map(x => Util.cleanCodeLine(x)).flatten.zipWithIndex
      .map(p => {
        val o = p._1.split(":")
        val readProtoLine = ("" + Util.typeDefiners(o.last.trim, appendProto) + " "
          + Util.toSnakeCase(o.head.trim)
          + " = " + (p._2 + 1)+";")
        println(readProtoLine)
        Field(Util.toSnakeCase(o.head.trim), Util.typeDefiners(o.last.trim, appendProto), "= " + (p._2 + 1) + ";")
      })
    lines
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
