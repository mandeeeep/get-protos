
import java.io.FileNotFoundException

import proto.Util

import scala.io.Source

object GetProto {

  def main(args: Array[String]): Unit = {
    val input = args(0)
    val output = args(1)
    val appendProto = args(2)

    try {
      Util.writeFile(output, convert(input, appendProto.toBoolean).mkString("\n"))
    } catch {
      case e: FileNotFoundException => {
        println("Cannot find target input file!")
      }
      case e: Exception => {
        println("Something went wrong!")
      }
    }
  }

  def convert(inputFile: String, appendProto: Boolean): Seq[Field] = {
    val lines: Seq[Field] = Source.fromFile(inputFile).getLines()
      .toSeq
      .map(x => Util.cleanCodeLine(x)).flatten.zipWithIndex
      .map(p => {
        val o = p._1.split(":")
//        val readProtoLine = ("" + Util.typeDefiners(o.last.trim,appendProto) + " "
        //          + Util.toSnakeCase(o.head.trim)
        //          + " = " + (p._2 + 1))
        Field(Util.toSnakeCase(o.head.trim), Util.typeDefiners(o.last.trim, appendProto), "= " + (p._2 + 1) + ";")
      })
    lines
  }

  case class Field(fieldName: String, fieldType: String, fieldSerial: String) {
    override def toString() = {
      fieldType + " " + fieldName + " " + fieldSerial
    }
  }


}
