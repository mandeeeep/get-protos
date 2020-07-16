package proto

import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec

object Util {

  def toSnakeCase2(str: String): String = {

    val headInUpperCase = str.takeWhile(c => c.isUpper || c.isDigit)
    val tailAfterHeadInUppercase = str.dropWhile(c => c.isUpper || c.isDigit)

    if (tailAfterHeadInUppercase.isEmpty) headInUpperCase.toLowerCase else {
      val firstWord = if (!headInUpperCase.dropRight(1).isEmpty) {
        headInUpperCase.last match {
          case c: Any if (c.isDigit) => headInUpperCase
          case _ => headInUpperCase.dropRight(1).toLowerCase
        }
      } else {
        headInUpperCase.toLowerCase + tailAfterHeadInUppercase.takeWhile(c => c.isLower)
      }

      if (firstWord == str.toLowerCase) {
        firstWord
      } else {
        s"${firstWord}_${toSnakeCase(str.drop(firstWord.length))}"
      }

    }
  }

  def toSnakeCase(str: String): String = {
    @tailrec
    def camel2SnakeRec(s: String, output: String, lastUppercase: Boolean): String =
      if (s.isEmpty) output
      else {
        val c = if (s.head.isUpper && !lastUppercase) "_" + s.head.toLower else s.head.toLower
        camel2SnakeRec(s.tail, output + c, s.head.isUpper && !lastUppercase)
      }

    if (str.forall(_.isUpper)) str.map(_.toLower)
    else {
      camel2SnakeRec(str, "", true)
    }
  }

  def cleanCodeLine(src: String): Option[String] = {
    //first remove comments
    val x = src.replaceAll(",", "").replaceAll("var ", "").replaceAll("val ", "")
    val y = removeComments(x).trim
    val z = removeDefaultValues(y).trim.replaceAll(",", "")
    if (z.isEmpty) {
      None
    } else {
      Some(z)
    }
  }

  def removeComments(src: String) = {
    src.replaceAll(
      "//.*|/\\*((.|\\n)(?!=*/))+\\*/", "")
  }

  def removeDefaultValues(src: String) = {
    src.replaceAll(
      "=.*|/\\*((.|\\n)(?!=*/))+\\*/", "")
  }

  def typeDefiners(src: String, appendProto: Boolean) = {
    val s: Seq[String] = src.replaceAll("\\[", "-")
      .replaceAll("\\]", "")
      .split("-")
    //.filterNot(x => x == "Option")

    if (s.head == "Option") {
      s.filterNot(x => x == "Option")
        .map(o => typeMappers(o, true, appendProto))
        .mkString(" ")
    } else {
      s.filterNot(x => x == "Option")
        .map(o => typeMappers(o, false, appendProto))
        .mkString(" ")
    }
  }

  def typeMappers(src: String, isOptional: Boolean, appendProto: Boolean): String = {
    src match {
      case "String" => if (!isOptional) "string" else "google.protobuf.StringValue"
      case "List" => "repeated"
      case "Seq" => "repeated"
      case "DateTime" => if (!isOptional) "string" else "google.protobuf.StringValue"
      case "Long" => if (!isOptional) "int64" else "google.protobuf.Int64Value"
      case "Int" => if (!isOptional) "int32" else "google.protobuf.Int32Value"
      case "BigDecimal" => if (!isOptional) "string" else "google.protobuf.StringValue"
      case "Boolean" => if (!isOptional) "bool" else "google.protobuf.BoolValue"
      case "Double" => if (!isOptional) "double" else "google.protobuf.DoubleValue"
      case "Float" => if (!isOptional) "float" else "google.protobuf.FloatValue"
      case _ => if (appendProto) src + "Proto" else src
    }

  }

  def writeFile(filename: String, s: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s)
    bw.close()
  }


}
