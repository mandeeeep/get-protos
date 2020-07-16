package graphql

import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec

object GQLUtil {

  def cleanCodeLine(src: String): Option[String] = {
    //first remove comments
    val x = src.replaceAll(";", "")
    val y = removeComments(x).trim
    val z = removeDefaultValues(y).trim
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

  def typeDefiners(src: String): Option[String] = {
    //its guaranteed that src will have either 2 or 3 words
    //2 is a normal field
    //3 is mostly always a list
    //google.protobuf.StringValue field1
    //repeated ReferenceProto list1
    //Splitting proto code lines by whitespace
    val codeParts = src.split(" ")
    if (codeParts.size == 2) {
      Some(typeMappers(codeParts.head))
    } else if (codeParts.size == 3) {
      Some(typeMappers(codeParts.head, Some(cleanProtoTypeNames(codeParts(1)).gqlName)))
    } else {
      None
    }
  }

  def typeMappers(src: String, innerField: Option[String] = None): String = {
    src match {
      case "google.protobuf.StringValue" => ("OptionType(StringType)")
      case "repeated" => "ListType(" + innerField.getOrElse("") + ")"
      case "google.protobuf.Int64Value" => "OptionType(LongType)"
      case "google.protobuf.Int32Value" => "OptionType(IntType)"
      case "google.protobuf.BoolValue" => "OptionType(BooleanType)"
      case "google.protobuf.DoubleValue" => "OptionType(FloatType)"
      case "google.protobuf.FloatValue" => "OptionType(FloatType)"
      case _ => cleanProtoTypeNames(src).gqlName
    }

  }

  def cleanProtoTypeNames(src: String): ClassInfo = {
    //1 = Proto Name
    //2 = Domain Name
    //3 = GQL Name
    val head: Seq[String] = GQLUtil.splitCamelCase(src).split(" ")
    val normal: Boolean = if (head.head.contains("Add") || head.head.contains("Update")) {
      false
    } else {
      true
    }
    val className = if (head.last == "Proto") {
      head.dropRight(1)
    } else {
      head
    }


    //Now we need to check if its a Request/Response/Normal type of class/message
    if (className.size == 1) {
      //Most normal messages will have single words
      //but not always
      ClassInfo(src, className.mkString(""), className.mkString("") + "Type", normal)
    } else {
      //Now we still need to determine if we are dealing with Request/Response/Normal
      //There are special cases in case of domain models with Name Request so ....
      //we try something, which isn't really full proof though
      if (className.last == "Request") {
        val finalName = className.dropRight(1).mkString("") + "Input"
        ClassInfo(src, finalName, finalName + "Type", false)
      } else if (className.last == "Response") {
        ClassInfo(src, className.mkString(""), className.mkString("") + "Type", normal)
      } else {
        if (normal == false) {
          val finalName = className.mkString("") + "Input"
          ClassInfo(src, finalName, finalName + "Type", normal)
        } else {
          ClassInfo(src, className.mkString(""), className.mkString("") + "Type", normal)
        }
      }
    }
  }

  def splitCamelCase(s: String): String = {
    return s.replaceAll(
      String.format("%s|%s|%s",
        "(?<=[A-Z])(?=[A-Z][a-z])",
        "(?<=[^A-Z])(?=[A-Z])",
        "(?<=[A-Za-z])(?=[^A-Za-z])"
      ),
      " "
    ).replaceAll("  ", " ")
  }

  def writeFile(filename: String, s: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s)
    bw.close()
  }

  case class ClassInfo(protoName: String, domainName: String, gqlName: String, normal: Boolean)

  case class GqlOLine(fieldName: String, gqlType: String, description: String, resolver: String)

  case class GqlILine(fieldName: String, gqlType: String, description: String)


}
