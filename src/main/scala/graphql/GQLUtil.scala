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
      Some(typeMappers(codeParts.head, None, codeParts.last))
    } else if (codeParts.size == 3) {
      Some(typeMappers(codeParts.head, Some(cleanProtoTypeNames(codeParts(1)).gqlName), codeParts.last))
    } else {
      None
    }
  }

  def typeMapperss(src: String, innerField: Option[String] = None, fieldName: String): String = {
    src match {
      case "google.protobuf.StringValue" => ("OptionType(StringType)  " + fieldName)
      case "repeated" => "ListType(" + innerField.getOrElse("") + ")" + fieldName
      case "google.protobuf.Int64Value" => "OptionType(LongType)" + fieldName
      case "google.protobuf.Int32Value" => "OptionType(IntType)" + fieldName
      case "google.protobuf.BoolValue" => "OptionType(BooleanType)" + fieldName
      case "google.protobuf.DoubleValue" => "OptionType(FloatType)" + fieldName
      case "google.protobuf.FloatValue" => "OptionType(FloatType)" + fieldName
      case _ => "OptionType(" + cleanProtoTypeNames(src).gqlName + ")" + fieldName
    }
  }

  def typeMappers(src: String, innerField: Option[String] = None, fieldName: String): String = {
    src match {
      case "google.protobuf.StringValue" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(StringType)"))
      case "repeated" =>{
        createGQlOutput(GqlOLineMini(fieldName,"ListType(" + innerField.getOrElse("").split("\\.").last+ ")"))
      }
      case "google.protobuf.Int64Value" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(LongType)"))
      case "google.protobuf.Int32Value" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(IntType)"))
      case "google.protobuf.BoolValue" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(BooleanType)" ))
      case "google.protobuf.DoubleValue" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(FloatType)" ))
      case "google.protobuf.FloatValue" => createGQlOutput(GqlOLineMini(fieldName,"OptionType(FloatType)"))
      case _ =>{
        createGQlOutput(GqlOLineMini(fieldName,"OptionType(" + (cleanProtoTypeNames(src).gqlName.split("\\.").last) + ")" ))
      }
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

  def cleanProtoTypeNames2(src: String): ClassInfo = {
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
          ClassInfo(src, className.mkString(""), className.mkString("").split(".").last + "Type", normal)
        }
      }
    }
  }

//  def snake2camel(name: String) = {
//    assert(!(name endsWith "_"), "names ending in _ not supported by this algorithm")
//    "[A-Za-z\\d]+_?|_".r.replaceAllIn(name, { x =>
//      val x0 = x.group(0)
//      if (x0 == "_") x0
//      else x0.stripSuffix("_").toLowerCase.capitalize
//    })
//  }

  def snake2camel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, {m =>
    m.group(1).toUpperCase()
  })

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

  case class GqlOLineMini(fieldName: String, gqlType: String)

  case class GqlILine(fieldName: String, gqlType: String, description: String)


  def createGQlOutput(o: GqlOLineMini) = {
    //Field("_id", OptionType(StringType), description = Some("AvPost Unique Identifier"), resolve = _.value.Id),
    "Field(" + "\"" + o.fieldName + "\", " + o.gqlType + "," + "description = Some(\"\")," + "resolve = _.value." + snake2camel(o.fieldName)+"),"

  }

}
