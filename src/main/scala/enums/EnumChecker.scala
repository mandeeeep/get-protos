package enums

import enums.InvalidEnumType.InvalidEnumType

object EnumChecker extends App {


  def makeSafe(unsafeEnum: String, enumType: InvalidEnumType): String = {

    val madeSafeEnum = unsafeEnum.replaceAll("\\s", "_SPACE_") //Always start with replacing empty space
      .replaceAll("/", "_SLASH_")
      .replaceAll("-", "_MINUS_")
      .replaceAll("\\+", "_PLUS_")
      .replaceAll("−", "_SDASH_")
      .replaceAll(":", "_COLON_")
      .replaceAll("%", "_PERC_")
      .replaceAll("\\(", "_BRCO_")
      .replaceAll("\\)", "_BRCC_")
    if (enumType == InvalidEnumType.REMINDER_TIME ||
      enumType == InvalidEnumType.CALENDAR_INTERVAL ||
      enumType == InvalidEnumType.TIME_FORMAT) {
      enumType.toString + "_" + madeSafeEnum
    } else {
      madeSafeEnum
    }

  }

  def makeUnsafe(unsafeEnum: String, enumType: InvalidEnumType): String = {
    val madeUnsafeEnum = unsafeEnum.replaceAll("_SLASH_", "/")
      .replaceAll("_MINUS_", "-")
      .replaceAll("_PLUS_", "\\+")
      .replaceAll("_SDASH_", "−")
      .replaceAll("_COLON_", ":")
      .replaceAll("_PERC_", "%")
      .replaceAll("_BRCO_", "\\(")
      .replaceAll("_BRCC_", "\\)")
      .replaceAll("_SPACE_", " ") //Always replace empty space in the end

    if (enumType == InvalidEnumType.REMINDER_TIME ||
      enumType == InvalidEnumType.CALENDAR_INTERVAL ||
      enumType == InvalidEnumType.TIME_FORMAT) {
      madeUnsafeEnum.replaceAll(enumType.toString + "_", "")
    } else {
      madeUnsafeEnum
    }
  }

//  CALENDAR_INTERVAL.map(_._1).map(x=> {
//    println(x+",")
//  })

//  TimeZoneT.values.toList.map(y => {
//    val x = y.toString
//    //println( "\""+makeSafe(x, InvalidEnumType.SHARING_RULE_ACCESS)+"\""+"->"+"\""+x+"\""+",")
//    println(makeSafe(x,InvalidEnumType.TIMEZONE))
//    //println("\"" + x + "\"" + "->" + "\"" + makeSafe(x, InvalidEnumType.TIMEZONE) + "\"" + ",")
//  })


}


//  ConversationColorType.map(_._2).map(x => {
//    println( "\""+makeSafe(x, InvalidEnumType.SHARING_RULE_ACCESS)+"\""+"->"+"\""+x+"\""+",")
//    //println("\"" + x + "\"" + "->" + "\"" + makeSafe(x, InvalidEnumType.REMINDER_TIME) + "\"" + ",")
//  })
