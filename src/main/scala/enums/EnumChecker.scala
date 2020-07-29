package enums

import enums.InvalidEnumType.InvalidEnumType

object EnumChecker extends App {

  val unsafe = "9:00 am"
  val safe = makeSafe(unsafe, InvalidEnumType.REMINDER_TIME)
  val madeUnsafe = makeUnsafe(safe,InvalidEnumType.REMINDER_TIME)

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
      madeUnsafeEnum.replaceAll(enumType.toString+"_", "")
    } else {
      madeUnsafeEnum
    }
  }

  println(unsafe)
  println(safe)
  println(madeUnsafe)

  println(unsafe == madeUnsafe)

}
