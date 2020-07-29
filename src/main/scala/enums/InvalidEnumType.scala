package enums

object InvalidEnumType extends Enumeration {
  type InvalidEnumType = Value
  val TIMEZONE,
  REMINDER_TIME,
  CALENDAR_INTERVAL,
  TIME_FORMAT,
  SHARING_RULE_ACCESS,
  CONVERSATION_COLOR_TYPE,
  DEFAULT_ORGANIZATION_ACCESS = Value
}
