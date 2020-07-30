import GetProto.Field

import scala.io.Source

object ConvertEnums extends App {

  def convert() = {
    val lines: List[(String, Int)] = (Source.fromFile("target.in").getLines().toList.map(x => x.split(",")).toList)
      .flatten
      .zipWithIndex

    for (x <- lines) {
      println(x._1.trim + " = "+ (x._2.toInt +1) + ";")
    }
  }

  convert()

}
