import GetProto.Field

import scala.io.Source

object ConvertEnums extends App {

  def convert() = {
    val lines: Seq[(String, Int)] = (Source.fromFile("target.in").getLines().toSeq.map(x => x.split(",")).toSeq)
      .flatten
      .zipWithIndex

    for (x <- lines) {
      println(x._1.trim + " = "+ (x._2.toInt +1) + ";")
    }
  }

  convert()

}
