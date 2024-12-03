import scala.io.Source
import scala.util.matching.Regex

object Main {
  def readInput(file: String): String = {
    Source.fromFile(file).getLines.mkString
  }

  def getMulPairs(inp: String): List[(Int, Int)] = {
    val reg: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
    reg.findAllMatchIn(inp)
      .map(m => (m.group(1).toInt, m.group(2).toInt))
      .toList
  }

  def main(args: Array[String]): Unit = {
    val input = readInput("input.txt")
    val result = getMulPairs(input).map {
      case (i, j) => i * j
    }.sum

    println(result)
  }
}
