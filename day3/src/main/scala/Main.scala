import scala.io.Source
import scala.util.matching.Regex

object Main {
  def readInput(file: String): String = {
    Source.fromFile(file).getLines.mkString
  }

  abstract class Command {
    def where: Int
  }

  case class Mul(i: Int, j: Int, where: Int) extends Command
  case class Do(where: Int) extends Command
  case class Dont(where: Int) extends Command

  def getMulPairs(inp: String): List[Mul] = {
    val reg: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
    reg.findAllMatchIn(inp)
      .map(m => Mul(m.group(1).toInt, m.group(2).toInt, m.start))
      .toList
  }

  def getDosDonts(inp: String): List[Command] = {
    val reg: Regex = "(do|don't)\\(\\)".r
    reg.findAllMatchIn(inp)
      .map(mt => mt.group(1) match {
        case "do" => Do(mt.start)
        case "don't" => Dont(mt.start)
      })
      .toList
  }

  def main(args: Array[String]): Unit = {
    val input = readInput("input.txt")
    
    val list = (getMulPairs(input) ++ getDosDonts(input)).sortBy(cmd => cmd.where)

    val parts = splitWhen(list, _.isInstanceOf[Do])
      .map(_.takeWhile(!_.isInstanceOf[Dont])) 

    val result = parts.flatten.map { 
      case Mul(i, j, _) => i * j
      case _ => 0
    }.sum

    println(result)
  }

  def splitWhen[A](xs: List[A], pred: A => Boolean): List[List[A]] =
    xs.foldRight(Nil: List[List[A]]) {
      case (elm, parts) if pred(elm) => 
        Nil :: parts
      case (elm, parts) => parts match {
        case head :: rest =>
          (elm :: head) :: rest
        case Nil =>
          List(List(elm))
      }
    }
}
