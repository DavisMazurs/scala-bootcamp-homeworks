package basics

import java.text.DecimalFormat

import basics.ControlStructures.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source

object ControlStructures {

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(message: String) {
    def getError: String = "Error: " + message
  }

  sealed trait Result {
    def command: String
    def numbers: List[Double]
    def result: Double
  }
  final case class Results(command: String, numbers: List[Double], result: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    def parseToDoubleList(list: List[String]): List[Double] = {
      list.map(parseToDouble)
    }

    def parseToDouble(value: String): Double = {
      value.toDoubleOption.getOrElse(Double.NaN)
    }

    x.split(" ").toList match {
      case Nil => Left(ErrorMessage("Input is empty"))
      case x :: Nil => Left(ErrorMessage("There are no numbers"))
      case x :: x1 :: x2 :: Nil if x == "divide" => Right(Divide(parseToDouble(x1), parseToDouble(x2)))
      case x :: xs if x == "sum" => Right(Sum(parseToDoubleList(xs)))
      case x :: xs if x == "average" => Right(Average(parseToDoubleList(xs)))
      case x :: xs if x == "min" => Right(Min(parseToDoubleList(xs)))
      case x :: xs if x == "max" => Right(Max(parseToDoubleList(xs)))
      case _ => Left(ErrorMessage("Invalid command"))
    }

  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    val invalidNumber: Left[ErrorMessage, Nothing] = Left(ErrorMessage("One of the numbers is invalid"))

    x match {
      case Divide(x1, x2) if x2 == 0 => Left(ErrorMessage("Division by zero"))
      case Divide(x1, x2) =>
        if (x1 == Double.NaN || x2 == Double.NaN) invalidNumber
        else Right(Results("divide", x1 :: x2 :: Nil, x1 / x2))
      case Sum(xs) => if (xs.contains(Double.NaN)) invalidNumber else Right(Results("sum", xs, xs.sum))
      case Average(xs) => if (xs.contains(Double.NaN)) invalidNumber else Right(Results("average", xs, xs.sum / xs.length))
      case Min(xs) => if (xs.contains(Double.NaN)) invalidNumber else Right(Results("min", xs, xs.min))
      case Max(xs) => if (xs.contains(Double.NaN)) invalidNumber else Right(Results("max", xs, xs.max))
    }

  }

  def renderResult(x: Result): String = {
    def format(): String = f"the ${x.command} of ${x.numbers.mkString(" ")} is ${x.result}"

    x.command match {
      case "divide" => s"${x.numbers(0)} divided by ${x.numbers(1)} is ${x.result}"
      case "sum" | "average" | "min" | "max" => format()
    }
  }

  def process(x: String): String = {
    (for {
      cmd <- parseCommand(x)
      result <- calculate(cmd)
    } yield result) match {
      case Right(x) => renderResult(x)
      case Left(x) => x.getError
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
