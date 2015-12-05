package main.scala

object Calculator extends App {

  override def main(args: Array[String]) {
    val input: String = args(0)
    println("Equation: " + input)

    val converted: String = Converter.convert(input)
    println("RPN: " + converted)

    val result: Double = RPN.solveRPN(converted)
    println("Result: " + result)
  }

}
