package main.scala

object RPN {
  def solveRPN(eqn: String): Double = {
    val tmp = eqn.trim()
    val accumulator = List[Double]()
    tmp.split(" ").foldLeft(accumulator)(foldingFunction).head
  }

  def foldingFunction(stack: List[Double], a: String): List[Double] = stack match {
    case List() => a.toDouble :: stack
    case List(_) => a.toDouble :: stack
    case x :: y :: ys => a match {
      case "^" => math.pow(y, x).toFloat :: ys
      case "*" => x * y :: ys
      case "+" => x + y :: ys
      case "-" => y - x :: ys
      case "/" => y / x :: ys
      case s: String => s.toDouble :: stack
    }
  }
}
