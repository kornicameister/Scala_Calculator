package main.scala

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical._

abstract class Expr {
  def rpn: String
}

case class BinaryOperator(lhs: Expr, op: String, rhs: Expr) extends Expr {
  def rpn: String = lhs.rpn + " " + rhs.rpn + " " + op
}

case class Number(v: String) extends Expr {
  def rpn: String = v
}

case class Variable(v: String) extends Expr {
  def rpn: String = v
}

class ExprLexical extends StdLexical {
  override def token: Parser[Token] = floatingToken | super.token

  def floatingToken: Parser[Token] =
    rep1(digit) ~ optFraction ~ optExponent ^^
      { case intPart ~ frac ~ exp => NumericLit(
        (intPart mkString "") :: frac :: exp :: Nil mkString "")}

  def chr(c:Char) = elem("", ch => ch==c )
  def sign = chr('+') | chr('-')
  def optSign = opt(sign) ^^ {
    case None => ""
    case Some(sign) => sign
  }
  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }
  def optFraction = opt(fraction) ^^ {
    case None => ""
    case Some(fraction) => fraction
  }
  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }
  def optExponent = opt(exponent) ^^ {
    case None => ""
    case Some(exponent) => exponent
  }
}

object Converter extends StandardTokenParsers {
  override val lexical = new ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "^", "(", ")")

  def value: Parser[Expr] = numericLit ^^ { s => Number(s) }

  def variable: Parser[Expr] = ident ^^ { s => Variable(s) }

  def parens: Parser[Expr] = "(" ~> expr <~ ")"

  def term = value | parens | variable

  // Needed to define recursive because ^ is right-associative
  def pow: Parser[Expr] = term ~ "^" ~ pow ^^ { case left ~ _ ~ right => BinaryOperator(left, "^", right) } | term

  def factor = pow * ("*" ^^^ { (left: Expr, right: Expr) => BinaryOperator(left, "*", right) } |
    "/" ^^^ { (left: Expr, right: Expr) => BinaryOperator(left, "/", right) })

  def sum = factor * ("+" ^^^ { (left: Expr, right: Expr) => BinaryOperator(left, "+", right) } |
    "-" ^^^ { (left: Expr, right: Expr) => BinaryOperator(left, "-", right) })

  def expr = sum | term

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  def convert(input: String): String = input match {
    case null => ""
    case "" => ""
    case _ =>
      parse(input) match {
        case Success(tree, _) =>
          tree.rpn
        case e: NoSuccess => Console.err.println(e)
          e.toString
      }
  }
}