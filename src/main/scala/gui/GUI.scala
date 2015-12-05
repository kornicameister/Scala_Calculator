package main.scala.gui

import main.scala.{Converter, RPN}

import scala.swing._

object GUI extends SimpleSwingApplication {

  val operatorList = List(
    "+",
    "-",
    "/",
    "*",
    "^",
    "%",
    "C"
  )

  val numberPanel = new GridPanel(3, 3) {
    (1 to 9) foreach (x => contents += new CalcButton(x.toString))
    List("0", ".", "=") foreach (x => contents += new CalcButton(x))
  }
  val operatorPanel = new GridPanel(2, 2) {
    operatorList foreach (x => contents += new CalcButton(x))
  }
  val displayPanel = new TextField(25) {
    horizontalAlignment = Alignment.Right
  }
  val operationsPanel = new GridPanel(1, 2){
    contents += numberPanel
    contents += operatorPanel
    hGap = 10
  }

  def calculate(input: String): String = {
    val rpn = Converter.convert(input)
    val result = RPN.solveRPN(rpn)
    result.toString
  }

  def top = new MainFrame {
    title = "Scala calculator"
    contents = new GridPanel(2, 1) {
      contents += displayPanel
      contents += operationsPanel
    }
  }

  class CalcAction(input: String) extends swing.Action(input) {
    override def apply = input match {
      case "=" => displayPanel.text = calculate(displayPanel.text)
      case "C" => displayPanel.text = ""
      case "." => if (!displayPanel.text.contains(".")) displayPanel.text += input
      case _ => displayPanel.text += input
    }
  }

  class CalcButton(x: String) extends Button(x) {
    action = new CalcAction(text)
  }

}
