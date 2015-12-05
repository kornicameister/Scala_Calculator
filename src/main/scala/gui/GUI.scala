package main.scala.gui

import main.scala.{Converter, RPN}

import scala.swing._

object GUI extends SimpleSwingApplication {

  val opList = List("+", "-", "/", "*", "^", "%", "C")

  val numPanel = new GridPanel(3, 3) {
    (1 to 9) foreach (x => contents += new CalcButton(x.toString))
    List("0", ".", "=") foreach (x => contents += new CalcButton(x))
  }
  val operatorPanel = new GridPanel(2, 2) {
    opList foreach (x => contents += new CalcButton(x))
  }
  val display = new TextField(25)

  def calculate(input: String): String = {
    val rpn = Converter.convert(input)
    val result = RPN.solveRPN(rpn)
    result.toString()
  }

  def top = new MainFrame {
    title = "Simple Calculator"
    contents = new FlowPanel {
      contents += display
      contents += numPanel
      contents += operatorPanel
    }
  }

  class CalcAction(input: String) extends swing.Action(input) {
    override def apply = input match {
      case "=" => display.text = calculate(display.text)
      case "C" => display.text = ""
      case "." => if (!display.text.contains(".")) display.text += input
      case _ => display.text += input
    }
  }

  class CalcButton(x: String) extends Button(x) {
    action = new CalcAction(text)
  }

}
