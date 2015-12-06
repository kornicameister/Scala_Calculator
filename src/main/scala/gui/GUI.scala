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
  val operationsPanel = new GridPanel(1, 2) {
    contents += numberPanel
    contents += operatorPanel
    hGap = 10
  }
  var hadError: Boolean = false

  def calculate(): String = {
    if (hadError){
      displayPanel.text = ""
      hadError = false
      return ""
    }
    val input = displayPanel.text
    try {
      val rpn = Converter.convert(input)
      val result = RPN.solveRPN(rpn)
      result.toString
    } catch {
      case e: Exception =>
        hadError = true
        e.getMessage
    }
  }

  def onDot(input: String): String = {
    if (hadError){
      displayPanel.text = ""
      hadError = false
    }
    val text: String = displayPanel.text
    if (!text.isEmpty) {
      val lastChar: Char = text.last
      if (lastChar == '.') {
        return text
      }
    }
    text + input
  }

  def onC(): String = {
    hadError = false
    ""
  }

  def onCharacter(input: String): String = {
    if (hadError){
      displayPanel.text = ""
      hadError = false
    }
    displayPanel.text + input
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
      case "=" => displayPanel.text = calculate()
      case "C" => displayPanel.text = onC()
      case "." => displayPanel.text = onDot(input)
      case _ => displayPanel.text = onCharacter(input)
    }
  }

  class CalcButton(x: String) extends Button(x) {
    action = new CalcAction(text)
  }

}
