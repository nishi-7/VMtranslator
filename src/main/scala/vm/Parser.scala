package vm

import java.io.File
import scala.io.Source
import MyException._
import Op._
import Segment._
import VMcommand._

class Parser(val path: File) {
  val s = Source.fromFile(path)("UTF-8")
  var cmd: Option[VMcommand] = None
  var lines = List[String]()
  var cmds = Iterator[String]()
  try {
    for (line <- s.getLines) {
      var l = line
      if (l.indexOf("/") != -1) {
        l = l.substring(0,l.indexOf("/"))
      }
      l = l.trim
      if (l.length != 0) {
        lines :+= l
      }
    }
  } finally {
    s.close
  }
  cmds = lines.iterator

  def hasMoreCommands() = { cmds.hasNext }

  def advance() = {
    if (hasMoreCommands()) {
      val line = cmds.next.split(" +")
      cmd = line(0) match {
        case "push"     => Some(Push(str2seg(line(1)), line(2).toInt))
        case "pop"      => Some(Pop(str2seg(line(1)), line(2).toInt))
        case "label"    => Some(Label(line(1)))
        case "goto"     => Some(Goto(line(1)))
        case "if-goto"  => Some(If(line(1)))
        case "function" => Some(Func(line(1), line(2).toInt))
        case "call"     => Some(Call(line(1), line(2).toInt))
        case "return"   => Some(Return)
        case _          => Some(str2arith(line(0)))
      }
    } else {
      throw(new MyException("No more commands."))
    }
  }

  def command() = { cmd.get }

  def str2seg(s: String): Segment = {
    s match {
      case "argument" => Arg
      case "local"    => Local
      case "static"   => Static
      case "constant" => Const
      case "this"     => This
      case "that"     => That
      case "pointer"  => Pointer
      case "temp"     => Temp
      case _          => throw(new MyException("SyntaxError"))
    }
  }

  def str2arith(s: String): VMcommand = {
    Arith(s match {
      case "add" => Add
      case "sub" => Sub
      case "neg" => Neg
      case "eq"  => Eq
      case "gt"  => Gt
      case "lt"  => Lt
      case "and" => And
      case "or"  => Or
      case "not" => Not
      case _     => throw(new MyException("SyntaxError"))
    })
  }
}
