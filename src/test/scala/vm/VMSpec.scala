package vm

import org.scalatest._
import java.io.File
import Op._
import Segment._
import VMcommand._


class ParserSpec extends FlatSpec {
  val sample1 = new File("./test_files/sample01.vm")
  "Paser" should "trim comments, spaces and blank line" in {
    val parser = new Parser(sample1)
    val expect = List[String](
        "push constant 0",
        "pop local 0",
        "label LOOP_START",
        "push argument      0",
        "push local 0",
        "add",
        "pop local 0",
        "push argument 0",
        "push constant 1",
        "sub",
        "pop argument 0",
        "push argument 0",
        "if-goto LOOP_START",
        "push local 0")
    assert(parser.lines == expect)
  }

  it should "translate line to command" in {
    val parser = new Parser(sample1)
    val expect = List[VMcommand](
        Push(Const, 0),
        Pop(Local, 0),
        Label("LOOP_START"),
        Push(Arg, 0),
        Push(Local, 0),
        Arith(Add),
        Pop(Local, 0),
        Push(Arg, 0),
        Push(Const, 1),
        Arith(Sub),
        Pop(Arg, 0),
        Push(Arg, 0),
        If("LOOP_START"),
        Push(Local, 0))
    var cmds = List[VMcommand]()
    while (parser.hasMoreCommands()) {
      parser.advance()
      cmds :+= parser.command()
    }
    assert(cmds == expect)
  }
}
