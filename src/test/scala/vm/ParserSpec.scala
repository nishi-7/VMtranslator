package vm

import org.scalatest._
import java.io.File
import scala.io.Source


class ParserSpec extends FlatSpec with Matchers {
  val sample1 = new File("./test_files/sample01.vm")
  val reader = Source.fromFile(sample1)("UTF-8")
  val src = reader.getLines().foldLeft("")((acc, e) => acc + "\n" + e)
  reader.close()
  val code = if (src.endsWith("\n")) { src } else { src + "\n" }
  "Parser" should "parse an input file" in {
    val lexer = new Lexer(code)
    val parser = new Parser(lexer)

    parser.command().ty shouldBe(Cmd2(Push, Const, Number(0)))
    parser.command().ty shouldBe(Cmd2(Pop, Local, Number(0)))
    parser.command().ty shouldBe(Cmd1(Label, Symbol("LOOP_START")))
    parser.command().ty shouldBe(Cmd2(Push, Arg, Number(0)))
    parser.command().ty shouldBe(Cmd2(Push, Local, Number(0)))
    parser.command().ty shouldBe(Cmd0(Add))
    parser.command().ty shouldBe(Cmd2(Pop, Local, Number(0)))
    parser.command().ty shouldBe(Cmd2(Push, Arg, Number(0)))
    parser.command().ty shouldBe(Cmd2(Push, Const, Number(1)))
    parser.command().ty shouldBe(Cmd0(Sub))
    parser.command().ty shouldBe(Cmd2(Pop, Arg, Number(0)))
    parser.command().ty shouldBe(Cmd2(Push, Arg, Number(0)))
    parser.command().ty shouldBe(Cmd1(If, Symbol("LOOP_START")))
    parser.command().ty shouldBe(Cmd2(Push, Local, Number(0)))
    parser.hasMoreCommands() shouldBe(false)
  }
}
