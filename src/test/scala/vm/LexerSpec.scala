package vm

import org.scalatest._
import java.io.File


class LexerSpec extends FlatSpec with Matchers {
  val sample1 = new File("./test_files/sample01.vm")
  "Lexer" should "tokenize an input file" in {
    val lexer = new Lexer(sample1)

    lexer.nextToken().ty shouldBe(NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Push)
    lexer.nextToken().ty shouldBe (Const)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Pop)
    lexer.nextToken().ty shouldBe (Local)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Label)
    lexer.nextToken().ty shouldBe (Symbol("LOOP_START"))
    lexer.nextToken().ty shouldBe(NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Push)
    lexer.nextToken().ty shouldBe (Arg)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Push)
    lexer.nextToken().ty shouldBe (Local)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Add)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Pop)
    lexer.nextToken().ty shouldBe (Local)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe(Push)
    lexer.nextToken().ty shouldBe (Arg)
    lexer.nextToken().ty shouldBe(Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Push)
    lexer.nextToken().ty shouldBe (Const)
    lexer.nextToken().ty shouldBe (Number(1))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Sub)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Pop)
    lexer.nextToken().ty shouldBe (Arg)
    lexer.nextToken().ty shouldBe (Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Push)
    lexer.nextToken().ty shouldBe (Arg)
    lexer.nextToken().ty shouldBe (Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (If)
    lexer.nextToken().ty shouldBe (Symbol("LOOP_START"))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Push)
    lexer.nextToken().ty shouldBe (Local)
    lexer.nextToken().ty shouldBe (Number(0))
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (NewLine)
    lexer.nextToken().ty shouldBe (Eof)
  }
}
