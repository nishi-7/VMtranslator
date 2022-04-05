package vm

import MyError._


class Parser(lex: Lexer) {
  private var curToken = lex.nextToken()
  while (curToken.ty == NewLine) {
    curToken = lex.nextToken()
  }

  def hasMoreCommands() = { curToken.ty != Eof }

  def advance() = {
    val tok = curToken
    curToken = lex.nextToken()
    tok
  }

  def command(): VMCmd = {
    val tok = this.advance()
    tok.ty match {
      case Push | Pop => parsePushPop(tok)
      case Label | Goto | If => parseLabel(tok)
      case Func | Call => parseFuncCall(tok)
      case Return | Add | Sub | Neg | Eq | Gt | Lt | And | Or | Not => {
        expectedNewLine()
        VMCmd(Cmd0(tok.ty), tok.loc)
      }
      case tt => throwError("VM command", tok)
    }
  }

  def parsePushPop(cmd: Token) = {
    val seg = parseSegment()
    val num = expected(Number(0))
    expectedNewLine()
    VMCmd(Cmd2(cmd.ty, seg.ty, num.ty), seg.loc)
  }

  def parseLabel(cmd: Token) = {
    val sym = expected(Symbol("_"))
    expectedNewLine()
    VMCmd(Cmd1(cmd.ty, sym.ty), cmd.loc)
  }

  def parseFuncCall(cmd: Token) = {
    val sym = expected(Symbol("_"))
    val num = expected(Number(0))
    expectedNewLine()
    VMCmd(Cmd2(cmd.ty, sym.ty, num.ty), cmd.loc)
  }

  def parseSegment() = {
    val seg = this.advance()
    seg.ty match {
      case Arg | Local | Static | Const | This | That | Pointer | Temp => seg
      case s => throwError("segment", seg)
    }
  }

  def check(ty: TokenType) = {
    (curToken.ty, ty) match {
      case (Number(_), Number(_)) | (Symbol(_), Symbol(_)) => true
      case _ => curToken.ty == ty
    }
  }

  def expected(ty: TokenType) = {
    if (!this.check(ty)) {
      throwSyntaxError(ty, curToken)
    }
    this.advance()
  }

  def expectedNewLine() = {
    this.expected(NewLine)
    while (curToken.ty == NewLine) {
      this.advance()
    }
  }
}
