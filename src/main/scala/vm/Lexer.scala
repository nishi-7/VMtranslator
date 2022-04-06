package vm

import MyError._
import Lexer.keywords


object Lexer {
  private val keywords = Map(
    "add" -> Add,
    "sub" -> Sub,
    "neg" -> Neg,
    "eq"  -> Eq,
    "gt"  -> Gt,
    "lt"  -> Lt,
    "and" -> And,
    "or"  -> Or,
    "not" -> Not,
    "argument"  -> Arg,
    "local"     -> Local,
    "static"    -> Static,
    "constant" -> Const,
    "this"      -> This,
    "that"      -> That,
    "pointer"   -> Pointer,
    "temp"      -> Temp,
    "push"      -> Push,
    "pop"       -> Pop,
    "label"     -> Label,
    "goto"      -> Goto,
    "if-goto"   -> If,
    "function"  -> Func,
    "call"      -> Call,
    "return"    -> Return,
  )
}

class Lexer(val code: String) {
  private var ch: Option[Char] = None
  private var pos = 0
  private var nextPos = 0
  private var line = 0
  private var left = -1
  this.readChar()

  def nextToken(): Token = {
    this.skipWithSpaces()
    ch match {
      case None => Token(Eof, Loc(line, left, 0))
      case Some('\n') => {
        val tok = Token(NewLine, Loc(line, left, 1))
        this.readChar()
        line += 1
        tok
      }
      case Some(c) => {
        if (this.isLetter()) {
          val ident = this.readIdentifier()
          keywords.get(ident) match {
            case Some(ty) =>
              Token(ty, Loc(line, left - ident.length, ident.length))
            case None =>
              Token(Symbol(ident), Loc(line, left - ident.length, ident.length))
          }
        } else if (this.isDigit()) {
          val num = this.readNumber()
          Token(Number(num.toInt), Loc(line, left - num.length, num.length))
        } else if (c == '/') {
          this.readChar()
          if (ch == Some('/')) {
            while (ch != Some('\n')) {
              this.readChar()
            }
            this.nextToken()
          } else {
            throwTokenError(Token(Illegal(c), Loc(line, left-1, 1)))
          }
        } else {
          throwTokenError(Token(Illegal(c), Loc(line, left, 1)))
        }
      }
    }
  }

  def skipWithSpaces() = {
    while (this.isWhiteSpaces()) {
      this.readChar()
    }
  }

  def isWhiteSpaces() = {
    ch match {
      case Some(c) => c == ' ' || c == '\t'
      case None    => false
    }
  }

  def readChar() = {
    if (nextPos >= code.length) {
      ch = None
    } else {
      ch = Some(code(nextPos))
    }
    pos = nextPos
    left += 1
    nextPos += 1
  }

  def isLetter() = {
    ch match {
      case Some(c) =>
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
          c == '-' || c == '_' || c == '.' || c == ':'
      case None => false
    }
  }

  def readIdentifier() = {
    val p = pos
    while (this.isLetter() || this.isDigit()) {
      this.readChar()
    }
    code.slice(p, pos)
  }

  def isDigit() = {
    ch match {
      case Some(c) => ('0' <= c && c <= '9')
      case None    => false
    }
  }

  def readNumber() = {
    val p = pos
    while (this.isDigit()) {
      this.readChar()
    }
    code.slice(p, pos)
  }
}
