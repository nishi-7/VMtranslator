package vm

import java.io.File
import scala.io.Source
import scala.util.{Try, Success, Failure}
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

class Lexer(val path: File) {
  var code = ""
  private val file = path.toString
  private var ch: Option[Char] = None
  private var pos = 0
  private var nextPos = 0
  private var line = 0
  private var left = -1
  Try(Source.fromFile(path)("UTF-8")) match {
    case Success(reader) => {
      val src = reader.getLines().foldLeft("")((acc, e) => acc + "\n" + e)
      reader.close()
      code = if (src.endsWith("\n")) { src } else { src + "\n" }
      this.readChar()
    }
    case Failure(exception) => {
      Console.err.println(
        s"[Error] can not find ${exception.printStackTrace().toString()}"
      )
    }
  }

  def nextToken(): Token = {
    this.skipWithSpaces()
    ch match {
      case None => Token(Eof, Loc(file, line, left, 0))
      case Some('\n') => {
        val tok = Token(NewLine, Loc(file, line, left, 1))
        this.readChar()
        line += 1
        tok
      }
      case Some(c) => {
        if (this.isLetter()) {
          val ident = this.readIdentifier()
          keywords.get(ident) match {
            case Some(ty) =>
              Token(ty, Loc(file, line, left - ident.length, ident.length))
            case None =>
              Token(Symbol(ident), Loc(file, line, left - ident.length, ident.length))
          }
        } else if (this.isDigit()) {
          val num = this.readNumber()
          Token(Number(num.toInt), Loc(file, line, left - num.length, num.length))
        } else if (c == '/') {
          this.readChar()
          if (ch == Some('/')) {
            while (ch != Some('\n')) {
              this.readChar()
            }
            this.nextToken()
          } else {
            throwTokenError(Token(Illegal(c), Loc(file, line, left-1, 1)))
          }
        } else {
          throwTokenError(Token(Illegal(c), Loc(file, line, left, 1)))
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
