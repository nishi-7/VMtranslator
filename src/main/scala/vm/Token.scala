package vm


// Token
sealed case class Token(var ty: TokenType, var loc: Loc)

// TokenType
abstract class TokenType
// illegal character
case class Illegal(ch: Char) extends TokenType
// Eof
case object Eof extends TokenType
// '\n'
case object NewLine extends TokenType


/* Operation */
// add
case object Add extends TokenType
// sub
case object Sub extends TokenType
// neg
case object Neg extends TokenType
// eq
case object Eq  extends TokenType
// gt
case object Gt  extends TokenType
// lt
case object Lt  extends TokenType
// and
case object And extends TokenType
// or
case object Or  extends TokenType
// not
case object Not extends TokenType


/* Segment */
// argument
case object Arg     extends TokenType
// local
case object Local   extends TokenType
// static
case object Static  extends TokenType
// constrant
case object Const   extends TokenType
// this
case object This    extends TokenType
// that
case object That    extends TokenType
// pointer
case object Pointer extends TokenType
// temp
case object Temp    extends TokenType


/* Command */
// push
case object Push    extends TokenType
// pop
case object Pop     extends TokenType
// label
case object Label   extends TokenType
// goto
case object Goto    extends TokenType
// if-goto
case object If      extends TokenType
// function
case object Func    extends TokenType
// call
case object Call    extends TokenType
// return
case object Return  extends TokenType


case class Symbol(s: String)  extends TokenType
case class Number(n: Int)     extends TokenType
