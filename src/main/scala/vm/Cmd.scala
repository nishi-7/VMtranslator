package vm


object Op {
  trait Op
  case object Add extends Op
  case object Sub extends Op
  case object Neg extends Op
  case object Eq  extends Op
  case object Gt  extends Op
  case object Lt  extends Op
  case object And extends Op
  case object Or  extends Op
  case object Not extends Op
}

object Segment {
  trait Segment
  case object Arg     extends Segment
  case object Local   extends Segment
  case object Static  extends Segment
  case object Const   extends Segment
  case object This    extends Segment
  case object That    extends Segment
  case object Pointer extends Segment
  case object Temp    extends Segment
}

object VMcommand {
  import Op._
  import Segment._

  trait VMcommand
  case class Arith(op: Op)                extends VMcommand
  case class Push(seg: Segment, idx: Int) extends VMcommand
  case class Pop(seg: Segment, idx: Int)  extends VMcommand
  case class Label(s: String)             extends VMcommand
  case class Goto(s: String)              extends VMcommand
  case class If(s: String)                extends VMcommand
  case class Func(s: String, n: Int)      extends VMcommand
  case object Return                      extends VMcommand
  case class Call(s: String, n: Int)      extends VMcommand
}
