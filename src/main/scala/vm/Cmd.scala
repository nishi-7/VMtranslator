package vm


case class VMCmd(ty: CmdType, loc: Loc)

abstract class CmdType
case class Cmd0(cmd: TokenType) extends CmdType
case class Cmd1(cmd: TokenType, arg1: TokenType) extends CmdType
case class Cmd2(cmd: TokenType, arg1: TokenType, arg2: TokenType) extends CmdType
