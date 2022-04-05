package vm

import java.io.{PrintWriter, File}
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import MyError._


class CodeWrite(val output: File) {
  val of = new PrintWriter(new OutputStreamWriter (new FileOutputStream(output), "utf-8"))
  var filename = ""
  var funcname = ""
  var alabel = 0
  var rlabel = 0
  this.writeInit()

  def setFileName(new_filename: String) = { filename = new_filename }

  def writeCmd(cmd: VMCmd) = {
    cmd.ty match {
      case Cmd2(Push, seg, Number(idx)) => writePush(seg, idx, cmd.loc)
      case Cmd2(Pop, seg, Number(idx)) => writePop(seg, idx, cmd.loc)
      case Cmd1(Label, Symbol(label)) => writeLabel(label)
      case Cmd1(Goto, Symbol(label)) => writeGoto(label)
      case Cmd1(If, Symbol(label)) => writeIf(label)
      case Cmd2(Func, Symbol(func), Number(nLocals)) => writeFunction(func, nLocals)
      case Cmd2(Call, Symbol(func), Number(nArgs)) => writeCall(func, nArgs)
      case Cmd0(Return) => writeReturn()
      case Cmd0(op) => writeArithmetic(op, cmd.loc)
      case c => throwCodeGenError("VM command", c.toString, cmd.loc)
    }
  }

  def writeInit() = {
    write("@256")
    write("D=A")
    write("@SP")
    write("M=D")
    writeCall("Sys.init", 0)
  }

  def writeArithmetic(op: TokenType, loc: Loc) = {
    write("@SP")
    op match {
      case Neg => write("A=M-1"); write("M=-M")
      case Not => write("A=M-1"); write("M=!M")
      case _   => {
        write("AM=M-1")
        write("D=M")
        write("@SP")
        write("A=M-1")
        op match {
          case Add => write("M=D+M")
          case Sub => write("M=M-D")
          case And => write("M=D&M")
          case Or  => write("M=D|M")
          case _   => {
            val a = alabel.toString
            write("D=M-D")
            write("@THEN"+a)
            op match {
              case Eq => write("D;JEQ")
              case Gt => write("D;JGT")
              case Lt => write("D;JLT")
              case tt  => throwCodeGenError("arith op", tt.toString, loc)
            }
            write("@SP")
            write("A=M-1")
            write("M=0")
            write("@ENDIF"+a)
            write("0;JMP")
            writeL("THEN", a)
            write("@SP")
            write("A=M-1")
            write("M=-1")
            writeL("ENDIF", a)
            alabel = alabel + 1
          }
        }
      }
    }
  }

  def writePush(seg: TokenType, idx: Int, loc: Loc) = {
    val id = idx.toString
    seg match {
      case Const   => write("@"+id); write("D=A");
      case Pointer => write("@R"+(idx+3).toString); write("D=M")
      case Temp    => write("@R"+(idx+5).toString); write("D=M")
      case Static  => write("@"+filename+"."+id); write("D=M")
      case _ => {
        write("@"+id)
        write("D=A")
        seg match {
          case Local => write("@LCL")
          case Arg   => write("@ARG")
          case This  => write("@THIS")
          case That  => write("@THAT")
          case s     => throwCodeGenError("segment", s.toString, loc)
        }
        write("A=D+M")
        write("D=M")
      }
    }
    write("@SP")
    write("AM=M+1")
    write("A=A-1")
    write("M=D")
  }

  def writePop(seg: TokenType, idx: Int, loc: Loc) = {
    val id = idx.toString
    seg match {
      case Local | Arg | This | That => {
        val reg = seg match {
          case Local => "@LCL"
          case Arg   => "@ARG"
          case This  => "@THIS"
          case That  => "@THAT"
          case _     => ""
        }
        write("@"+id)
        write("D=A")
        write(reg)
        write("M=D+M")
        write("@SP")
        write("AM=M-1")
        write("D=M")
        write(reg)
        write("A=M")
        write("M=D")
        write("@"+id)
        write("D=A")
        write(reg)
        write("M=M-D")
      }
      case _ => {
        write("@SP")
        write("AM=M-1")
        write("D=M")
        seg match {
          case Pointer => write("@R"+(idx+3).toString)
          case Temp    => write("@R"+(idx+5).toString)
          case Static  => write("@"+filename+"."+id)
          case s       => throwCodeGenError("segment", s.toString, loc)
        }
        write("M=D")
      }
    }
  }

  def writeLabel(label: String) = { of.write("("+funcname+"$"+label+")\n") }

  def writeGoto(label: String) = { write("@"+funcname+"$"+label); write("0;JMP") }

  def writeIf(label: String) = {
    write("@SP")
    write("AM=M-1")
    write("D=M")
    write("@"+funcname+"$"+label)
    write("D;JNE")
  }

  def writeCall(func: String, nArgs: Int) = {
    val r = rlabel.toString
    write("@RETURN"+r)
    write("D=A")
    write("@SP")
    write("AM=M+1")
    write("A=A-1")
    write("M=D")
    callerSave()
    write("@SP")
    write("D=M")
    write("@LCL")
    write("M=D")
    write("@"+(nArgs+5).toString)
    write("D=D-A")
    write("@ARG")
    write("M=D")
    write("@"+func)
    write("0;JMP")
    writeL("RETURN", r)
    rlabel = rlabel + 1
  }

  def writeReturn() = {
    write("@5")
    write("D=A")
    write("@LCL")
    write("A=M-D")
    write("D=M")
    write("@R13")
    write("M=D")
    write("@SP")
    write("A=M-1")
    write("D=M")
    write("@ARG")
    write("A=M")
    write("M=D")
    write("D=A+1")
    write("@SP")
    write("M=D")
    val regs = List("@THAT", "@THIS", "@ARG", "@LCL")
    for (reg <- regs) {
      write("@LCL")
      write("AM=M-1")
      write("D=M")
      write(reg)
      write("M=D")
    }
    write("@R13")
    write("A=M")
    write("0;JMP")
  }

  def writeFunction(func: String, nLocals: Int) = {
    of.write("("+func+")\n")
    write("@SP")
    write("A=M")
    for (_ <- 0 until nLocals) {
      write("M=0")
      write("A=A+1")
    }
    write("D=A")
    write("@SP")
    write("M=D")
    funcname = func
  }

  def callerSave() = {
    val regs = List("@LCL", "@ARG", "@THIS", "@THAT")
    for (reg <- regs) {
      write(reg)
      write("D=M")
      write("@SP")
      write("AM=M+1")
      write("A=A-1")
      write("M=D")
    }
  }

  def close() = { of.write("(END)\n"); write("@END"); write("0;JMP"); of.close() }

  def write(s: String) = { of.write("\t"+s+"\n") }

  def writeL(label: String, num: String) = { of.write("("+label+num+")\n") }
}