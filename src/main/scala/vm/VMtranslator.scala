package vm

import java.nio.file.{Paths, Files}
import java.io.File
import VMcommand._


object VMtranlator {
  def translate(path: String) = {
    val (files, name) = getFilesAndBasename(path)
    val output = new File(name+".asm")
    val cw = new CodeWrite(output)
    cw.writeInit()
    for (f <- files) {
      val file = f.toString
      val ext = file.substring(file.lastIndexOf("."))
      if (ext == ".vm") {
        val parser = new Parser(f);
        var fname = file.substring(0,file.lastIndexOf("."))
        if (fname.lastIndexOf("/") != -1) {
          fname = fname.substring(fname.lastIndexOf("/")+1)
        }
        cw.setFileName(fname)
        while (parser.hasMoreCommands()) {
          parser.advance()
          val cmd = parser.command()
          cmd match {
            case Arith(op)     => cw.writeArithmetic(op)
            case Push(seg,idx) => cw.writePush(seg,idx)
            case Pop(seg,idx)  => cw.writePop(seg,idx)
            case Label(s)      => cw.writeLabel(s)
            case Goto(s)       => cw.writeGoto(s)
            case If(s)         => cw.writeIf(s)
            case Func(s,n)     => cw.writeFunction(s,n)
            case Return        => cw.writeReturn()
            case Call(s,n)     => cw.writeCall(s,n)
            case _             =>
          }
        }
      }
    }
    cw.close()
  }

  def getFilesAndBasename(path: String): (List[File], String) = {
    val p = Paths.get(path)
    if (Files.notExists(p)) {
      println("no exist file or directory")
      (List[File](), "")
    } else {
      val f = new File(path)
      var dir = f.getParent()
      if (dir == null) dir = "."
      val basename = f.getName()
      if (Files.isDirectory(p)) {
        val files = getListOfFiles(f)
        (files, dir+"/"+basename)
      } else {
        val name = basename.substring(0,basename.lastIndexOf('.'))
        (List(f), dir+"/"+name)
      }
    }
  }

  def getListOfFiles(d: File): List[File] = {
    var files = d.listFiles.filter(_.isFile).toList
    d.listFiles.filter(_.isDirectory).foreach{ dir =>
      files = files ::: getListOfFiles(dir.getAbsoluteFile)
    }
    files
  }

  def main(args: Array[String]) = {
    if (args.length>0) {
      VMtranlator.translate(args(0))
    } else {
      println("no args")
    }
  }
}
