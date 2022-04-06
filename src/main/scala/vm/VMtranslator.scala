package vm

import java.nio.file.{Paths, Files}
import java.io.File
import scala.io.Source
import scala.util.{Try, Success, Failure}
import error.SyntaxError


object VMtranlator {
  def translate(path: String) = {
    val (files, name) = getFilesAndBasename(path)
    if (name == "") {
      Console.err.println(s"[Error] can't find ${path}")
    } else {
      val output = new File(name+".asm")
      val cw = new CodeWrite(output)
      for (f <- files) {
        val file = f.toString
        if (file.endsWith(".vm")) {
          Try(Source.fromFile(file)("UTF-8")) match {
            case Success(reader) => {
              val src = reader.getLines().foldLeft("")((acc, e) => acc + "\n" + e)
              reader.close()
              val code = if (src.endsWith("\n")) { src } else { src + "\n" }

              Try({
                val lexer = new Lexer(code)
                val parser = new Parser(lexer);
                var fname = file.substring(0,file.lastIndexOf("."))
                if (fname.lastIndexOf("/") != -1) {
                  fname = fname.substring(fname.lastIndexOf("/")+1)
                }
                cw.setFileName(fname)
                while (parser.hasMoreCommands()) {
                  val cmd = parser.command()
                  cw.writeCmd(cmd)
                }
              }) match {
                case Success(_) =>
                case Failure(e: SyntaxError) =>
                  Console.err.println(e.errorReport(code, file))
                case Failure(e) =>
                  Console.err.println(
                    s"[Error] failed to translate ${e.printStackTrace().toString()}"
                  )
              }
            }
            case Failure(exception) => {
              Console.err.println(
                s"[Error] can not find ${exception.printStackTrace().toString()}"
              )
            }
          }
        }
      }
      cw.close()
    }
  }

  def getFilesAndBasename(path: String): (List[File], String) = {
    val p = Paths.get(path)
    if (Files.notExists(p)) {
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
      Console.err.println("[Error] no args")
    }
  }
}
