package vm

import java.nio.file.{Paths, Files}
import java.io.File


object VMtranlator {
  def translate(path: String) = {
    val (files, name) = getFilesAndBasename(path)
    val output = new File(name+".asm")
    val cw = new CodeWrite(output)
    for (f <- files) {
      val file = f.toString
      if (file.endsWith(".vm")) {
        val lexer = new Lexer(f)
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
      }
    }
    cw.close()
  }

  def getFilesAndBasename(path: String): (List[File], String) = {
    val p = Paths.get(path)
    if (Files.notExists(p)) {
      Console.err.println(s"[Error] can't find ${path}")
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
