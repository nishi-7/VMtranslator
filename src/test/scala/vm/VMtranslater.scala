package vm

import org.scalatest._
import java.io.File
import scala.io.Source


class VMtranslaterSpec extends FlatSpec with Matchers {
  val dir = "./test_files/"
  val samples = List[String](
    "BasicLoop",
    "FibonacciElement",
    "FibonacciSeries",
    "NestedCall",
    "SimpleFunction",
    "StaticsTest"
  )

  "VMtranslater" should "translate as C++ VMtranslater" in {

    for (sample <- samples) {
      VMtranlator.translate(dir+sample)
      val cpp_file = Source.fromFile(dir+sample+"/"+sample+".asm_cpp")
      val cpp_asm = cpp_file.getLines().foldLeft("")((acc, e) => acc + "\n" + e)
      cpp_file.close()

      val scala_file = Source.fromFile(dir+sample+".asm")
      val scala_asm = scala_file.getLines().foldLeft("")((acc, e) => acc + "\n" + e)
      scala_file.close()

      scala_asm shouldBe(cpp_asm)
    }
  }
}
