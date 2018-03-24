import mill._
import mill.scalalib._

object fp_scala extends ScalaModule {
  def scalaVersion = "2.12.4"

  object test extends Tests {
    def ivyDeps = Agg(ivy"io.monix::minitest:2.1.1")
    def testFrameworks = Seq("minitest.runner.Framework")
  }
}