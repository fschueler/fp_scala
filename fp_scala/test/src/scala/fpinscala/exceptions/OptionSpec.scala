package fpinscala.exceptions

import minitest._

object OptionSpec extends SimpleTestSuite {

  test("map on Some(x)") {
    val os = Some(2.0)
    val f = (x: Double) => x * x

    val act = os.map(f)
    val exp = Some(4.0)

    assert(act == exp)
  }

  test("map on None") {
    val n: Option[Double] = None
    val f = (x: Double) => x * x

    val act = n.map(f)
    val exp = None
    
    assert(act == exp)
  }
}