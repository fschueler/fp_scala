package fpinscala.datastructures

import minitest._
import fpinscala.datastructures.List._

object ListSpec extends SimpleTestSuite {
  test("tail") {
    val act = tail(List(1, 2, 3))
    val exp = List(2, 3)
    assertEquals(act, exp)
  }

  test("init") {
    val act = init(List(1, 2, 3, 4))
    val exp = List(1, 2, 3)
    assertEquals(act, exp)
  }

  test("drop") {
    val act = drop(List(1, 2, 3), 2)
    val exp = List(3)
    assertEquals(act, exp)
  }

  test("dropWhile") {
    val act = dropWhile(List(2, 4, 7))(x => x % 2 == 0)
    val exp = List(7)
    assertEquals(act, exp)
  }

  test("append") {
    val act = append(List(1, 2), List(3, 4))
    val exp = List(1, 2, 3, 4)
    assertEquals(act, exp)
  }

  test("foldRight with identity") {
    val act = foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
    val exp = List(1, 2, 3)
    assertEquals(act, exp)
  }

  test("sum[Int] foldRight") {
    val act = sum(List(1, 2, 3))
    val exp = 6
    assertEquals(act, exp)
  }

  test("product[Double] foldRight") {
    val act = product(List(1.0, 2.0, 3.0))
    val exp = 6.0
    assertEquals(act, exp)
  }

  test("length foldRight") {
    val act = length(List(1, 2, 3))
    val exp = 3
    assertEquals(act, exp)
  }

  test("foldLeft") {
    val act = foldLeft(List(1, 2, 3), "X")((x, y) => x + y)
    val exp = "X123"
    assertEquals(act, exp)
  }

  test("sum[Int] foldLeft") {
    val act = sumFoldLeft(List(1, 2, 3))
    val exp = 6
    assertEquals(act, exp)
  }

  test("product[Double] foldLeft") {
    val act = productFoldLeft(List(1.0, 2.0, 3.0))
    val exp = 6.0
    assertEquals(act, exp)
  }

  test("length foldLeft") {
    val act = lengthFoldLeft(List(1, 2, 3, 4))
    val exp = 4
    assertEquals(act, exp)
  }

  test("reverse") {
    val act = reverse(List("a", "b", "c", "d"))
    val exp = List("d", "c", "b", "a")
    assertEquals(act, exp)
  }
}
