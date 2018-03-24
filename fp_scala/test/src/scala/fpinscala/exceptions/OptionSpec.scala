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

  case class Employee(name: String, department: String, manager: Option[String])
  val employees = Seq(
    Employee("Joe", "HR", Some("Bob")),
    Employee("Eve", "IT", None),
    Employee("Fred", "IT", Some("Eve")))

  def convert[A](op: scala.Option[A]): fpinscala.exceptions.Option[A] = op match {
    case scala.None => fpinscala.exceptions.None
    case scala.Some(v) => fpinscala.exceptions.Some(v)
  }
  def lookupByName(name: String): Option[Employee] = convert(employees.find(_.name == name))

  test("map on Option[Employee] with Some") {
    val act = lookupByName("Joe").map(_.department)
    val exp = Some("HR")

    assert(act == exp)
  }

  test("map on Option[Employee] with None") {
    val act = lookupByName("Joelle").map(_.department)
    val exp = None

    assert(act == exp)
  }

  test("flatMap on Option[Employee] with failing first function") {
    val act = lookupByName("Joelle").flatMap(_.manager)
    val exp = None

    assert(act == exp)
  }

  test("flatMap on Option[Employee] with failing second function") {
    val act = lookupByName("Eve").flatMap(_.manager)
    val exp = None

    assert(act == exp)
  }

  test("getOrElse") {
    val act = lookupByName("Eve").map(_.department).getOrElse("Default Dept")
    val exp = "IT"

    assert(act == exp)
  }

  test("getOrElse with default") {
    val act = lookupByName("Evelyn").map(_.manager).getOrElse("Default manager")
    val exp = "Default manager"

    assert(act == exp)
  }

  test("flatMap2 on Option[Employee] with failing first function") {
    val act = lookupByName("Joelle").flatMap2(_.manager)
    val exp = None

    assert(act == exp)
  }

  test("flatMap2 on Option[Employee] with failing second function") {
    val act = lookupByName("Eve").flatMap2(_.manager)
    val exp = None

    assert(act == exp)
  }
}