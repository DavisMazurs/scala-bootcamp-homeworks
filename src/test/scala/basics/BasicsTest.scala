package basics

import basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BasicsTest extends AnyFlatSpec {
  "gcd" should "return 6 for numbers 18 and 12" in {
    gcd(18, 12) shouldEqual 6
  }

  it should "return 15 for numbers 45 and 60" in {
    gcd(45, 60) shouldEqual 15
  }

  "lcm" should "return 60 for numbers 20 and 30" in {
    lcm(20, 30) shouldEqual 60
  }

  it should "return 15 for numbers 15 and 5" in {
    lcm(15, 5) shouldEqual 15
  }
}
