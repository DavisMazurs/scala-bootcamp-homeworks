package basics

import basics.DataStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DataStructuresTest extends AnyFlatSpec {
  "sortConsideringEqualValues" should "return correctly mapped list" in {
    sortConsideringEqualValues(Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)) shouldEqual
      List((Set("e"),0), (Set("a", "d"),1), (Set("b", "f", "g"),2), (Set("c"),4))
  }
}
