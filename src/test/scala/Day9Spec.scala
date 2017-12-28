import org.scalatest._

class Day9Spec extends FlatSpec with Matchers {

  "An empty Set" should "have size 0" in {
    assert(Set.empty.size == 1)
  }

  it should "produce NoSuchElementException when head is invoked" in {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}