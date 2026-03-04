import org.scalatest.funsuite.AnyFunSuite

class TopWordsTest extends AnyFunSuite {

  test("basic word count") {
    val text = "scala scala java spark spark spark"
    val stopWords = Set[String]()
    val result = TopWords.getTopWords(text, stopWords, 2)

    assert(result.head._1 == "spark")
    assert(result.head._2 == 3)
  }

  test("stop words should be removed") {
    val text = "scala is great scala is powerful"
    val stopWords = Set("is")
    val result = TopWords.getTopWords(text, stopWords, 5)

    assert(!result.exists(_._1 == "is"))
  }

  test("top N words") {
    val text = "a b b c c c"
    val stopWords = Set[String]()
    val result = TopWords.getTopWords(text, stopWords, 1)

    assert(result.head == ("c", 3))
  }

  test("punctuation should be removed and case should not matter") {
    val text = "Scala, scala!! SPARK spark."
    val stopWords = Set[String]()
    val result = TopWords.getTopWords(text, stopWords, 2).toMap

    assert(result("scala") == 2)
    assert(result("spark") == 2)
  }

  test("words of length 1 should be filtered out") {
    val text = "a i u scala scala"
    val stopWords = Set[String]()
    val result = TopWords.getTopWords(text, stopWords, 5)

    assert(!result.exists { case (w, _) => w.length == 1 })
  }
}
