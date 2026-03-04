import scala.io.Source

object TopWords {

  // added the method here for the test cases
  def getTopWords(text: String, stopWords: Set[String], n: Int): Seq[(String, Int)] = {
    val words = text.toLowerCase.replaceAll("[^a-z]", " ").split("\\s+").filter(word => word.nonEmpty && word.length > 1)

    val filteredWords = words.filter(word => !stopWords.contains(word))

    val wordCounts = filteredWords.groupBy(identity).map { case (word, list) => (word, list.length) }

    wordCounts.toSeq.sortBy(-_._2).take(n)
  }


  def main(args: Array[String]): Unit = {
    val url = "https://www.gutenberg.org/files/2701/2701-0.txt"

    val text = Source.fromURL(url).mkString

    val stopWords = Set(
      "the","of","to","and","a","in","is","it","you","that","he","was","for","on",
      "are","with","as","i","his","they","be","at","one","have","this","from","or",
      "had","by","not","word","but","what","some","we","can","out","other","were",
      "all","there","when","up","use","your","how","said","an","each","she"
    )

    val words = text.toLowerCase
      .replaceAll("[^a-z]", " ")
      .split("\\s+")
      .filter(word => word.nonEmpty && word.length > 1)

    val filteredWords = words.filter(word => !stopWords.contains(word))

    val wordCounts = filteredWords.groupBy(identity).map { case (word, list) => (word, list.length) }

    val top50 = wordCounts.toSeq.sortBy(-_._2).take(50)

    top50.foreach { case (word, count) =>
      println(s"$word -> $count")
    }
  }
}
