package lab4

import scala.annotation.tailrec


def spellCheckPipeline(doc: String, dict: List[String]): Int = {
  val wordList = doc.split("\\W+")
  wordList.map(_.toLowerCase()).filter(!dict.contains(_)).map(x=>1).reduce(_ + _)
}
def spellCheckTailRecurs(doc: String, dict: List[String]): Int = {
  val wordList = doc.split("\\W+").toList
  @tailrec
  def helper(result:Int,remaining:List[String]):Int = {
    if (remaining.isEmpty)
      result
    else
      if (!dict.contains(remaining.head.toLowerCase()))
        helper(result+1,remaining.tail)
      else
        helper(result,remaining.tail)

  }
  helper(0,wordList)
}


object spellCheckTest extends App{
  val oed = List("dog", "cat", "bat", "bug", "fox", "see", "run", "bite", "the", "a", "and")
  val essay = "See the blue dog run . See the blue dog bite the man ."
  println(spellCheckPipeline(essay, oed)) // = 3
  println(spellCheckTailRecurs(essay, oed)) // = 3
}