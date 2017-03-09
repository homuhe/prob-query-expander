package com.ir

/**
  * Created by root on 09.03.17.
  */
import scala.io._
import java.util.regex.Pattern

class PhraseExtractor {
  /**
    * extracts a word array of a conllu format file that contains only words (no punctuation) and is lowercased
    * @param file : a String that denotes the input file
    * @return the array of words contained in that file
    */
  def preprocessing(file: String): Array[String] = {
    val delimiter = "[ \t\n\r,.?!\\-:;()\\[\\]'\"/*#&$]+"

    val words = Source.fromFile(file).getLines()
                                          .filter(!_.isEmpty)
                                          .map(_.split("\t")(1))
                                          .map(_.toLowerCase())
    val spacePattern = Pattern.compile(delimiter)

    val processed_words = words.filter(!spacePattern.matcher(_).find()).toArray
    processed_words
  }
}
object PhraseExtractor {
  def main(args: Array[String]): Unit = {
    val file = "testcorpus/53293.conll"
    val f = new PhraseExtractor
    f.preprocessing(file)
  }
}