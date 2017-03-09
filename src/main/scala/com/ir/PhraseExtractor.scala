package com.ir

/**
  * Created by root on 09.03.17.
  */
import scala.io._
import java.util.regex.Pattern

class PhraseExtractor {
  /**
    * extracts a word array of a conllu format file that contains only words (no punctuation) and is lowercased
    * @param inputfile : a String that denotes the input file
    * @return the array of words contained in that file
    */
  def preprocessFile(inputfile: String): Array[String] = {
    val delimiter = "[ \t\n\r,.?!\\-:;()\\[\\]'\"/*#&$]+"
    val words = Source.fromFile(inputfile).getLines().filter(!_.isEmpty).map(_.split("\t")(1)).map(_.toLowerCase())
    val spacePattern = Pattern.compile(delimiter)
    val newwords = words.filter(el => spacePattern.matcher(el).find() == false)
    newwords
  }
}
object PhraseExtractor {
  def main(args: Array[String]): Unit = {
    val file = "/home/neele/Dokumente/InformationRetrieval/query-hyb/testcorpus/53293.conll"
    val f = new PhraseExtractor
    f.preprocessFile(file)
  }
}