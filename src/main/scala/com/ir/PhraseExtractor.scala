package com.ir

/**
  * Created by root on 09.03.17.
  */
import scala.io._

class PhraseExtractor {
  def preprocessFile(inputfile: String) = {
    val words = Source.fromFile(inputfile).getLines().map(_.split("\t")(1)


  }
}
object PhraseExtractor {
  def main(args: Array[String]): Unit = {
    val file = "/home/neele/Dokumente/InformationRetrieval/query-hyb/testcorpus/53293.conll"
    println("test")
  }
}