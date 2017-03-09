package com.ir

import scala.collection.mutable

/**
  * Created by holger on 09.03.17.
  */
object QueryExpander {

  val unigrams  = mutable.HashMap[String, Int]()
  val bigrams   = mutable.HashMap[Array[String], Int]()
  val trigrams  = mutable.HashMap[Array[String], Int]()

  class ngram(docID: Int, input: Array[String]) {

  }

  def extract_ngrams(input: String) = {

  }

  def main(args : Array[String]) {
    println("Hello group member")
  }


}
