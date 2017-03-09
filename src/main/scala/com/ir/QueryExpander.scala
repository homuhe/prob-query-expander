package com.ir

import scala.collection.mutable

/**
  * Created by holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("of", "in")
  val unigrams  = mutable.HashMap[Array[String], Int]()
  val bigrams   = mutable.HashMap[Array[Array[String]], Int]()
  val trigrams  = mutable.HashMap[Array[Array[String]], Int]()

  class ngram(docID: Int, input: Array[String]) {
  }


  def extract_ngrams(input: Array[String]) = {
    var uni = 0
    var bi  = 0
    var tri = 0

    var unigram = String
    var bigram  = Array[String]()
    var trigram = Array[String]()


    for (word <- input) {
      unigram   = word
      bigram  :+= word
      trigram :+= word
      if (!stopwords.contains(word)) {
        uni += 1
        bi  += 1
        tri += 1
      }
      

    }

  }

  def main(args : Array[String]) {
    println("Hello group member")
  }


}
