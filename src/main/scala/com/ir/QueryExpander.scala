package com.ir

import scala.collection.mutable

/**
  * Created by holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("of", "in", "to", "per", "the", "by", "a")
  val unigrams  = mutable.HashMap[Array[String], List[Int]]()
  val bigrams   = mutable.HashMap[Array[String], List[Int]]()
  val trigrams  = mutable.HashMap[Array[String], List[Int]]()

  class ngram(docID: Int, input: Array[String]) {
  }


  def extract_ngrams(input: Array[String], stopwords:List[String], docID:Int) = {
    var uni = 0
    var bi  = 0
    var tri = 0

    var unigram = Array[String]()
    var bigram  = Array[String]()
    var trigram = Array[String]()


    for (word <- input) {
      if (uni == 1) {
        //put in map
        val value = unigrams.getOrElseUpdate(unigram, List())
        val newvalue = docID::value
        unigrams.update(unigram, newvalue)
        //make variables empty
        unigram = Array[String]()
        uni = 0
      }
      if (bi == 2) {
        //put bigram in map
        val value = bigrams.getOrElseUpdate(bigram, List())
        val newvalue = docID::value
        bigrams.update(unigram, newvalue)
        //make variable empty
        bigram = Array[String]()
        bi = 0
      }
      if (tri == 3) {
        // put trigram in map
        val value = trigrams.getOrElseUpdate(trigram, List())
        val newvalue = docID::value
        trigrams.update(trigram, newvalue)
        //make variable empty
        trigram = Array[String]()
        tri = 0
      }
      //append next word
      unigram :+= word
      bigram  :+= word
      trigram :+= word
      //update counter if word is content word
      if (!stopwords.contains(word)) {
        uni += 1
        bi  += 1
        tri += 1
      }
    }
  }

  def main(args : Array[String]) {
    val pe = new PhraseExtractor
    val  f = "testcorpus/53293.conll"
    val words = pe.preprocessing(f)
    extract_ngrams(words, stopwords, 122)
    //println(unigrams.size)

  }


}
