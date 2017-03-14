package com.ir

import scala.collection.mutable

/**
  * Created by neele, holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("of", "in", "to", "per", "the", "by", "a")
  val unigrams  = mutable.HashMap[String, Array[Array[Int]]]()
  val bigrams   = mutable.HashMap[Array[String], Array[Array[Int]]]()
  val trigrams  = mutable.HashMap[Array[String], Array[Array[Int]]]()

  var num_of_docs = 0

  class ngram(docID: Int, input: Array[String]) {
  }

  /**
    * calculates the inverse document frequency for a given term,
    * can be extracted by using the number of total documents and the number of documents
    * containing the term
    * @param term a String
    * @return the IDF as a Float
    */
  def getIDF(term:String) : Float = {
    val df = unigrams.getOrElse(term, Array()).length
    val idf = Math.log(num_of_docs/df).toFloat
    idf
  }

  /**
    * for a given query word this method extracts all words that are possible word completions of that
    * query word
    * @param start a String = query word
    * @return an Array of Strings that contains the candidate word completions
    */
  def extract_candidates(start: String): Array[String] = {
    val candidates = unigrams.keySet.filter(el => el.startsWith(start))
    candidates.toArray
  }

  /**
    *
    * @param input words of document
    * @param stopwords List of stopwords
    * @param docID docID of the current document
    */
  def extract_ngrams(input: Array[String], stopwords:List[String], docID:Int) = {

    var uni = 0
    var bi  = 0
    var tri = 0

    var unigram = ""
    var bigram  = Array[String]()
    var trigram = Array[String]()


    for (word <- input) {
      if (uni == 1) {
        //put in map
        if (!unigrams.contains(unigram.trim)){
          unigrams.put(unigram.trim, Array(Array(docID, 1)))
        }
        else {
          val doclist = unigrams(unigram.trim)
          val doc_freqpair = unigrams(unigram.trim).filter(el => el.head == docID).flatten
          val index = doclist.indexOf(doc_freqpair)
          doclist.update(index, Array(doc_freqpair(0), doc_freqpair(1)+1))
          unigrams.update(unigram.trim, doclist)
        }
        //make variables empty
        unigram = ""
        uni = 0
      }
      if (bi == 2) {
        //put bigram in map
        if (!bigrams.contains(bigram)){
          bigrams.put(bigram, Array(Array(docID, 1)))
        }
        else {
          val doclist = bigrams(bigram)
          val doc_freqpair = bigrams(bigram).filter(_ == docID).flatten
          val index = doclist.indexOf(doc_freqpair)
          doclist.update(index, Array(doc_freqpair(0), doc_freqpair(1)+1))
          bigrams.update(bigram, doclist)
        }
        //make variable empty
        bigram = Array[String]()
        bi = 0
      }
      if (tri == 3) {// put trigram in map
        if (!trigrams.contains(trigram)){
          trigrams.put(trigram, Array(Array(docID, 1)))
        }
        else {
          val doclist = trigrams(trigram)
          val doc_freqpair = trigrams(trigram).filter(_== docID).flatten
          val index = doclist.indexOf(doc_freqpair)
          doclist.update(index, Array(doc_freqpair(0), doc_freqpair(1)+1))
          trigrams.update(trigram, doclist)
        }
        //make variable empty
        trigram = Array[String]()
        tri = 0
      }
      //append next word
      unigram += " " + word
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

    if (args.length != 1) println("Not enough arguments!")
    else {
      val files = new java.io.File(args(0)).listFiles
      num_of_docs = files.size

      for (file <- files) {
        val words = pe.preprocessing(file.toString)
        val doc_id = file.toString.split("/").last.replace(".conll", "").toInt


        //println(id)
        extract_ngrams(words, stopwords, doc_id) //TODO possible error
      }
     print(num_of_docs)
    }
  }


}
