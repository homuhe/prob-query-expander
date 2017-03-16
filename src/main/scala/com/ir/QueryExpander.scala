package com.ir

import scala.collection.mutable

/**
  * Created by neele, holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("of", "in", "to", "per", "the", "by", "a")
  val unigrams  = mutable.HashMap[String, Array[Array[Int]]]()
  val bigrams   = mutable.HashMap[String, Array[Array[Int]]]()
  val trigrams  = mutable.HashMap[String, Array[Array[Int]]]()
  var text_as_unigrams = Array[String]()
  var num_of_docs = 0

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
  def update_nGram_Map(ngram:String, ngramMap: mutable.HashMap[String, Array[Array[Int]]], docID:Int) :Unit= {
    if (!ngramMap.contains(ngram)) {
      ngramMap.put(ngram, Array(Array(docID, 1)))
    }
    else {
      {
        var doclist = ngramMap(ngram)
        var index = 0
        var new_docID = true
        for (freqpair <- doclist) {
          if (freqpair.head == docID) {
            doclist.update(index, Array(freqpair(0), freqpair(1) + 1))
            ngramMap.update(ngram, doclist)
            new_docID = false
          }
          index += 1
        }
        if (new_docID){
          doclist :+= Array(docID, 1)
          ngramMap.update(ngram, doclist)
        }
      }
  }
  def extract_ngrams(input: Array[String], stopwords:List[String], docID:Int) = {
    var bigramcounter = 0
    var trigramcounter = 0
    var gramIndex = 0
    var bigram = Array[String]()
    var trigram = Array[String]()

    for (i <- input.indices) {
      if (!stopwords.contains(input(i))) {
        bigramcounter = 0
        trigramcounter = 0
        gramIndex = 0
        bigram = Array()
        trigram = Array()
        while (trigramcounter != 3 && gramIndex<input.length) {
          if (bigramcounter == 1) {
            update_nGram_Map(bigram.mkString(" "), bigrams, docID)
          }
          gramIndex = i + gramIndex
          val actualword = input(gramIndex)
          bigram += actualword
          trigram += actualword
          if (!stopwords.contains(actualword)) {
            bigramcounter += 1
            trigramcounter += 1
          }
        }
        update_nGram_Map(trigram.mkString(" "), trigrams, docID)
      }
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

        //println("doc_id: " + doc_id + ", file number: " + (files.indexOf(file)+1))
        //extract_ngrams(words, stopwords, doc_id)
      }


      println(num_of_docs)
      extract_ngrams("this is house of cards the new house of cards house of cards".split(" "), stopwords, 1)
      //extract_ngrams("This is House of the Cards. The new House of Cards. House of the Cards.".split(" "), stopwords, 1)
/*
      for (unigram <- bigrams) {
        println("<" + unigram._1.mkString(" ") + "> ")
       for (freqpair <- unigram._2) println(" in doc " + freqpair(0) + " with frequency " + freqpair(1))
      }*/
    }
  }


}
