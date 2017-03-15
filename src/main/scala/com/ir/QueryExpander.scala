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

        unigram = unigram.trim

        //case1: unigram not in Map, yet
        if (!unigrams.contains(unigram))
          unigrams.put(unigram, Array(Array(docID, 1)))
        //case2a & 2b: unigram is in Map, occurance either in existing docID or new docID
        else {
          var doclist = unigrams(unigram)
          var index = 0
          var new_docID = true
          //case2a: unigram with existing docID, update frequency in found Array
          for (freqpair <- unigrams(unigram)) {
            if (freqpair.head == docID) {
              doclist.update(index, Array(freqpair(0), freqpair(1)+1))
              unigrams.update(unigram, doclist)
              new_docID = false
            }
            index += 1
          }
          //case2b:
          if (new_docID) {
            doclist :+= Array(docID, 1)
            unigrams.update(unigram, doclist)
          }
        }
        //make variables empty
        unigram = ""
        uni = 0
      }

      if (bi == 2) {

        //case1: bigram not in Map, yet
        if (!bigrams.contains(bigram))
          bigrams.put(bigram, Array(Array(docID, 1)))
        //case2a & 2b: bigram is in Map, occurance either in existing docID or new docID
        else {
          var doclist = bigrams(bigram)
          var index = 0
          var new_docID = true
          //case2a: bigram with existing docID, update frequency in found Array
          for (freqpair <- bigrams(bigram)) {
            if (freqpair.head == docID) {
              doclist.update(index, Array(freqpair(0), freqpair(1)+1))
              bigrams.update(bigram, doclist)
              new_docID = false
            }
            index += 1
          }
          //case2b:
          if (new_docID) {
            doclist :+= Array(docID, 1)
            bigrams.update(bigram, doclist)
          }
        }
        //make variables empty
        bigram = Array[String]()
        bi = 0
      }

      if (tri == 3) {

        //case1: trigram not in Map, yet
        if (!trigrams.contains(trigram))
          trigrams.put(trigram, Array(Array(docID, 1)))
        //case2a & 2b: trigram is in Map, occurance either in existing docID or new docID
        else {
          var doclist = trigrams(trigram)
          var index = 0
          var new_docID = true
          //case2a: trigram with existing docID, update frequency in found Array
          for (freqpair <- trigrams(trigram)) {
            if (freqpair.head == docID) {
              doclist.update(index, Array(freqpair(0), freqpair(1)+1))
              trigrams.update(bigram, doclist)
              new_docID = false
            }
            index += 1
          }
          //case2b:
          if (new_docID) {
            doclist :+= Array(docID, 1)
            trigrams.update(trigram, doclist)
          }
        }
        //make variables empty
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


        println("doc_id: " + doc_id + ", file number: " + (files.indexOf(file)+1))
        //extract_ngrams("This is House of Cards".split(" "), stopwords, doc_id)
      }
      println(num_of_docs)
      extract_ngrams("This is House of Cards. The new House of Cards. House of Cards.".split(" "), stopwords, 1)


      for (unigram <- unigrams) {
        print("unigram <" + unigram._1 + ">\t\t")
        for (freqpair <- unigram._2)
          println("in doc " + freqpair(0) + " with frequency " + freqpair(1))
      }
    }
  }


}
