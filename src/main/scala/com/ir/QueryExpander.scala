package com.ir

import scala.collection.mutable

/**
  * Created by neele, holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("is", "this", "the", "of", "in", "to", "per", "the", "by", "a")
  val unigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val bigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val trigrams = mutable.HashMap[String, Array[Array[Int]]]()
  var text_as_unigrams = Array[String]()
  var num_of_docs = 0

  /**
    * calculates the inverse document frequency for a given term,
    * can be extracted by using the number of total documents and the number of documents
    * containing the term
    *
    * @param term a String
    * @return the IDF as a Float
    */
  def getIDF(term: String): Float = {
    val df = unigrams.getOrElse(term, Array()).length
    val idf = Math.log(num_of_docs / df).toFloat
    idf
  }

  /**
    * take a term and sum all the frequencies from all documents that contain that term
    * @param term
    * @return the total frequency of a term in a corpus
    */
  def get_frequency(term: String): Int = {
    val docList = unigrams(term)
    docList.map(el =>el(1)).sum
  }

  /**
    * take a list of candidate completion words and return the sum of the product of the frequency and the IDF of a term:
    * Sum: (#candidate*IDF(candidate)
    * this can be used as the normalization factor in the probability calculation for the most probable term
    * @param candidates
    * @return a Float = normalization factor
    */
  def get_sum_of_IDFs(candidates: Array[String]):Float = {
    val IDFs = candidates.map(candidate => getIDF(candidate)*get_frequency(candidate)).sum
    IDFs
  }

  /**
    * This calculation gives the probability that a term is the completion:
    * p(completion|partial word)
    * @param term a candidate completion word
    * @param normalizationfactor
    * @return the probability of that term
    */
  def completion_probability(term:String, normalizationfactor: Float): Float = {
    get_frequency(term)*getIDF(term)/normalizationfactor
  }

  /**
    * Given the beginning of a word, return the most probable completed word
    * @param start
    * @return
    */
  def get_most_probable(start: String): String = {
    val candidates = extract_candidates(start)
    val normalizationfactor = get_sum_of_IDFs(candidates)
    val probabilities = candidates.map(completion_probability(_, normalizationfactor))
    val best_candidate = candidates(probabilities.indexOf(probabilities.max))
    best_candidate
  }
  /**
    * for a given query word this method extracts all words that are possible word completions of that
    * query word
    *
    * @param start a String = query word
    * @return an Array of Strings that contains the candidate word completions
    */
  def extract_candidates(start: String): Array[String] = {
    val candidates = unigrams.keySet.filter(el => el.startsWith(start))
    candidates.toArray
  }


  def update_nGram_Map(ngram:String, ngramMap: mutable.HashMap[String, Array[Array[Int]]], docID:Int) = {
    if (!ngramMap.contains(ngram)) {
      ngramMap.put(ngram, Array(Array(docID, 1)))
    }
    else {
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
      if (new_docID) {
        doclist :+= Array(docID, 1)
        ngramMap.update(ngram, doclist)
      }
    }
  }

  def extract_ngrams(input: Array[String], docID:Int) = {

    var gramIndex = 0
    var bigram = Array[String]()
    var trigram = Array[String]()
    var bigram_completed = false
    import scala.util.control.Breaks._
    for (i <- input.indices) {
      if (!stopwords.contains(input(i))) {

        //skipping from non-stopword to non-stopwords equals all unigrams
        update_nGram_Map(input(i), unigrams, docID)

        bigram = Array()
        trigram = Array()

        //counts skip-grams by ignoring stopwords
        var bigramCounter = 0
        var trigramCounter = 0

        while (trigramCounter != 3) {
          if (gramIndex + i < input.length) {

            val token = input(gramIndex+i)

            bigram  :+= token
            trigram :+= token

            if (!stopwords.contains(token)) {

              if (bigramCounter < 2) {

                bigramCounter += 1
              }
              if (bigramCounter == 2) {
                update_nGram_Map(bigram.mkString(" "), bigrams, docID)
                bigramCounter = 3
              }

              if (trigramCounter < 3) {

                trigramCounter += 1
              }
              if (trigramCounter == 3) update_nGram_Map(trigram.mkString(" "), trigrams, docID)
            }
            gramIndex += 1
          }
          else trigramCounter = 3
        }
        gramIndex = 0
      }
    }
  }


  def main(args: Array[String]) {
    val pe = new PhraseExtractor

    if (args.length != 1) println("Not enough arguments!")
    else {
      val files = new java.io.File(args(0)).listFiles
      num_of_docs = files.size

      for (file <- files) {
        val words = pe.preprocessing(file.toString)
        val doc_id = file.toString.split("/").last.replace(".conll", "").toInt

        //println("doc_id: " + doc_id + ", file number: " + (files.indexOf(file)+1))
        extract_ngrams(words, doc_id)
      }

    }
  }


}
