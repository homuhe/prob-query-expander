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
  val docs2IDs = mutable.HashMap[String, Int]()
  var num_of_words = 0
  var format = ""

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
    val num_of_docs = get_num_docs()

    Math.log(num_of_docs / df).toFloat
  }

  /**
    * take a term and sum all the frequencies from all documents that contain that term
    * @param term
    * @return the total frequency of a term in a corpus
    */
  def get_frequency(term: String, ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Int = {
    ngramMap(term).map(_(1)).sum
  }

  /**
    * take a list of candidate completion words and return the sum of the product of the frequency and the IDF of a term:
    * Sum: (#candidate*IDF(candidate)
    * this can be used as the normalization factor in the probability calculation for the most probable term
    * @param candidates
    * @return a Float = normalization factor
    */
  def get_sum_of_IDFs(candidates: Array[String], ngramMap: mutable.HashMap[String, Array[Array[Int]]]):Float = {

    candidates.map(candidate => getIDF(candidate) * get_frequency(candidate, ngramMap)).sum
  }

  /**
    * This calculation gives the probability that a term is the completion:
    * p(completion|partial word)
    * @param term a candidate completion word
    * @param normalizationfactor
    * @return the probability of that term
    */
  def completion_probability(term:String,
                             normalizationfactor: Float,
                             ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Float = {

    get_frequency(term, ngramMap) * getIDF(term) / normalizationfactor
  }

  /**
    * for a given query word this method extracts all words that are possible word completions of that
    * query word
    *
    * @param start a String = query word
    * @return an Array of Strings that contains the candidate word completions
    */
  def extract_candidates(start: String,
                         ngramMap: mutable.HashMap[String, Array[Array[Int]]]) = {

    ngramMap.keySet.filter(_.startsWith(start)).toArray
  }

  def get_avg_nGram_freq( ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Float = {
    ngramMap.keySet.map(key => ngramMap(key).map(_(1)).sum).sum/ngramMap.keySet.size
  }

  def nGram_norm(ngram: String, ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Double = {
    get_frequency(ngram, ngramMap) / Math.log(get_avg_nGram_freq(ngramMap))
  }

  def extract_phrase_candidates(term:String):(Array[String], Array[String]) = {
    val bigramcandidates = bigrams.keySet.filter(_.contains(term))
    val trigramcandidates = trigrams.keySet.filter(_.contains(term))

    (bigramcandidates.toArray, trigramcandidates.toArray)
  }

  def term_phrase_probability(term: String, phrase:String,
                              ngramMap: mutable.HashMap[String, Array[Array[Int]]]) = {

    nGram_norm(phrase, ngramMap)//TODO /
  }

  /**
    * //TODO
    * @param ngram
    * @param ngramMap
    * @param docID
    * @return
    */
  def update_nGram_Map(ngram: String, ngramMap: mutable.HashMap[String, Array[Array[Int]]], docID:Int) = {

    if (!ngramMap.contains(ngram)) {              //ngram new to Map -> new entry with new docID & freq 1
      ngramMap.put(ngram, Array(Array(docID, 1)))
    }
    else {                                        //ngram already in Map, either with same or new docID
      var doclist = ngramMap(ngram)
      var index = 0
      var new_docID = true
      for (freqpair <- doclist) {
        if (freqpair.head == docID) {             //ngram with docID exists in Map -> update freq
          doclist.update(index, Array(freqpair(0), freqpair(1) + 1))
          ngramMap.update(ngram, doclist)
          new_docID = false
        }
        index += 1
      }

      if (new_docID) {                            //ngram exists in Map, but given docID is new
        doclist :+= Array(docID, 1)                 //append new docID - freq entry
        ngramMap.update(ngram, doclist)
      }
    }
  }

  /**
    * Extracts uni/bi/trigrams (skipgrams) by iterating once over all non-stopwords + lookahead.
    *
    * @param tokens input of given document
    * @param docID current document ID
    */
  def extract_ngrams(tokens: Array[String], docID:Int) = {

    for (i <- tokens.indices) {

      num_of_words += 1

      if (!stopwords.contains(tokens(i))) {

        //unigrams: only non-stopword tokens
        update_nGram_Map(tokens(i), unigrams, docID)

        var bigram = Array[String]()
        var trigram = Array[String]()

        //skip-gram counter
        var ngramCounter = 0
        var bigram_complete = false
        var trigram_complete = false

        var lookahead_i = 0

        while (i + lookahead_i < tokens.length) { //Index + Lookahead: max. array length

          val token = tokens(i + lookahead_i)
          bigram  :+= token
          trigram :+= token

          if (!stopwords.contains(token)) {
              ngramCounter += 1

            if (ngramCounter == 2 && !bigram_complete) {
              update_nGram_Map(bigram.mkString(" "), bigrams, docID)
              bigram_complete = true
            }

            else if (ngramCounter == 3 && !trigram_complete) {
              update_nGram_Map(trigram.mkString(" "), trigrams, docID)
              trigram_complete = true
              lookahead_i = tokens.length //soft break of while condition
            }
          }
          lookahead_i += 1
        }
      }
    }
  }

  def get_num_docs() = docs2IDs.size

  def main(args: Array[String]) {
    val pe = new PhraseExtractor

    if (args.length < 1) println("Not enough arguments!")
    else {
      if (args.length == 2) format = "conll"

      val files = new java.io.File(args(0)).listFiles

      var doc_id = 0

      for (file <- files) {
        val words = pe.preprocessing(file.toString, format)
        val doc = file.toString.split("/").last//.replace(".conll", "").toInt

        docs2IDs.put(doc, doc_id)

        //println("doc_id: " + doc_id + ", file number: " + (files.indexOf(file)+1))
        extract_ngrams(words, doc_id)
        
        doc_id += 1
      }

      var input = ""
      var prev_input = " "
      while (true) {
        print("\nquery-expander: ")

        if (input != prev_input) {
          prev_input = input
          input += scala.io.StdIn.readLine(input)
        }
        else input = scala.io.StdIn.readLine()

        val candidates =  extract_candidates(input, unigrams)
                              .map(unigram => (unigram, get_frequency(unigram, unigrams))) ++
                          extract_candidates(input, bigrams)
                              .map(bigram => (bigram, get_frequency(bigram, bigrams))) ++
                          extract_candidates(input, trigrams)
                              .map(trigram => (trigram, get_frequency(trigram, trigrams)))

        candidates.sortBy(_._2)   //sort by score
                  .reverse        //descending order
                  .take(10)       //top 10 results
                  .foreach(tuple => println(tuple._1 + " " + tuple._2))//._1))

        input = candidates.sortBy(_._2)   //sort by score
                          .reverse        //descending order
                          .head._1
      }

      unigrams
      bigrams
      trigrams
      num_of_words
      docs2IDs
      val x = "bla"

    }
  }


}
