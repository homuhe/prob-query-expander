package com.ir

import java.util.regex.Pattern
import scala.collection.mutable
import scala.io.Source
import java.io.File

/**
  * Created by neele, holger on 09.03.17.
  */
object QueryExpander {

  val stopwords = List("is", "this", "the", "of", "in", "to", "per", "the", "by", "a")
  val unigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val bigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val trigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val docs2IDs = mutable.HashMap[String, Int]()
  var num_of_words = 0 //TODO: can be deleted
  var format = ""
  var uni_norm: Float = 0
  var bi_norm: Float = 0
  var tri_norm: Float = 0

  /**
    * extracts a word array of a conll format file that contains only words (no punctuation) and is lowercased
    * @param file : a String that denotes the input file
    * @return the array of words contained in that file
    */
  def preprocessing(file: String, format: String): Array[String] = {
    val delimiter = "[ \t\n\r,.?!\\-:;()\\[\\]'\"/*#&$]+"

    var words = Iterator[String]()

    if (format == "conll") {
      words = Source.fromFile(file).getLines()
        .filter(!_.isEmpty)
        .map(_.split("\t")(1))
        .map(_.toLowerCase())
    }
    else {
      words = Source.fromFile(file.toString).getLines()
        .filter(!_.isEmpty)
        .map(_.toLowerCase()).mkString
        .split(" ").toIterator
    }

    val spacePattern = Pattern.compile(delimiter)
    words.filter(!spacePattern.matcher(_).find()).toArray
  }

  /**
    * //TODO
    * @param files
    */
  def create_ngrams(files: Array[File]): Unit = {

    var doc_id = 0

    for (file <- files) {
      val words = preprocessing(file.toString, format)
      val doc = file.toString.split("/").last//.replace(".conll", "").toInt

      //println("Reading doc " + doc + ", new docID: " + doc_id)//(files.indexOf(file)+1))
      extract_ngrams(words, doc_id)
      docs2IDs.put(doc, doc_id)

      doc_id += 1
    }
  }

  /**
    * Extracts uni/bi/trigrams (skipgrams) by iterating once over all non-stopwords + lookahead.
    *
    * @param tokens input of given document
    * @param docID current document ID
    */
  def extract_ngrams(tokens: Array[String], docID:Int): Unit = {

    for (i <- tokens.indices) {

      num_of_words += 1

      if (!stopwords.contains(tokens(i))) {

        //unigrams: only non-stopword tokens
        update_ngramMap(tokens(i), unigrams, docID)

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
              update_ngramMap(bigram.mkString(" "), bigrams, docID)
              bigram_complete = true
            }

            else if (ngramCounter == 3 && !trigram_complete) {
              update_ngramMap(trigram.mkString(" "), trigrams, docID)
              trigram_complete = true
              lookahead_i = tokens.length //soft break of while condition
            }
          }
          lookahead_i += 1
        }
      }
    }
  }

  /**
    * //TODO
    * @param ngram
    * @param ngramMap
    * @param docID
    * @return
    */
  def update_ngramMap(ngram: String,
                      ngramMap: mutable.HashMap[String, Array[Array[Int]]], docID:Int): Unit = {

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
    * for a given query word this method extracts all words that are possible word completions of that
    * query word
    *
    * @param Qt a String = query word
    * @return an Array of Strings that contains the candidate word completions
    */
  def extract_candidates(Qt: String,
                         ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Iterable[String] = {

    ngramMap.keys.filter(_.startsWith(Qt))
  }

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

    Math.log(get_num_docs() / df).toFloat
  }

  /**
    * take a term and sum all the frequencies from all documents that contain that term
    * @param term
    * @return the total frequency of a term in a corpus
    */
  def get_frequency(term: String): Int = {
    if (unigrams.contains(term))
      unigrams(term).map(_(1)).sum
    else if (bigrams.contains(term))
      bigrams(term).map(_(1)).sum
    else
      trigrams(term).map(_(1)).sum
  }

  /**
    * take a list of candidate completion words and return the sum of the product of the frequency and the IDF of a term:
    * Sum: (#candidate*IDF(candidate)
    * this can be used as the normalization factor in the probability calculation for the most probable term
    * @param candidates
    * @return a Float = normalization factor
    */
  def get_sum_of_FreqIDFs(candidates: Iterable[String]): Float = {
    candidates.map(candidate => get_frequency(candidate) * getIDF(candidate)).sum
  }

  /**
    * This calculation gives the probability that a term is the completion:
    * p(completion|partial word)
    * @param candidate a candidate completion word
    * @param candidates
    * @return the probability of that term
    */
  def term_completion_prob(candidate: String, candidates: Iterable[String]): Float = {
    (get_frequency(candidate) * getIDF(candidate)) / get_sum_of_FreqIDFs(candidates)
  }

  /**
    * TODO
    * @param candidates
    * @param ngramMap
    * @return
    */
  def extract_phrases(candidates: Iterable[String],
                      ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Iterable[String] = { //to DELETE

    candidates.flatMap(x => extract_candidates(x, ngramMap))
  }

  def extract_phrases(candidate: String): Array[(Iterable[String], Int)] = {
    Array((extract_candidates(candidate, unigrams), 1),
          (extract_candidates(candidate, bigrams), 2),
          (extract_candidates(candidate, trigrams), 3))
  }

  /**
    * TODO
    * @param phrase
    * @param order
    * @return
    */
  def term2phrase_prob(phrase:String, order: Int) = {

/*    if (order == 1)
      nGram_norm(phrase, unigrams)
    else if (order == 2)
      nGram_norm(phrase, bigrams)
    else
      nGram_norm(phrase, trigrams)*/

    nGram_norm(phrase, order)
  }

  /**
    * TODO
    * @param ngram
    * @param order
    * @return
    */
  def nGram_norm(ngram: String, order: Int): Float = {
    get_frequency(ngram) / Math.log(get_avg_nGram_freq(order)).toFloat
  }

  /**
    * //TODO
    * @param order
    * @return
    */
  def get_avg_nGram_freq(order: Int): Float = {
    if (order == 1) uni_norm
    else if (order == 2) bi_norm
    else tri_norm

  }

  def generate_nGram_norms() {
    uni_norm  = unigrams.keys.map(key => unigrams(key).map(_(1)).sum).sum / unigrams.keys.size
    bi_norm    =  bigrams.keys.map(key => bigrams(key).map(_(1)).sum).sum / bigrams.keys.size
    tri_norm  = trigrams.keys.map(key => trigrams(key).map(_(1)).sum).sum / trigrams.keys.size
  }

  /**
    * //TODO
    * @return
    */
  def get_num_docs() = docs2IDs.size

/*  /**
    * TODO
    * @param ranks
    * @param k
    */
  def print_ranks(ranks:  Array[((String, (String, Int)), Float)], k: Int): Unit = {
    ranks.sortBy(_._2)   //sort by score
      .reverse        //descending order
      .take(k)       //top k results
      .foreach(tuple => println("\ncandidate: " + tuple._1._1 +
                                  "\n phrase: " + tuple._1._2 +
                                   "\n score: " + tuple._2))
    println()
  }*/

  /**
    * TODO
    * @param ranks
    * @param k
    */
  def print_ranks(ranks: mutable.HashMap[String, Float], k: Int): Unit = {
    ranks
      .toSeq.filter(_._2 > 0.001)
      .sortBy(_._2)
      .reverse.take(k)
      .foreach(rank => println(rank._1 + " " + rank._2))
  }




  def main(args: Array[String]) {

    if (args.length < 1) println("Not enough arguments!")
    else {
      if (args.length == 2) format = args(1)

      val files = new File(args(0)).listFiles

      create_ngrams(files)
      generate_nGram_norms()

      while (true) {
        print("query-expander: ")

        val input = scala.io.StdIn.readLine()

        val Qk1 = input.split(" ")
        var Qc  = Qk1.init
        val Qt  = Qk1.last
        if (Qc.length == 0) Qc = Array(Qt) //TODO

        val term_completion_candidates =  extract_candidates(Qt , unigrams).toSet

        val completion_ranks = term_completion_candidates
          .map(candidate => (candidate, term_completion_prob(candidate, term_completion_candidates)))

        // ranks: phrase, score
        val ranks = mutable.HashMap[String, Float]()

        for ((ci, term_comp_prob) <- completion_ranks) {

          val phrases_all_orders = extract_phrases(ci)
          for ((phrases, order) <- phrases_all_orders) {

            for (phrase <- phrases) {

              //PHRASE SELECTION PROBABILITY = TERM COMPLETION PROB x TERM TO PHRASE PROB
              val phrase_selection_prob = term_comp_prob * term2phrase_prob(phrase, order)

              if (!ranks.contains(phrase))
                ranks.put(phrase, phrase_selection_prob)
              else if (ranks.contains(phrase) && phrase_selection_prob > ranks(phrase))
                ranks.update(phrase, phrase_selection_prob)
            }

          }
        }

        //phrases & the order of the phrase
/*        val phrase_candidates = ((term_completion_candidates, 1),
          (extract_phrases(term_completion_candidates, bigrams), 2),
          (extract_phrases(term_completion_candidates, trigrams), 3))*/

/*        val term2phrase_ranks = phrase_candidates._1._1
          .map(phrase => (phrase, term2phrase_prob(phrase, 1))) ++
                                  phrase_candidates._2._1
          .map(phrase => (phrase, term2phrase_prob(phrase, 1))) ++
                                  phrase_candidates._3._1
          .map(phrase => (phrase, term2phrase_prob(phrase, 1)))*/

/*
        println("\nTerm completion ranks for: " + input)
        print_ranks(completion_ranks.toArray, 10)

        println("\nTerm-to-Phrase ranks for: " + input)
        print_ranks(term2phrase_ranks.toArray, 10)*/

        println("\nPhrase Selection Probability ranks for: " + input)
        print_ranks(ranks, 20)


        unigrams
        bigrams
        trigrams
        docs2IDs
        //phrase_candidates
        //term2phrase_ranks
        val x = "bla" //set breakpoint here to see data structures
      }
    }
  }


}
