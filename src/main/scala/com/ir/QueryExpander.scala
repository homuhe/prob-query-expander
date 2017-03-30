package com.ir
import java.util.regex.Pattern
import scala.collection.mutable
import scala.io.Source
import java.io.File

/** Author:       Holger Muth-Hellebrandt,
  *               Neele Witte
  *
  * Task:         IR Project WS16/17
  * Description:  Query expander as a probabilistic ngram ranking approach
  */
object QueryExpander {

  var stopwords = List("is", "this", "the", "of", "in", "to", "per", "the", "by", "a")
  val unigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val bigrams = mutable.HashMap[String, Array[Array[Int]]]()
  val trigrams = mutable.HashMap[String, Array[Array[Int]]]()
  var format = "conll" //default if argument not given
  var num_of_docs = 0
  var uni_norm: Float = 0
  var bi_norm: Float = 0
  var tri_norm: Float = 0



  def read_stopwords(file: String): List[String] = {
    Source.fromFile(file).getLines().toList
  }

  /**
    * extracts an array of tokens of given file format
    * @param file string that denotes the input file
    * @return array of tokens contained in file
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
      words = Source.fromFile(file).getLines()
        .filter(!_.isEmpty)
        .map(_.toLowerCase()).mkString
        .split(" ").toIterator
    }

    val spacePattern = Pattern.compile(delimiter)
    words.filter(!spacePattern.matcher(_).find()).toArray
  }

  /**
    * Creates ngrams from multiple input files
    * @param files input files to be processed
    */
  def create_ngrams(files: Array[File]): Unit = {

    var doc_id = 0

    for (file <- files) {
      num_of_docs += 1

      val words = preprocessing(file.toString, format)
      extract_ngrams(words, doc_id)
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

      update_ngramMap(tokens(i), unigrams, docID)

      if (!stopwords.contains(tokens(i))) {
        //only non-stopword tokens

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
    * Updates ngram maps due to 3 different possible cases:
    * case 1    = ngram is new -> put in map with freq 1
    * case 2.1  = ngram with docID is known       -> update freq for docID
    * case 2.2  = ngram is known but docID is new -> add docId to ngram with freq 1
    * @param ngram ngram to be updated
    * @param ngramMap according map of same order as ngram is
    * @param docID doc of which ngram was extracted
    */
  def update_ngramMap(ngram: String,
                      ngramMap: mutable.HashMap[String, Array[Array[Int]]], docID: Int): Unit = {

    if (!ngramMap.contains(ngram)) {              //case 1
      ngramMap.put(ngram, Array(Array(docID, 1)))
    }
    else {                                        //case 2.x
    var doclist = ngramMap(ngram)
      var index = 0
      var new_docID = true
      for (freqpair <- doclist) {
        if (freqpair.head == docID) {             //case 2.1
          doclist.update(index, Array(freqpair(0), freqpair(1) + 1))
          ngramMap.update(ngram, doclist)
          new_docID = false
        }
        index += 1
      }

      if (new_docID) {                            //case 2.2
        doclist :+= Array(docID, 1)                 //append new docID - freq entry
        ngramMap.update(ngram, doclist)
      }
    }
  }

  /**
    * for a given, incomplete query word Qt, this method extracts all possible word completions
    *
    * @param Qt a String = incomplete query word
    * @return an Array of Strings that contains the candidate word completions
    */
  def extract_candidates(Qt: String,
                         ngramMap: mutable.HashMap[String, Array[Array[Int]]]): Iterable[String] = {

    ngramMap.keys.filter(_.startsWith(Qt))
  }

  /**
    * calculates the inverse document frequency for a given term:
    * can be extracted by using the number of total documents and
    *   the number of documents containing the term
    *
    * @param term a String
    * @return the IDF as a Float
    */
  def getIDF(term: String): Float = {
    val df = unigrams.getOrElse(term, Array()).length

    Math.log(num_of_docs / df).toFloat
  }

  /**
    * Returns frequencies of a term by
    * summing through all freqs from all documents containing that term
    * @param term which can be a single token or a phrase
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
    * returns the sum of the product of the frequencies and the IDFs of term completion candidates:
    * Sum of candidates: (freq of candidate * IDF of candidate)
    * this is used as normalization factor in the probability calculation for the most probable term
    * @param candidates term completion candidates
    * @return a Float = normalization factor
    */
  def get_sum_of_FreqIDFs(candidates: Iterable[String]): Float = {
    candidates.map(candidate => get_frequency(candidate) * getIDF(candidate)).sum
  }

  /**
    * Extracts phrases of a given term completion candidate.
    * @param candidate term completion candidate
    * @return returns all ngram phrases beginning with the completion + order of the ngram
    */
  def extract_phrases(candidate: String): Array[(Iterable[String], Int)] = {
    Array((extract_candidates(candidate, unigrams), 1),
          (extract_candidates(candidate, bigrams), 2),
          (extract_candidates(candidate, trigrams), 3))
  }

  /**
    * Term Completion Probability:
    * the probability that term ci is the completion of partial user input Qt, p(ci|Qt)
    * @param ci a completion candidate for Qt
    * @param cm all completion candidates for Qt
    * @return conditional probability P = p(ci|Qt)
    */
  def term_completion_prob(ci: String, cm: Iterable[String]): Float = {
    (get_frequency(ci) * getIDF(ci)) / get_sum_of_FreqIDFs(cm)
  }

  /**
    * Term-to-Phrase Probability:
    * the probability that a phrase pij is the desired query phrase of the term completion ci. p(pij|ci)
    * @param pij a ngram phrase candidate pij of order m for term completion ci
    * @param m ngram order of pij, needed for the norm factor
    * @param phrases_m ngram phrases of order m
    * @return conditional probability P = p(pij|ci)
    */
  def term2phrase_prob(pij: String, m: Int, phrases_m: Iterable[String]) = {
    freqNorm(pij, m) / phrases_m.map(phrase => freqNorm(phrase, m)).sum
  }

  /**
    * Frequency normalization of ngram phrase of order m
    * @param phrase ngram phrase
    * @param m ngram order of phrase
    * @return frequenzy normalization factor
    */
  def freqNorm(phrase: String, m: Int): Float = {
    get_frequency(phrase) / Math.log(get_avg_nGram_freq(m)).toFloat
  }

  /**
    * Retrieving ngram norm of order m
    * @param m order m of desired ngram norm
    * @return ngram norm of order m
    */
  def get_avg_nGram_freq(m: Int): Float = {
    if (m == 1) uni_norm
    else if (m == 2) bi_norm
    else tri_norm

  }

  /**
    * pre-computing ngram norms
    */
  def generate_nGram_norms() {
    uni_norm = unigrams.keys.map(key => unigrams(key).map(_ (1).toFloat).sum).sum / unigrams.keys.size
    bi_norm = bigrams.keys.map(key => bigrams(key).map(_(1).toFloat).sum).sum / bigrams.keys.size
    tri_norm = trigrams.keys.map(key => trigrams(key).map(_(1).toFloat).sum).sum / trigrams.keys.size
  }

  /**
    * Phrase-Query Correlation:
    * the probability that a phrase pi is the 2nd half of the desired query,
    * where the 1st half is the complete user input Qc.
    *   Simplification: P = |docs containing Qc & pi| / |docs containing pi|
    *
    * @param Qc complete user input aka context, one or more complete terms
    * @param pi phrase aka completion of desired query in context Qc.
    * @return Phrase Query Correlation probability P = (Qc|pi)
    */
  def phrase_query_corr(Qc: Array[String], pi: String): Float = {
    def get_postings(Qc: Array[String]): Array[Int] = {
      Qc.map(token => unigrams(token).map(_(0))).reduceLeft(_.intersect(_))
    }

    var posting_pi = Array[Int]()

    if (unigrams.contains(pi))      posting_pi = unigrams(pi).map(_(0))
    else if (bigrams.contains(pi))  posting_pi = bigrams(pi).map(_(0))
    else                            posting_pi = trigrams(pi).map(_(0))

    val posting_Qc_pi = get_postings(Qc).intersect(posting_pi)
    posting_Qc_pi.length.toFloat / posting_pi.length.toFloat
  }

  /**
    * Prints top k query suggestions.
    * Filters out highly unlikely queries before sorting.
    * Pre-sorting of length for ranks with same score.
    * @param ranks computed rank of query suggestion
    * @param k number of top k suggestions printed
    */
  def print_ranks(ranks: mutable.HashMap[String, Float], k: Int): Unit = {
    ranks
      .toSeq.filter(_._2 > 0.001)
      .sortBy(_._1.length)
      .sortWith(_._2 > _._2)
      .take(k)
      .foreach(rank => println(rank._1 + " " + rank._2))
  }


  /**
    * Main method, query and argument handling
    * @param args 1st obligatory argument: corpus directory (raw or conll files) for ngram extraction
    *               2nd optional argument: corpus format, "conll"       = conll
    *                                      anything else except "conll" = raw
    *                                      default = "conll"
    *               3rd optional argument: stopwords list, each line one stopword
    *
    */
  def main(args: Array[String]) {

    if (args.length < 1) help()
    else {
      if (args.length == 3) stopwords = read_stopwords(args(2))
      if (args.length == 2) format = args(1)

      val input = new File(args(0)).listFiles
      val files = input.take(input.length / 2)

      create_ngrams(files)
      generate_nGram_norms()

        while (true) {
          try {
          print("prob-query-expander: ")

          val input = scala.io.StdIn.readLine()

          val Qk1 = input.split(" ")
          val Qc = Qk1.init
          val Qt = Qk1.last
          val term_completion_candidates = extract_candidates(Qt, unigrams)

          val completion_ranks = term_completion_candidates
            .map(candidate => (candidate, term_completion_prob(candidate, term_completion_candidates)))

          if (Qc.length == 0) {
            completion_ranks.toArray.sortWith(_._2 > _._2)
                                    .foreach(x => println(x._1 + ", " + x._2))
            println()
          }

          else {
            // ranks: phrase, score
            val ranks = mutable.HashMap[String, Float]()

            for ((ci, term_comp_prob) <- completion_ranks) {

              for ((phrases, order) <- extract_phrases(ci)) {

                for (phrase <- phrases) {

                  //PHRASE SELECTION PROBABILITY = TERM COMPLETION PROB x TERM TO PHRASE PROB
                  val phrase_selection_prob = term_comp_prob * term2phrase_prob(phrase, order, phrases)

                  //PHRASE QUERY CORRELATION
                  val phrase_query_correlation = phrase_query_corr(Qc, phrase)

                  //TOTAL PROBABILITY
                  val p = phrase_selection_prob * phrase_query_correlation
                  if (!ranks.contains(phrase))
                    ranks.put(phrase, p)
                  else if (ranks.contains(phrase) && p > ranks(phrase))
                    ranks.update(phrase, p)
                }
              }
            }

            print_ranks(ranks, 20)
            println
          }
        }
          catch { case x:Exception => println("Not found...\n")}
      }
    }

    /**
      * Helper method
      */
    def help(): Unit = {
      println("Usage: ./prob-query-expander arg1 [opt1] [opt2]")
      println("\t\targ1: CORPUS DIRECTORY\t - directory with text files, either raw or conll")
      println("\t\topt1: FORMAT\t           - 'conll', 'raw', default = 'conll'")
      println("\t\topt2: STOPWORDS\t        - list of stopwords, each line one stopword")
      sys.exit()
    }
  }
}
