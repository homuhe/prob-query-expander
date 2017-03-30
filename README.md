# prob-query-expander
Query expansion without query logs - a probabilistic approach

Expands any given query by extracting ngrams of a given corpus. Ranking of the suggestions is computed after the approach of [Bhatia, Majumda, and Mitra, "Query suggestions in the absence of query logs.", 2011.](http://www.tyr.unlu.edu.ar/tallerIR/2013/papers/querysuggestion.pdf).

The assumption is, that given an user input Q and a possible query suggestion p_i, the rank of p_i is computed as following:
Rank pf p_i = phrase selection probability x phrase-query correlation
P(p_i | Q) = P(p_i | Q_t) x P(Q_c | p_i)

## Input
Sample input file is a word embedding trained on German Wikipedia by using the word2vec CBOW model with vector size 300 and a minimum word frequency of 50.

Required format for using cluster-kmeans:
```
word1 value1 value2 value3 ...
word2 value1 value2 value3 ...
...
```

## Usage
```
$ ./cluster-kmeans arg1 arg2 [opt1]
    arg1: INPUT FILE  - text file with embeddings")
    arg2: INTEGER     - number of desired clusters
                        min = 1; max = number of words in input file")
    opt1: FLOAT       - cluster movement tolerance, threshold to stop algorithm
                        min = 0.1E-15, max = Float.MaxValue")
```
Example run:
```
$ ./cluster-kmeans sample-input.txt 100 0.01
> 0 Seit
0 Ab
0 Von
0 Zum
1 Dichter
1 Mensch
1 Historiker
...
```

## Output
Program prints each word and its cluster on a line, separating the word and the cluster number by a space.


_

Authors: *Alexander Hartmann, Holger Muth-Hellebrandt*
