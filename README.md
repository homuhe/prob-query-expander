# prob-query-expander
Query expansion without query logs - a probabilistic approach

Expands any given query by extracting ngrams of a given corpus. Ranking of the suggestions is computed after the approach of [Bhatia, Majumda, and Mitra, "Query suggestions in the absence of query logs.", 2011.](http://www.tyr.unlu.edu.ar/tallerIR/2013/papers/querysuggestion.pdf)

The assumption is, that given an user input Q and a possible query suggestion p_i, the rank of p_i is computed as following:
```
Rank pf p_i = phrase selection prob. x phrase-query correlation
 P(p_i | Q) =       P(p_i | Qt)     x       P(Qc | p_i)
```
where the user input Q splits into Qc + Qt, Qc = possible complete input of the user which is interpreted as a context in the phrase-query correlation, and Qt = partial and not complete input of the user.

The computation of the phrase selection probability splits into two steps/calculations, completion c_i of the partial word (term completion probability) and then selection of a phrase containing that word completion c_i (term-to-phrase probability):
```
phrase selection prob. = term completion prob. x term-to-phrase prob.
      P(p_ij | Qt)      =       P(c_i | Qt)     x       P(p_ij | c_i)
```



## Input
Input file can be any text file, either as raw text or in CoNLL format.


## Usage
```
$./prob-query-expander arg1 [opt1]")
      arg1: INPUT FILE - text file, either raw or conll
      opt1: FORMAT     - 'conll', 'raw', default = 'conll'
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
Query expansion ranks


_

Authors: *Holger Muth-Hellebrandt, Neele Witte*
