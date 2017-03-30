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
Input files in directory can be any kind of text file, either as raw text or in CoNLL format.


## Usage
```
$ ./prob-query-expander arg1 [opt1]
      arg1: CORPUS DIRECTORY	 - directory with text files, either raw or conll
      opt1: FORMAT            - 'conll', 'raw', default = 'conll'
```
Example run:
```
$ ./prob-query-expander /home/corpus_files conll
> prob-query-expander: president of the uni
united states 0.091636546
united states government 0.07686396
united 0.0367568
union of police associations 0.02680128
...
```

## Output
Output are the query expansion suggestions together with the ranking score.


_

Authors: *Holger Muth-Hellebrandt, Neele Witte*
