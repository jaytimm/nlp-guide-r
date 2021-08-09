nlp with r: some notes
======================

A summary of some (more upstream) NLP workflows – mostly using the
[udpipe](https://github.com/bnosac/udpipe) and
[corpus](https://github.com/patperry/r-corpus) packages.

------------------------------------------------------------------------

-   [nlp with r: some notes](#nlp-with-r:-some-notes)
    -   [Quick live text](#quick-live-text)
        -   [Online news articles](#online-news-articles)
        -   [PubMed abstracts](#pubmed-abstracts)
        -   [Tweets](#tweets)
    -   [Processing](#processing)
        -   [Sentence splitting](#sentence-splitting)
        -   [Tokenization](#tokenization)
        -   [Tokens to data frame](#tokens-to-data-frame)
    -   [Annotation](#annotation)
    -   [Multiword expressions](#multiword-expressions)
        -   [Collocations](#collocations)
        -   [Noun phrases](#noun-phrases)
        -   [Tokenizing multiword
            expressions](#tokenizing-multiword-expressions)
        -   [Annotation to DTM](#annotation-to-dtm)
        -   [Rebuilding text](#rebuilding-text)
    -   [doc2vec](#doc2vec)
    -   [Search](#search)
        -   [Search in context](#search-in-context)
        -   [Sentences containing X](#sentences-containing-x)
    -   [Odds](#odds)
        -   [Visualizing dependencies](#visualizing-dependencies)
    -   [Summary](#summary)

Quick live text
---------------

### Online news articles

``` r
library(tidyverse)
meta <- quicknews::qnews_get_newsmeta('joe biden')
news <- quicknews::qnews_extract_article(url = meta$link[1:20],
                                         cores = 7)

strwrap(news$text[10], width = 60)[1:5]
```

    ## [1] "Remember moderate Democrat Joe Biden’s lackluster 2020"     
    ## [2] "Campaign, which willingly reduced public appearances and"   
    ## [3] "probing press questions, citing COVID concerns? With little"
    ## [4] "opportunity to assess aging Biden, did we really know his"  
    ## [5] "intentions? Did it matter? Was he actually the likeable"

### PubMed abstracts

``` r
pmids <- PubmedMTK::pmtk_search_pubmed(search_term = 'medical marijuana', 
                                       fields = c('TIAB','MH'))
```

    ## [1] "medical marijuana[TIAB] OR medical marijuana[MH]: 2206 records"

``` r
abstracts <- PubmedMTK::pmtk_get_records2(pmids = pmids$pmid[1:10], 
                                          cores = 3 #, 
                                          #ncbi_key = key
                                          )

strwrap(abstracts[[1]]$abstract, width = 60)[1:10]
```

    ##  [1] "People living with HIV (PLWH) experience higher rates of"   
    ##  [2] "comorbid chronic pain conditions compared to the general"   
    ##  [3] "population. Managing HIV and chronic pain, two stigmatized" 
    ##  [4] "health conditions, can exacerbate physical and"             
    ##  [5] "psychological suffering. The current qualitative study was" 
    ##  [6] "designed to increase our understanding of the experience of"
    ##  [7] "living with HIV and chronic pain. Twenty participants were" 
    ##  [8] "recruited from a hospital-based immunology center to"       
    ##  [9] "participate in individual in-depth qualitative interviews." 
    ## [10] "The interviews focused on the experience of living with (or"

### Tweets

``` r
tweets <-  rtweet::search_tweets(q = '#Jan6',
                                 n = 100,
                                 type = "recent",
                                 include_rts = FALSE,
                                 geocode = NULL,
                                 max_id = NULL,
                                 parse = TRUE,
                                 token = NULL)

strwrap(tweets$text[1], width = 60)
```

    ## [1] "@AssemblyRoomUSA @MasterOfPsy @JoeBiden @humanrights1st A"  
    ## [2] "lame attempt to distract. You know you’ve been caught, Jim."
    ## [3] "@Jim_Jordan #Jan6 will haunt you forever. The @FBI is"      
    ## [4] "lurking. #Lackey @RepSwalwell @RepAdamSchiff @PostOpinions" 
    ## [5] "@nytopinion @SpeakerPelosi @DevinNunes — you too Devin."    
    ## [6] "Only a matter of time. @JKSuicideWatch @maddow"             
    ## [7] "https://t.co/Qv3qpYGbVF https://t.co/tbCOLW29xb"

Processing
----------

### Sentence splitting

> The `pmtk_split_sentences` function from the `PumbedMTK` package is a
> simple wrapper to the `corpus::text_split` function. The function is
> mindful of stops used in titles/honorifics (eg, Mr., Dr., Ms., etc.)
> and common acronyms (eg, U.S.A.) when delineating sentences.

``` r
sentences <- PubmedMTK::pmtk_split_sentences(text = news$text,
                                             doc_id = 1:nrow(news))

sentences %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 97%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">doc_id</th>
<th style="text-align: left;">text</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1.1</td>
<td style="text-align: left;">WILMINGTON, Del.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.2</td>
<td style="text-align: left;">(AP) — After more than six months of work combating the coronavirus, negotiating a bipartisan infrastructure bill and repairing the U.S. image abroad, President Joe Biden should be heading out on vacation and a traditional August break from Washington.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.3</td>
<td style="text-align: left;">But with legislative work on the infrastructure bill keeping the Senate in session for a second straight weekend, and likely through next week, Biden hasn’t gone far — just home to Wilmington, Delaware, as he has done most weekends since taking office.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.4</td>
<td style="text-align: left;">“Every president is always working no matter where they are,” White House press secretary Jen Psaki said, explaining that presidents can’t ever really tune out.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.5</td>
<td style="text-align: left;">Biden will spend some of next week at the White House before he decamps again, either for Delaware — he also owns a home in Rehoboth Beach — or Camp David, the official presidential retreat in Maryland’s Catoctin Mountains, Psaki said.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.6</td>
<td style="text-align: left;">The modern president is never completely free from work, tethered by secure telephone lines and other technology with a coterie of top aides and advisers always close by.</td>
</tr>
</tbody>
</table>

### Tokenization

> The `text_tokens` function from the `corpus` package provides a host
> of options for text tokenization.

``` r
tokens <- corpus::text_tokens(sentences$text,
                          
  filter = corpus::text_filter(
    map_case = FALSE, 
    map_quote = TRUE,
    remove_ignorable = TRUE,
    combine = c(corpus::abbreviations_en),
    stemmer = NULL,
    stem_dropped = FALSE,
    stem_except = NULL,
    drop_letter = FALSE,
    drop_number = FALSE,
    drop_punct = FALSE,
    drop_symbol = FALSE,
    drop = NULL,
    drop_except = NULL,
    connector = '_',
    sent_crlf = FALSE)
  )

names(tokens) <-sentences$doc_id
tokens[[1]]
```

    ## [1] "WILMINGTON" ","          "Del"        "."

### Tokens to data frame

> A simple approach to reshaping token objects.

``` r
tokens_df <- PubmedMTK::pmtk_cast_tokens(tokens)
tokens_df %>%  slice(1:10)
```

    ##     doc_id sentence_id token_id      token
    ##  1:      1           1        1 WILMINGTON
    ##  2:      1           1        2          ,
    ##  3:      1           1        3        Del
    ##  4:      1           1        4          .
    ##  5:      1           2        1          (
    ##  6:      1           2        2         AP
    ##  7:      1           2        3          )
    ##  8:      1           2        4          —
    ##  9:      1           2        5      After
    ## 10:      1           2        6       more

Annotation
----------

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')
```

> The `udpipe` package can be used to annotate simple text or token
> objects. The utility of annotating a token object versus simple text,
> however, is that the user specifies what constitutes a token and what
> constitutes a sentence.

> One issue with token objects is that sentence info is less obvious to
> annotators. The `pmtk_rebuild_sentences` function hacks around this by
> adding a newline character to the end of every tokenized sentence in
> the corpus, and aggregating the sentence-level tokens to
> document-level.

``` r
tokens1 <- PubmedMTK::pmtk_rebuild_sentences(x = tokens,
                                             sentence_id = names(tokens))
```

``` r
annotation <- udpipe::udpipe(object = udmodel,
                             x = tokens1,
                             tagger = 'default', 
                             parser = 'default')

colnames(annotation)
```

    ##  [1] "doc_id"        "paragraph_id"  "sentence_id"   "sentence"     
    ##  [5] "start"         "end"           "term_id"       "token_id"     
    ##  [9] "token"         "lemma"         "upos"          "xpos"         
    ## [13] "feats"         "head_token_id" "dep_rel"       "deps"         
    ## [17] "misc"

``` r
annotation %>%
  select(doc_id, sentence_id, token_id:xpos) %>%
  head() %>%
  knitr::kable()
```

| doc\_id |  sentence\_id| token\_id | token      | lemma      | upos  | xpos  |
|:--------|-------------:|:----------|:-----------|:-----------|:------|:------|
| 1       |             1| 1         | WILMINGTON | WILMINGTON | PROPN | NNP   |
| 1       |             1| 2         | ,          | ,          | PUNCT | ,     |
| 1       |             1| 3         | Del        | del        | PROPN | NNP   |
| 1       |             1| 4         | .          | .          | PUNCT | .     |
| 1       |             2| 1         | (          | (          | PUNCT | -LRB- |
| 1       |             2| 2         | AP         | ap         | NOUN  | NN    |

Multiword expressions
---------------------

### Collocations

``` r
collocations <- udpipe::collocation(x = annotation,
                                    term = 'token',
                                    group = c('doc_id'),
                                    ngram_max = 5,
                                    sep = ' ')

collocations0 <- subset(collocations, freq > 1 & pmi > 5 &
                          !grepl('[[:punct:]]', keyword))

collocations0 %>% 
  sample_n(6) %>%
  mutate(pmi = round(pmi, 3)) %>%
  select(keyword, freq, pmi) %>%
  knitr::kable()
```

| keyword      |  freq|     pmi|
|:-------------|-----:|-------:|
| in late July |     3|  10.622|
| he wants     |     3|   6.796|
| get out      |     4|   6.601|
| red states   |     5|   9.302|
| might not    |     4|   7.252|
| if you       |     5|   7.512|

### Noun phrases

``` r
annotation$phrase_tag <- udpipe::as_phrasemachine(annotation$xpos, 
                                                  type = "penn-treebank")

splits <- split(annotation, f = annotation$doc_id)

## lapply to preserve doc_id info
nps <- lapply(1:length(splits), function(x) {
  udpipe::keywords_phrases(x = splits[[x]]$phrase_tag,
                           term = splits[[x]]$token,
                           pattern = "(A|N)+N(P+D*(A|N)*N)*",
                           is_regex = TRUE,
                           ngram_max = 5,
                           detailed = TRUE,
                           sep = '_') })

names(nps) <- names(splits)
nps1 <- data.table::rbindlist(nps, idcol = 'doc_id')

nps1 %>%
  count(keyword, pattern, ngram) %>%
  sample_n(5) %>%
  knitr::kable()
```

| keyword            | pattern |  ngram|    n|
|:-------------------|:--------|------:|----:|
| One\_bill          | AN      |      2|    1|
| approval\_ratings  | NN      |      2|    2|
| jobs\_numbers      | NN      |      2|    1|
| Senate\_filibuster | NN      |      2|    1|
| many\_party        | AN      |      2|    1|

### Tokenizing multiword expressions

> Recode noun phrases identified above as a single token in annotation
> data frame.

``` r
# lex$ngram <- stringr::str_count(lex$TermName,stringr::fixed('_')) + 1
# data.table::setDT(lex)
# ms <- subset(lex, lex$ngram > 1)

annotation$newness <- udpipe::txt_recode_ngram(tolower(annotation$token),
                                               compound = c(nps1$keyword),
                                               ngram = c(nps1$ngram),
                                               sep = '_')

annotation %>%
  select(doc_id, token:xpos, newness) %>%
  filter(grepl('_', newness)) %>%
  head() %>%
  knitr::kable()
```

| doc\_id | token       | lemma       | upos | xpos | newness                                    |
|:--------|:------------|:------------|:-----|:-----|:-------------------------------------------|
| 1       | six         | six         | NUM  | CD   | six\_months\_of\_work                      |
| 1       | bipartisan  | bipartisan  | ADJ  | JJ   | bipartisan\_infrastructure\_bill           |
| 1       | legislative | legislative | ADJ  | JJ   | legislative\_work\_on\_the\_infrastructure |
| 1       | second      | second      | ADJ  | JJ   | second\_straight\_weekend                  |
| 1       | next        | next        | ADJ  | JJ   | next\_week                                 |
| 1       | most        | most        | ADJ  | JJS  | most\_weekends                             |

### Annotation to DTM

> Per the annotation structure above, we can (1) cast into a
> document-term matrix and (2) normalize vocabulary to the lemma in one
> fell swoop.

``` r
annotation0 <- annotation %>%
  filter(!is.na(newness)) %>%
  mutate(newness = ifelse(grepl('_', newness), newness, lemma)) 

dtm <- annotation0 %>% 
  count(doc_id, newness) %>%
  tidytext::cast_sparse(row = doc_id,
                        column = newness,
                        value = n)
str(dtm)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:5879] 0 1 2 6 7 8 9 10 12 13 ...
    ##   ..@ p       : int [1:3399] 0 15 34 54 67 85 103 106 110 111 ...
    ##   ..@ Dim     : int [1:2] 20 3398
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:20] "1" "10" "11" "12" ...
    ##   .. ..$ : chr [1:3398] "-" "," "." "\"" ...
    ##   ..@ x       : num [1:5879] 6 2 10 4 2 9 1 6 2 5 ...
    ##   ..@ factors : list()

### Rebuilding text

``` r
new_text <- data.table::setDT(annotation0)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[5], width = 60)[1:5]
```

    ## [1] "Jenna Ellis , a former_senior_legal_advisor to Donald Trump"
    ## [2] "during he White House tenure , have stand by she suggestion"
    ## [3] "that President Joe Biden should be impeach . speak on"      
    ## [4] "Newsmax , to which she be a regular_contributor , Ellis"    
    ## [5] "criticize Biden's action in regard to immigration at the"

doc2vec
-------

``` r
new_text$nwords <- tokenizers::count_words(new_text$text)
new_text0 <- subset(new_text, nwords < 1000 & nchar(text) > 0)

set.seed(9)
model.d2v <- doc2vec::paragraph2vec(x = new_text0, 
                                    type = "PV-DM", 
                                    dim = 100, 
                                    iter = 20,
                                    min_count = 2, 
                                    lr = 0.05, 
                                    threads = 1)

embedding.words <- as.matrix(model.d2v, which = "words")
embedding.docs <- as.matrix(model.d2v,   which = "docs")

both <- do.call(rbind, list(embedding.docs, embedding.words))
```

``` r
predict(model.d2v, 'Biden', 
        type = "nearest",
        which = "word2word")[[1]]
```

    ##    term1           term2 similarity rank
    ## 1  Biden           upend  0.9595785    1
    ## 2  Biden approval_rating  0.9374418    2
    ## 3  Biden          decide  0.9346738    3
    ## 4  Biden            Jill  0.9337398    4
    ## 5  Biden            poll  0.9299915    5
    ## 6  Biden            come  0.9280250    6
    ## 7  Biden            send  0.9277644    7
    ## 8  Biden            mean  0.9272794    8
    ## 9  Biden           stand  0.9263697    9
    ## 10 Biden             pal  0.9262639   10

Search
------

### Search in context

> Based on the `corpus::text_locate` function.

``` r
egs <- PubmedMTK::pmtk_locate_term(text = tokens,
                                   doc_id = names(tokens),
                                   term = c('joe biden'),
                                   stem = F,
                                   window = 10)

egs %>% head() %>% knitr::kable()
```

<table style="width:100%;">
<colgroup>
<col style="width: 4%" />
<col style="width: 45%" />
<col style="width: 5%" />
<col style="width: 45%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">doc_id</th>
<th style="text-align: left;">lhs</th>
<th style="text-align: left;">instance</th>
<th style="text-align: left;">rhs</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1.2</td>
<td style="text-align: left;">bipartisan infrastructure bill and repairing the U.S. image abroad , President</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">should be heading out on vacation and a traditional August break</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.1</td>
<td style="text-align: left;">1,600 people affected by the September 11 attacks are asking President</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">to refrain from coming to Ground Zero to mark the 20th</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3.1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">declared his third candidacy for president on 25 April 2019 in</td>
</tr>
<tr class="even">
<td style="text-align: left;">4.1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">was first in line to celebrate his pal Barack Obama’s birthday</td>
</tr>
<tr class="odd">
<td style="text-align: left;">5.1</td>
<td style="text-align: left;">White House tenure , has stood by her suggestion that President</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">should be impeached .</td>
</tr>
<tr class="even">
<td style="text-align: left;">6.2</td>
<td style="text-align: left;">With rare exceptions ,</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">throughout his presidency has stressed his determination to cooperate with the</td>
</tr>
</tbody>
</table>

### Sentences containing X

``` r
jrb_sentences <- tokens_df[, if(any(token == 'Biden')) .SD, 
                    by = list(doc_id,sentence_id)]

jrb_sentences0 <- jrb_sentences[, list(text = paste(token, collapse = " ")), 
                                by = list(doc_id,sentence_id)]

jrb_sentences0 %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 4%" />
<col style="width: 93%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">doc_id</th>
<th style="text-align: left;">sentence_id</th>
<th style="text-align: left;">text</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">( AP ) — After more than six months of work combating the coronavirus , negotiating a bipartisan infrastructure bill and repairing the U.S. image abroad , President Joe Biden should be heading out on vacation and a traditional August break from Washington .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">3</td>
<td style="text-align: left;">But with legislative work on the infrastructure bill keeping the Senate in session for a second straight weekend , and likely through next week , Biden hasn’t gone far — just home to Wilmington , Delaware , as he has done most weekends since taking office .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">5</td>
<td style="text-align: left;">Biden will spend some of next week at the White House before he decamps again , either for Delaware — he also owns a home in Rehoboth Beach — or Camp David , the official presidential retreat in Maryland’s Catoctin Mountains , Psaki said .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">7</td>
<td style="text-align: left;">Like his predecessors , Biden travels with a large entourage of aides , Secret Service agents and journalists in an unmistakable motorcade of more than a dozen dark vehicles .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">13</td>
<td style="text-align: left;">Biden and his aides are likely to discuss a range of issues , including getting the $ 1 trillion infrastructure bill through the Senate , strategizing next steps to counter surging coronavirus infections and eyeing the Aug. 31 deadline for the U.S. pullout from Afghanistan .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">27</td>
<td style="text-align: left;">During weekends at home in Wilmington , Biden has ventured out to play golf , attend Mass and head to his sister’s Pennsylvania home for family dinner .</td>
</tr>
</tbody>
</table>

Odds
----

### Visualizing dependencies

``` r
sentence <- "The green giant wishes for Jackie-boy only good things"
sent_depend <- udpipe::udpipe(udmodel, x = sentence)

textplot::textplot_dependencyparser(sent_depend, 
                                    title = sentence, 
                                    subtitle = NULL)
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

Summary
-------
