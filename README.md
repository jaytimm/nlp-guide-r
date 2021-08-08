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
        -   [Sentence extraction](#sentence-extraction)
        -   [Tokenization](#tokenization)
        -   [Tokens to data frame](#tokens-to-data-frame)
        -   [Sentences containing X](#sentences-containing-x)
    -   [Annotation](#annotation)
    -   [Multiword expressions](#multiword-expressions)
        -   [Collocations](#collocations)
        -   [Noun phrases](#noun-phrases)
        -   [Tokenizing multiword
            expressions](#tokenizing-multiword-expressions)
        -   [Annotation to DTM](#annotation-to-dtm)
        -   [Rebuilding text](#rebuilding-text)
    -   [doc2vec](#doc2vec)
    -   [Search in context](#search-in-context)
    -   [Odds](#odds)
        -   [Visualizing dependencies](#visualizing-dependencies)

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

    ## [1] "Julian Zelizer, a CNN political analyst, is a professor of"  
    ## [2] "history and public affairs at Princeton University and"      
    ## [3] "author of the book, \"Burning Down the House: Newt Gingrich,"
    ## [4] "the Fall of a Speaker, and the Rise of the New Republican"   
    ## [5] "Party.\" Follow him on Twitter @julianzelizer. The views"

### PubMed abstracts

``` r
s0 <- PubmedMTK::pmtk_search_pubmed(search_term = 'medical marijuana', 
                                    fields = c('TIAB','MH'))
```

    ## [1] "medical marijuana[TIAB] OR medical marijuana[MH]: 2206 records"

``` r
s1 <- PubmedMTK::pmtk_get_records2(pmids = s0$pmid[1:10], 
                                   cores = 3 #, 
                                   #ncbi_key = key
                                   )

strwrap(s1[[1]]$abstract, width = 60)[1:10]
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
tsearch <- rtweet::search_tweets(q = '#Jan6',
                                 n = 100,
                                 type = "recent",
                                 include_rts = FALSE,
                                 geocode = NULL,
                                 max_id = NULL,
                                 parse = TRUE,
                                 token = NULL)

strwrap(tsearch$text[1], width = 60)
```

    ## [1] "#GymJordan #Jan6 #PoliticalMoron #BulldogPolitics"
    ## [2] "https://t.co/IPNNHxaDv7 https://t.co/TbyyyyaJa3"

Processing
----------

### Sentence extraction

> The `pmtk_toke_sentences` function from the `PumbedMTK` package is a
> simple wrapper to the `corpus::text_split` function.

``` r
sentences <- PubmedMTK::pmtk_toke_sentences(text = news$text,
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
<td style="text-align: left;">Nine months after the election he comprehensively lost, the spectre of Donald Trump – darkly menacing, subversive and apparently immune from prosecution – continues to cast a shadow over US democracy and America’s global standing, distorting policy and poisoning political life.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.2</td>
<td style="text-align: left;">How can this be?</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.3</td>
<td style="text-align: left;">Why is this horror movie still running?</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.4</td>
<td style="text-align: left;">Trumpism, like other fascist variants, is a disease, a blight – a noxious far-right populist-nationalist miasma that taints and rots all it touches.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.5</td>
<td style="text-align: left;">Older Europeans share a folk memory of fascism.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.6</td>
<td style="text-align: left;">But too many Americans just don’t get it.</td>
</tr>
</tbody>
</table>

### Tokenization

> The `text_tokens` function from the `corpus` package is absolutely
> lovely.

``` r
a1 <- corpus::text_tokens(sentences$text,
                          
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

names(a1) <-sentences$doc_id
a1[[1]]
```

    ##  [1] "Nine"            "months"          "after"           "the"            
    ##  [5] "election"        "he"              "comprehensively" "lost"           
    ##  [9] ","               "the"             "spectre"         "of"             
    ## [13] "Donald"          "Trump"           "–"               "darkly"         
    ## [17] "menacing"        ","               "subversive"      "and"            
    ## [21] "apparently"      "immune"          "from"            "prosecution"    
    ## [25] "–"               "continues"       "to"              "cast"           
    ## [29] "a"               "shadow"          "over"            "US"             
    ## [33] "democracy"       "and"             "America's"       "global"         
    ## [37] "standing"        ","               "distorting"      "policy"         
    ## [41] "and"             "poisoning"       "political"       "life"           
    ## [45] "."

### Tokens to data frame

> A simple approach to reshaping token objects.

``` r
a2 <- PubmedMTK::pmtk_cast_tokens(a1)
a2 %>%  slice(1:10)
```

    ##     doc_id sentence_id token_id           token
    ##  1:      1           1        1            Nine
    ##  2:      1           1        2          months
    ##  3:      1           1        3           after
    ##  4:      1           1        4             the
    ##  5:      1           1        5        election
    ##  6:      1           1        6              he
    ##  7:      1           1        7 comprehensively
    ##  8:      1           1        8            lost
    ##  9:      1           1        9               ,
    ## 10:      1           1       10             the

### Sentences containing X

``` r
jrb_sentences <- a2[, if(any(token == 'Biden')) .SD, by = list(doc_id,sentence_id)]
jrb_sentences0 <- jrb_sentences[, list(text = paste(token, collapse = " ")), by = list(doc_id,sentence_id)]

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
<td style="text-align: left;">8</td>
<td style="text-align: left;">By refusing to confront his crooked predecessor and bring him to justice , Joe Biden feeds delusional Trump’s sense of godlike impunity , and the dread prospect of a blasphemous second coming .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">12</td>
<td style="text-align: left;">Maybe Biden lacks the killer instinct .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">36</td>
<td style="text-align: left;">Asked what he would do about Trump’s crimes , Biden said last August that to pursue his predecessor in court would be “ very unusual ” .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">46</td>
<td style="text-align: left;">But still Biden and Garland sit on their hands .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">60</td>
<td style="text-align: left;">Biden struggles daily with the toxic fallout .</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">2</td>
<td style="text-align: left;">( AP ) — After more than six months of work combating the coronavirus , negotiating a bipartisan infrastructure bill and repairing the U.S. image abroad , President Joe Biden should be heading out on vacation and a traditional August break from Washington .</td>
</tr>
</tbody>
</table>

Annotation
----------

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')
```

> The `udpipe` package can be used to annotate token objects; however,
> sentence details are not captured. Below we add a newline to the end
> of every sentence in the corpus, and aggregate the sentence-level
> tokens to document-level. This minor hack works (ie, helps the
> annotator identify sentences), and the resulting annotation contains
> the same number of rows as the table generated by `pmtk_cast_tokens`.
> Ie, tokens in, tokens out.

``` r
## a possible function -- 
a01 <- lapply(a1, c, '\n')
names(a01) <- gsub('\\..*$', '', names(a1))

a3 <- sapply(unique(names(a01)), 
             function(x) unname(unlist(a01[names(a01) == x])), 
             simplify=FALSE)
```

``` r
x0 <- udpipe::udpipe(object = udmodel,
                     x = a3,
                     tagger = 'default', 
                     parser = 'default')

colnames(x0)
```

    ##  [1] "doc_id"        "paragraph_id"  "sentence_id"   "sentence"     
    ##  [5] "start"         "end"           "term_id"       "token_id"     
    ##  [9] "token"         "lemma"         "upos"          "xpos"         
    ## [13] "feats"         "head_token_id" "dep_rel"       "deps"         
    ## [17] "misc"

> The utility of annotating a token object versus simple text is that
> the user specifies what constitutes a token and what constitutes a
> sentence. PoS/dependency annotators are not the best tokenizers or
> sentence extractors. Doing some of the dirty work prior to annotation
> really goes a long way in preserving text structure.

``` r
x0 %>%
  select(doc_id, sentence_id, token_id:xpos) %>%
  head() %>%
  knitr::kable()
```

| doc\_id |  sentence\_id| token\_id | token    | lemma    | upos | xpos |
|:--------|-------------:|:----------|:---------|:---------|:-----|:-----|
| 1       |             1| 1         | Nine     | nine     | NUM  | CD   |
| 1       |             1| 2         | months   | month    | NOUN | NNS  |
| 1       |             1| 3         | after    | after    | ADP  | IN   |
| 1       |             1| 4         | the      | the      | DET  | DT   |
| 1       |             1| 5         | election | election | NOUN | NN   |
| 1       |             1| 6         | he       | he       | PRON | PRP  |

Multiword expressions
---------------------

### Collocations

``` r
collocations <- udpipe::collocation(x = x0,
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

| keyword       |  freq|    pmi|
|:--------------|-----:|------:|
| related to    |     3|  5.192|
| well as       |     3|  5.940|
| Psaki said    |     3|  8.827|
| importance of |     3|  5.567|
| a lot         |     3|  5.090|
| on the stage  |     4|  9.255|

### Noun phrases

``` r
x0$phrase_tag <- udpipe::as_phrasemachine(x0$xpos, 
                                          type = "penn-treebank")

splits <- split(x0, f = x0$doc_id)

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

| keyword                | pattern |  ngram|    n|
|:-----------------------|:--------|------:|----:|
| nuclear\_pact          | AN      |      2|    1|
| back-to-back\_voting   | AN      |      2|    1|
| American\_Rescue\_Plan | NNN     |      3|    1|
| all-time\_low          | AN      |      2|    1|
| Congressional\_Budget  | NN      |      2|    1|

### Tokenizing multiword expressions

> Recode noun phrases identified above as a single token in annotation
> data frame.

``` r
# lex$ngram <- stringr::str_count(lex$TermName,stringr::fixed('_')) + 1
# data.table::setDT(lex)
# ms <- subset(lex, lex$ngram > 1)

x0$newness <- udpipe::txt_recode_ngram(tolower(x0$token),
                                       compound = c(nps1$keyword),
                                       ngram = c(nps1$ngram),
                                       sep = '_')

x0 %>%
  select(doc_id, token:xpos, newness) %>%
  filter(grepl('_', newness)) %>%
  head() %>%
  knitr::kable()
```

| doc\_id | token     | lemma     | upos | xpos | newness                                          |
|:--------|:----------|:----------|:-----|:-----|:-------------------------------------------------|
| 1       | darkly    | darkly    | ADJ  | JJ   | darkly\_menacing                                 |
| 1       | global    | global    | ADJ  | JJ   | global\_standing                                 |
| 1       | political | political | ADJ  | JJ   | political\_life                                  |
| 1       | horror    | horror    | NOUN | NN   | horror\_movie                                    |
| 1       | other     | other     | ADJ  | JJ   | other\_fascist\_variants                         |
| 1       | noxious   | noxious   | ADJ  | JJ   | noxious\_far-right\_populist-nationalist\_miasma |

### Annotation to DTM

> Per the annotation structure above, we can (1) cast into a
> document-term matrix and (2) normalize vocabulary to the lemma in one
> fell swoop.

``` r
x2 <- x0 %>%
  filter(!is.na(newness)) %>%
  mutate(newness = ifelse(grepl('_', newness), newness, lemma)) 

dtm <- x2 %>% 
  count(doc_id, newness) %>%
  tidytext::cast_sparse(row = doc_id,
                        column = newness,
                        value = n)
str(dtm)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:5325] 0 2 3 4 5 9 11 14 15 16 ...
    ##   ..@ p       : int [1:3129] 0 13 30 38 41 47 67 78 96 114 ...
    ##   ..@ Dim     : int [1:2] 20 3128
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:20] "1" "10" "11" "12" ...
    ##   .. ..$ : chr [1:3128] "-" "," ":" "!" ...
    ##   ..@ x       : num [1:5325] 12 18 3 9 1 4 6 11 1 6 ...
    ##   ..@ factors : list()

### Rebuilding text

``` r
new_text <- data.table::setDT(x2)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[5], width = 60)[1:5]
```

    ## [1] "poll of the week : a new Quinnipiac University poll find"  
    ## [2] "that President Joe Biden's approval_rating stand at 46_% ,"
    ## [3] "while he disapproval rating_sits_at_43_% . he"             
    ## [4] "approval_rating in Quinnipiac' previous_poll be 49_% ."    
    ## [5] "What's the point : two week ago , I note that Biden's"

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

    ##    term1       term2 similarity rank
    ## 1  Biden         yet  0.9059902    1
    ## 2  Biden     January  0.8999321    2
    ## 3  Biden     largely  0.8763039    3
    ## 4  Biden Machiavelli  0.8276467    4
    ## 5  Biden      hehave  0.8262905    5
    ## 6  Biden        talk  0.8206981    6
    ## 7  Biden     Garland  0.8023541    7
    ## 8  Biden        it's  0.7978688    8
    ## 9  Biden       today  0.7951390    9
    ## 10 Biden    consider  0.7914523   10

Search in context
-----------------

> Based on the `corpus::text_locate` function.

``` r
egs <- PubmedMTK::pmtk_locate_term(text = a1,
                                   doc_id = names(a1),
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
<td style="text-align: left;">1.8</td>
<td style="text-align: left;">to confront his crooked predecessor and bring him to justice ,</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">feeds delusional Trump’s sense of godlike impunity , and the dread</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.2</td>
<td style="text-align: left;">bipartisan infrastructure bill and repairing the U.S. image abroad , President</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">should be heading out on vacation and a traditional August break</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3.1</td>
<td style="text-align: left;">1,600 people affected by the September 11 attacks are asking President</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">to refrain from coming to Ground Zero to mark the 20th</td>
</tr>
<tr class="even">
<td style="text-align: left;">4.2</td>
<td style="text-align: left;">With rare exceptions ,</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">throughout his presidency has stressed his determination to cooperate with the</td>
</tr>
<tr class="odd">
<td style="text-align: left;">4.96</td>
<td style="text-align: left;">“ Will</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">feel he’s in a good place for reelection when we don’t</td>
</tr>
<tr class="even">
<td style="text-align: left;">8.1</td>
<td style="text-align: left;">long — but it illustrated a crucial lesson in leadership that</td>
<td style="text-align: left;">Joe Biden</td>
<td style="text-align: left;">needs to learn , and soon .</td>
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
