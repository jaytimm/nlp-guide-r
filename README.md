NLP with R: some notes
======================

A summary of some R-based, NLP workflows – mostly using `udpipe`.

------------------------------------------------------------------------

-   [NLP with R: some notes](#nlp-with-r:-some-notes)
    -   [Quick live text](#quick-live-text)
        -   [Online news articles](#online-news-articles)
        -   [PubMed abstracts](#pubmed-abstracts)
        -   [Tweets](#tweets)
    -   [Processing](#processing)
        -   [Sentence tokenization](#sentence-tokenization)
        -   [Tokenization](#tokenization)
        -   [Tokens to data frame](#tokens-to-data-frame)
        -   [Sentences containing X](#sentences-containing-x)
        -   [Annotation](#annotation)
    -   [Multi-word expressions](#multi-word-expressions)
        -   [Collocations](#collocations)
        -   [Noun phrases](#noun-phrases)
        -   [Tokenizing multi-word
            expressions](#tokenizing-multi-word-expressions)
        -   [Annotation to DTM](#annotation-to-dtm)
        -   [Rebuilding text](#rebuilding-text)
    -   [doc2vec](#doc2vec)
    -   [Search](#search)
        -   [Search in context](#search-in-context)
        -   [More complex patterns](#more-complex-patterns)
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

    ## [1] "August 8, 2021 - President Joe Biden proclaims the week of" 
    ## [2] "August 8 through August 14, 2021, as National Health Center"
    ## [3] "Week.   In 1965, our Nation launched its first community"   
    ## [4] "health centers to improve the lives and well-being of"      
    ## [5] "Americans regardless of their ability to pay.  These health"

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

    ## [1] "@DOJ @CIA @FBI @ My uncle's died in #WWII fighting"      
    ## [2] "#fascism. #RupertMurdoch, as owner of #FauxNews should"  
    ## [3] "loose broadcasting licence &amp; be subpoenaed as his"   
    ## [4] "agency &amp; staff publicly promote ongoing overthrow of"
    ## [5] "our democracy #Jan6 #insurrection."                      
    ## [6] ""                                                        
    ## [7] "https://t.co/Pfoaa7BLbI"

Processing
----------

### Sentence tokenization

``` r
sentences <- PubmedMTK::pmtk_toke_sentences(text = news$text,
                                            doc_id = 1:nrow(news))

sentences %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 96%" />
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
<td style="text-align: left;">Joe Biden declared his third candidacy for president on 25 April 2019 in a three-and-a-half minute video.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.2</td>
<td style="text-align: left;">The format was new, but for Biden relied on an old-fashioned conception of masculinity.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.3</td>
<td style="text-align: left;">He talked about the 12 August 2017 neo-Nazi rally in Charlottesville, Virginia, about which Donald Trump (in)famously said there were “very fine people on both sides”.</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.4</td>
<td style="text-align: left;">The incident provided Biden with a good vs evil story frame, which he entered as a sort of superhero.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.5</td>
<td style="text-align: left;">“At that moment,” Biden intoned, as viewers saw white supremacists marching with torches, “I knew the threat to this nation was unlike any I had seen in my lifetime.”</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.6</td>
<td style="text-align: left;">I wrote at the time that we’re in the battle for the soul of this nation.</td>
</tr>
</tbody>
</table>

### Tokenization

The order here is wrong; we want to identify sentences, then tokenize –

``` r
a1 <- corpus::text_tokens(sentences$text,
                          
  filter = corpus::text_filter(
    map_case = TRUE, 
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

    ##  [1] "joe"              "biden"            "declared"         "his"             
    ##  [5] "third"            "candidacy"        "for"              "president"       
    ##  [9] "on"               "25"               "april"            "2019"            
    ## [13] "in"               "a"                "three-and-a-half" "minute"          
    ## [17] "video"            "."

### Tokens to data frame

``` r
a2 <- PubmedMTK::pmtk_cast_tokens(a1)
a2 %>%  slice(1:10)
```

    ##     doc_id token_id     token
    ##  1:    1.1        1       joe
    ##  2:    1.1        2     biden
    ##  3:    1.1        3  declared
    ##  4:    1.1        4       his
    ##  5:    1.1        5     third
    ##  6:    1.1        6 candidacy
    ##  7:    1.1        7       for
    ##  8:    1.1        8 president
    ##  9:    1.1        9        on
    ## 10:    1.1       10        25

``` r
a4 <- a2 %>%
  separate(doc_id, 
           into = c('doc_id', 'sentence_id'), 
           sep = '[.]')
```

### Sentences containing X

``` r
jrb_sentences <- a2[, if(any(token == 'biden')) .SD, by = list(doc_id)]
jrb_sentences0 <- jrb_sentences[, list(text = paste(token, collapse = " ")), by = doc_id]

jrb_sentences0 %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 3%" />
<col style="width: 96%" />
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
<td style="text-align: left;">joe biden declared his third candidacy for president on 25 april 2019 in a three-and-a-half minute video .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.2</td>
<td style="text-align: left;">the format was new , but for biden relied on an old-fashioned conception of masculinity .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.4</td>
<td style="text-align: left;">the incident provided biden with a good vs evil story frame , which he entered as a sort of superhero .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.5</td>
<td style="text-align: left;">“ at that moment , ” biden intoned , as viewers saw white supremacists marching with torches , “ i knew the threat to this nation was unlike any i had seen in my lifetime . ”</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1.14</td>
<td style="text-align: left;">the charlottesville setting , adjacent to thomas jefferson’s home , monticello , supplied biden with a pretext to quote the declaration of independence .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1.16</td>
<td style="text-align: left;">the “ battle for the soul of america ” narrative frame served biden well .</td>
</tr>
</tbody>
</table>

### Annotation

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')
```

Collapse token list by doc\_id

``` r
names(a1) <- gsub('\\..*$', '', names(a1))
a3 <- sapply(unique(names(a1)), 
             function(x) unname(unlist(a1[names(a1)==x])), 
             simplify=FALSE)
```

``` r
x0 <- udpipe::udpipe(object = udmodel,
                     x = a3,
                     tagger = 'default', 
                     parser = 'none')

colnames(x0)
```

    ##  [1] "doc_id"        "paragraph_id"  "sentence_id"   "sentence"     
    ##  [5] "start"         "end"           "term_id"       "token_id"     
    ##  [9] "token"         "lemma"         "upos"          "xpos"         
    ## [13] "feats"         "head_token_id" "dep_rel"       "deps"         
    ## [17] "misc"

``` r
x1 <- cbind(a4, x0[, c(10:16)])
```

Multi-word expressions
----------------------

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
| this week     |     3|  5.475|
| at the center |     3|  7.119|
| rather than   |     4|  9.329|
| the bay of    |     4|  5.468|
| tell you      |     3|  7.796|
| have been     |    11|  6.387|

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

| keyword                             | pattern |  ngram|    n|
|:------------------------------------|:--------|------:|----:|
| other\_democrats                    | AN      |      2|    2|
| gas\_tax                            | NN      |      2|    1|
| federal\_elections                  | AN      |      2|    1|
| house\_covid-19                     | NN      |      2|    1|
| congressional\_budget\_office\_drew | ANNN    |      4|    1|

### Tokenizing multi-word expressions

``` r
# lex$ngram <- stringr::str_count(lex$TermName,stringr::fixed('_')) + 1
# data.table::setDT(lex)
# ms <- subset(lex, lex$ngram > 1)

x1$newness <- udpipe::txt_recode_ngram(tolower(x1$token),
                                       compound = c(nps1$keyword),
                                       ngram = c(nps1$ngram),
                                       sep = '_')

x1 %>%
  select(doc_id, token:xpos, newness) %>%
  filter(grepl('_', newness)) %>%
  head() %>%
  knitr::kable()
```

| doc\_id | token            | lemma            | upos | xpos | newness                                    |
|:--------|:-----------------|:-----------------|:-----|:-----|:-------------------------------------------|
| 1       | joe              | joe              | NOUN | NN   | joe\_biden                                 |
| 1       | third            | third            | ADJ  | JJ   | third\_candidacy\_for\_president           |
| 1       | three-and-a-half | three-and-a-half | NOUN | NN   | three-and-a-half\_minute\_video            |
| 1       | old-fashioned    | old-fashioned    | ADJ  | JJ   | old-fashioned\_conception\_of\_masculinity |
| 1       | 12               | 12               | NUM  | CD   | 12\_august                                 |
| 1       | donald           | donald           | AUX  | MD   | donald\_trump                              |

### Annotation to DTM

Normalizing to lemma –

``` r
x2 <- x1 %>%
  filter(!is.na(newness)) %>%
  mutate(newness = ifelse(grepl('_', newness), newness, lemma)) 

x3 <- x2 %>%
  count(doc_id, newness)

dtm <- tidytext::cast_sparse(data = x3,
                             row = doc_id,
                             column = newness,
                             value = n)
str(dtm)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:5772] 0 1 3 4 6 7 8 9 10 11 ...
    ##   ..@ p       : int [1:3328] 0 16 33 41 60 75 88 101 103 105 ...
    ##   ..@ Dim     : int [1:2] 19 3327
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:19] "1" "10" "11" "12" ...
    ##   .. ..$ : chr [1:3327] "-" "," ":" "." ...
    ##   ..@ x       : num [1:5772] 5 4 18 2 1 2 4 2 14 8 ...
    ##   ..@ factors : list()

### Rebuilding text

``` r
new_text <- data.table::setDT(x2)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[5], width = 60)[1:10]
```

    ##  [1] "the eviction_moratorium_crisis may have come and go -"      
    ##  [2] "though likely not for long - but it illustrate a"           
    ##  [3] "crucial_lesson_in_leadership that joe_biden to learn , and" 
    ##  [4] "soon . in a free_society , the people at the top owe it to" 
    ##  [5] "the public to admit when they make a mistake . after that ,"
    ##  [6] "they must learn from those mistake , even if that mean"     
    ##  [7] "assume considerable_political_risk . to understand why this"
    ##  [8] "principle be so important , let' look back at the brief but"
    ##  [9] "memorable_presidency_of_john_f. kennedy . upon take office" 
    ## [10] ", kennedy inherit a plan that have be hatch under he"

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
predict(model.d2v, 'biden', 
        type = "nearest",
        which = "word2word")[[1]]
```

    ##    term1      term2 similarity rank
    ## 1  biden     resign  0.9602762    1
    ## 2  biden         he  0.9547604    2
    ## 3  biden      thing  0.9440539    3
    ## 4  biden      speak  0.9431004    4
    ## 5  biden     should  0.9348323    5
    ## 6  biden  statement  0.9331188    6
    ## 7  biden physically  0.9302188    7
    ## 8  biden      march  0.9232423    8
    ## 9  biden    believe  0.9099379    9
    ## 10 biden        ask  0.9068149   10

Search
------

### Search in context

``` r
egs <- PubmedMTK::pmtk_locate_term(text = a1,
                                   doc_id = x0$doc_id,
                                   term = c('joe biden'),
                                   stem = F,
                                   window = 10)
```

    ## Warning in corpus::text_locate(x = text, terms = term, stemmer = stx): renaming
    ## entries with duplicate names

``` r
egs %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 42%" />
<col style="width: 5%" />
<col style="width: 47%" />
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
<td style="text-align: left;">1</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">declared his third candidacy for president on 25 april 2019 in</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">president</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">and first lady jill biden met with members of the 2020</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">1,600 people affected by the september 11 attacks are asking president</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">to refrain from coming to ground zero to mark the 20th</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">long — but it illustrated a crucial lesson in leadership that</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">needs to learn , and soon .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">even though</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">is the oldest person ever elected president — while kennedy was</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">with rare exceptions ,</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">throughout his presidency has stressed his determination to cooperate with the</td>
</tr>
</tbody>
</table>

### More complex patterns

Odds
----

### Visualizing dependencies

``` r
sentence <- "The green giant wishes for Jackie-boy only good things"
sent_depend <- udpipe::udpipe(udmodel, x = sentence)

plot_annotation <- function(x, size = 3){
  
  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  edges <- x[x$head_token_id != 0, c("token_id", 
                                     "head_token_id", 
                                     "dep_rel")]
  edges$label <- edges$dep_rel
  
  g <- igraph::graph_from_data_frame(edges,
                                     vertices = x[, c("token_id",
                                                      "token",
                                                      "lemma",
                                                      "upos",
                                                      "xpos",
                                                      "feats")],
                                     directed = TRUE)
  
  ggraph::ggraph(g, layout = "linear") +
    ggraph::geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                          arrow = grid::arrow(length = unit(4, 'mm'), 
                                              ends = "last", 
                                              type = "closed"),
                          end_cap = ggraph::label_rect("w123"),
                          label_colour = "#55752f", 
                          check_overlap = TRUE, 
                          label_size = size) +
    
    ggraph::geom_node_label(ggplot2::aes(label = token), 
                            col = "steelblue", 
                            size = size, 
                            fontface = "bold") +
    
    ggraph::geom_node_text(ggplot2::aes(label = upos), 
                           nudge_y = -0.35, 
                           size = size) +
    
    ggraph::theme_graph() 
}


plot_annotation(sent_depend, size = 4) +
  labs(title = sentence)
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)
