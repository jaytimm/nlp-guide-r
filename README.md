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
        -   [Tokenization](#tokenization)
        -   [Sentence tokenization](#sentence-tokenization)
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

    ## [1] "(CNN)Want to see President Joe Biden in person? Consider a"
    ## [2] "move to Pennsylvania."                                     
    ## [3] NA                                                          
    ## [4] NA                                                          
    ## [5] NA

### PubMed abstracts

``` r
s0 <- PubmedMTK::pmtk_search_pubmed(search_term = 'medical marijuana', 
                                    fields = c('TIAB','MH'))
```

    ## [1] "medical marijuana[TIAB] OR medical marijuana[MH]: 2196 records"

``` r
s1 <- PubmedMTK::pmtk_get_records2(pmids = s0$pmid[1:10], 
                                   cores = 3 #, 
                                   #ncbi_key = key
                                   )

strwrap(s1[[1]]$abstract, width = 60)[1:10]
```

    ##  [1] "Multiple states have passed legislation permitting"        
    ##  [2] "marijuana use. The impact of legalization on trends in"    
    ##  [3] "hospital encounters for marijuana exposures in young"      
    ##  [4] "children across states remains unknown. We aimed to"       
    ##  [5] "describe trends in marijuana-related hospital encounters"  
    ##  [6] "over time in children <6 years and assess the association" 
    ##  [7] "of state-level marijuana legislation with the rate of"     
    ##  [8] "marijuana-related hospitalizations. We identified"         
    ##  [9] "inpatient, emergency department and observation encounters"
    ## [10] "for children <6 years with marijuana exposures (defined by"

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

    ## [1] "@SpeakerPelosi and @RepMaxineWaters please encourage the"
    ## [2] "subpoena of TFG and all his cronies about #Jan6"

Processing
----------

### Tokenization

``` r
a1 <- corpus::text_tokens(news$text,
                          
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

names(a1) <- 1:nrow(news)
a1[[1]][1:20]
```

    ##  [1] "("          "cnn"        ")"          "with"       "votes"     
    ##  [6] "in"         "the"        "senate"     "to"         "advance"   
    ## [11] "his"        "bipartisan" "compromise" "last"       "week"      
    ## [16] ","          "president"  "joe"        "biden"      "took"

### Sentence tokenization

``` r
sentences <- PubmedMTK::pmtk_toke_sentences(text = news$text,
                                            doc_id = 1:nrow(news))

sentences %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 1%" />
<col style="width: 98%" />
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
<td style="text-align: left;">(CNN)With votes in the Senate to advance his bipartisan compromise last week, President Joe Biden took a big step toward upgrading America’s infrastructure.</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.1</td>
<td style="text-align: left;">In a city of ambitious influencers, a shadow cabinet hopes it can summon a new New Deal.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2.2</td>
<td style="text-align: left;">Ruby Cramer is a senior staff writer at POLITICO and POLITICO Magazine.</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.3</td>
<td style="text-align: left;">One recent Wednesday evening, a small of group of concerned citizens gathered on a Zoom call to talk about how to get the attention of the president.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2.4</td>
<td style="text-align: left;">At 6 p.m., two rows of elderly faces appeared on screen, staring into the camera: June Hopkins, Henry Scott Wallace, Tomlin Perkins Coggeshall and James Roosevelt Jr. If their names sound vaguely familiar it’s because their relatives—Harry Hopkins, Henry Wallace, Frances Perkins and Franklin Delano Roosevelt—formed the nucleus of one of the most famous and influential Oval Office rosters in American history.</td>
</tr>
<tr class="even">
<td style="text-align: left;">2.5</td>
<td style="text-align: left;">Ninety years later, these descendants of the FDR administration have reconstituted his Cabinet.</td>
</tr>
</tbody>
</table>

### Annotation

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')

x0 <- udpipe::udpipe(object = udmodel,
                     x = a1,
                     tagger = 'default', 
                     parser = 'none')
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

| keyword               |  freq|     pmi|
|:----------------------|-----:|-------:|
| so far                |     3|   8.413|
| economic recovery     |     3|  11.220|
| american rescue       |     3|   9.220|
| their own             |    11|   7.380|
| the most powerful man |     3|  11.216|
| in loudon             |     3|   5.040|

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

| keyword                        | pattern |  ngram|    n|
|:-------------------------------|:--------|------:|----:|
| imminent\_ending\_of\_the\_cdc | ANPDN   |      5|    1|
| new\_castle\_county\_council   | ANNN    |      4|    1|
| pro-crime\_voters              | AN      |      2|    1|
| oval\_office\_rosters          | ANN     |      3|    1|
| direct\_contact\_with\_biden   | ANPN    |      4|    1|

### Tokenizing multi-word expressions

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

| doc\_id | token      | lemma      | upos | xpos | newness                            |
|:--------|:-----------|:-----------|:-----|:-----|:-----------------------------------|
| 1       | bipartisan | bipartisan | ADJ  | JJ   | bipartisan\_compromise\_last\_week |
| 1       | president  | president  | NOUN | NN   | president\_joe\_biden              |
| 1       | big        | big        | ADJ  | JJ   | big\_step\_toward\_upgrading       |
| 2       | ambitious  | ambitious  | ADJ  | JJ   | ambitious\_influencers             |
| 2       | shadow     | shadow     | NOUN | NN   | shadow\_cabinet                    |
| 2       | new        | new        | ADJ  | JJ   | new\_new\_deal                     |

### Annotation to DTM

Normalizing to lemma –

``` r
x1 <- x0 %>%
  filter(!is.na(newness)) %>%
  mutate(newness = ifelse(grepl('_', newness), newness, lemma)) 

x2 <- x1 %>%
  count(doc_id, newness)

dtm <- tidytext::cast_sparse(data = x2,
                             row = doc_id,
                             column = newness,
                             value = n)
str(dtm)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:5093] 0 2 3 4 5 6 8 9 10 11 ...
    ##   ..@ p       : int [1:2981] 0 17 37 53 69 87 94 97 98 99 ...
    ##   ..@ Dim     : int [1:2] 20 2980
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:20] "1" "10" "11" "12" ...
    ##   .. ..$ : chr [1:2980] "," "." "(" ")" ...
    ##   ..@ x       : num [1:5093] 1 55 5 5 1 42 72 39 44 131 ...
    ##   ..@ factors : list()

### Rebuilding text

``` r
new_text <- data.table::setDT(x1)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[1], width = 60)[1:10]
```

    ##  [1] "( cnn ) with vote in the senate to advance he"             
    ##  [2] "bipartisan_compromise_last_week , president_joe_biden take"
    ##  [3] "a big_step_toward_upgrading america' infrastructure ."     
    ##  [4] NA                                                          
    ##  [5] NA                                                          
    ##  [6] NA                                                          
    ##  [7] NA                                                          
    ##  [8] NA                                                          
    ##  [9] NA                                                          
    ## [10] NA

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
                                    min_count = 5, 
                                    lr = 0.05, 
                                    threads = 1)

embedding.words <- as.matrix(model.d2v, which = "words")
embedding.docs <- as.matrix(model.d2v,   which = "docs")

both <- do.call(rbind, list(embedding.docs, embedding.words))
```

Search
------

### Search in context

``` r
egs <- PubmedMTK::pmtk_locate_term(text = a1,
                                   doc_id = x0$doc_id,
                                   term = c('joe biden'),
                                   stem = F,
                                   window = 10)

egs %>% head() %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 45%" />
<col style="width: 6%" />
<col style="width: 43%" />
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
<td style="text-align: left;">the senate to advance his bipartisan compromise last week , president</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">took a big step toward upgrading america’s infrastructure .</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">legislative agenda as transformational as the new deal . they want</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">to embrace the idea of an “ activist ” government .</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">new york ( ap ) —</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">wagered his campaign and now his presidency on the premise that</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">how bad are things for</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">? so bad that even the new york times is getting</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">. among independents , the downdraft hit 26 points . as</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">might say , “ gee , what happened ? ” if</td>
</tr>
<tr class="even">
<td style="text-align: left;">1</td>
<td style="text-align: left;">washington ( ap ) — for president</td>
<td style="text-align: left;">joe biden</td>
<td style="text-align: left;">and the senators laboring over a nearly $ 1 trillion infrastructure</td>
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

![](README_files/figure-markdown_github/unnamed-chunk-16-1.png)
