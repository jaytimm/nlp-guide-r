A summary of some R-based, NLP workflows. I principally use the
`udpipe`, `corpus`, and `text2vec` packages to work with text data in R.

-   [Quick live text](#quick-live-text)
    -   [News article extraction](#news-article-extraction)
    -   [PubMed abstract extraction](#pubmed-abstract-extraction)
    -   [Twiiter](#twiiter)
-   [Processing](#processing)
    -   [Tokenization](#tokenization)
    -   [Sentence tokenization](#sentence-tokenization)
    -   [Annotation](#annotation)
-   [Multi-word expressions](#multi-word-expressions)
    -   [Collocations](#collocations)
    -   [Noun phrases](#noun-phrases)
    -   [Tokenizing multi-word
        expressions](#tokenizing-multi-word-expressions)
    -   [Dictionary-based entity
        recognition](#dictionary-based-entity-recognition)
    -   [Rebuilding text](#rebuilding-text)
-   [Search](#search)
    -   [Search in context](#search-in-context)
    -   [Highlight](#highlight)
    -   [More complex patterns](#more-complex-patterns)
-   [Odds](#odds)
    -   [Visualizing dependencies](#visualizing-dependencies)

## Quick live text

### News article extraction

``` r
library(tidyverse)
meta <- quicknews::qnews_get_newsmeta()
news <- quicknews::qnews_extract_article(url = meta$link,
                                         cores = 7)
```

### PubMed abstract extraction

``` r
s0 <- PubmedMTK::pmtk_search_pubmed(search_term = 'medical marijuana', 
                                    fields = c('TIAB','MH'))

s1 <- PubmedMTK::pmtk_get_records2(pmids = s0$pmid, 
                                   cores = 3 #, 
                                   #ncbi_key = key
                                   )
```

### Twiiter

## Processing

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
```

### Sentence tokenization

### Annotation

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')

x0 <- udpipe::udpipe(object = udmodel,
                     x = a1[1:10],
                     tagger = 'default', #'none'
                     parser = 'none')
```

## Multi-word expressions

### Collocations

``` r
collocations <- udpipe::collocation(x = x0,
                                    term = 'token',
                                    group = c('doc_id'),
                                    ngram_max = 5,
                                    sep = ' ')

collocations0 <- subset(collocations, freq > 5 & pmi > 8 &
                          !grepl('[[:punct:]]', keyword))

collocations0 %>% sample_n(6) %>%
  mutate(pmi = round(pmi, 3)) %>%
  select(keyword, freq, pmi) %>%
  knitr::kable()
```

| keyword            | freq |    pmi |
|:-------------------|-----:|-------:|
| lashkar gah        |    6 | 10.678 |
| united states      |    9 |  8.874 |
| the white house    |   11 |  8.397 |
| the delta variant  |    8 |  9.855 |
| justice department |    7 |  9.064 |
| the united states  |    9 |  8.873 |

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

| keyword                     | pattern | ngram |   n |
|:----------------------------|:--------|------:|----:|
| two_independents_in_support | ANPN    |     4 |   1 |
| federal_employee            | AN      |     2 |   1 |
| personal_crises             | AN      |     2 |   1 |
| saturday_evening            | NN      |     2 |   1 |
| one_kaiser                  | AN      |     2 |   1 |

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
  head() %>%
  knitr::kable()
```

| doc_id | token          | lemma          | upos  | xpos | newness        |
|:-------|:---------------|:---------------|:------|:-----|:---------------|
| 1      | the            | the            | DET   | DT   | the            |
| 1      | administration | administration | NOUN  | NN   | administration |
| 1      | made           | make           | VERB  | VBD  | made           |
| 1      | a              | a              | DET   | DT   | a              |
| 1      | last-ditch     | last-ditch     | NOUN  | NN   | last-ditch     |
| 1      | ,              | ,              | PUNCT | ,    | ,              |

### Dictionary-based entity recognition

### Rebuilding text

## Search

### Search in context

### Highlight

### More complex patterns

## Odds

### Visualizing dependencies
