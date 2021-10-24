# nlp with r: some notes

A summary of some (more upstream) NLP workflows – mostly using the
[udpipe](https://github.com/bnosac/udpipe) and
[corpus](https://github.com/patperry/r-corpus) packages. Mostly notes to
self.

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
        -   [Vocabulary](#vocabulary)
    -   [Annotation](#annotation)
    -   [Multiword expressions](#multiword-expressions)
        -   [Collocations](#collocations)
        -   [Noun phrases](#noun-phrases)
        -   [Tokenizing multiword
            expressions](#tokenizing-multiword-expressions)
    -   [Annotation to DTM](#annotation-to-dtm)
    -   [doc2vec](#doc2vec)
    -   [Text summary via Pagerank](#text-summary-via-pagerank)
    -   [Search](#search)
        -   [Search in context](#search-in-context)
        -   [Sentences containing X](#sentences-containing-x)
    -   [Visualizing dependencies](#visualizing-dependencies)
    -   [Appendix](#appendix)

## Quick live text

### Online news articles

``` r
library(tidyverse)
rss1 <- quicknews::qnews_build_rss(x = 'political ideology')
meta <- quicknews::qnews_strip_rss(rss1) 
news <- quicknews::qnews_extract_article(url = meta$link[1:10], cores = 7)

full <- news %>% left_join(meta)

strwrap(full$text[1], width = 60)[1:5]
```

    ## [1] "The \"anti-establishment\" ideology is a major contributor to"
    ## [2] "the belief systems that catapulted former President Donald"   
    ## [3] "Trump to power and the formation of the QAnon movement,"      
    ## [4] "according to findings published by the American Journal of"   
    ## [5] "Political Science and The Forum. Speaking to PsyPost, Adam"

### PubMed abstracts

``` r
pmids <- PubmedMTK::pmtk_search_pubmed(search_term = 'political ideology', 
                                       fields = c('TIAB','MH'))
```

    ## [1] "political ideology[TIAB] OR political ideology[MH]: 499 records"

``` r
abstracts <- PubmedMTK::pmtk_get_records2(pmids = pmids$pmid[1:10], 
                                          cores = 3 #, 
                                          #ncbi_key = key
                                          )

strwrap(abstracts[[1]]$abstract, width = 60)[1:10]
```

    ##  [1] "This study aimed to assess the correlation between"         
    ##  [2] "political ideologies, government trust, and COVID-19"       
    ##  [3] "vaccine hesitancy in South Korea during the COVID-19"       
    ##  [4] "pandemic. A cross-sectional survey was conducted among"     
    ##  [5] "South Korea's general population and 1000 respondents (aged"
    ##  [6] "18 years and older) were included. We used multivariate"    
    ##  [7] "logistic regression models to identify the factors"         
    ##  [8] "associated with vaccine hesitancy. Respondents who"         
    ##  [9] "self-identified as liberal or held \"no political opinion\""
    ## [10] "had higher rates of vaccine hesitancy than conservative"

### Tweets

``` r
tweets <-  rtweet::search_tweets(q = 'political ideology',
                                 n = 100,
                                 type = "recent",
                                 include_rts = FALSE,
                                 geocode = NULL,
                                 max_id = NULL,
                                 parse = TRUE,
                                 token = NULL)

strwrap(tweets$text[1], width = 60)
```

    ## [1] "The irony of this NOVA resident chiming in: it's okay and"  
    ## [2] "*necessary* for the DC spillovers to completely take over"  
    ## [3] "1/4 of VA, and inject their ideology and totally change the"
    ## [4] "political landscape forever. But this sign… this sign! The" 
    ## [5] "gall! https://t.co/AZSa6Y9yPW"

## Processing

### Sentence splitting

``` r
abbrevs <- c(corpus::abbreviations_en, 'Gov.', 'Sen.')
```

``` r
c0 <- full$text
names(c0) <- full$doc_id

sentences <- corpus::text_split(c0, 
                                filter = corpus::text_filter(
                                  sent_suppress = abbrevs))

sentences$text <- as.character(sentences$text)
sentences$uid <- paste0(sentences$parent, '.', sentences$index)
colnames(sentences)[1:2] <- c('doc_id', 'sentence_id')

sentences %>% select(sentence_id, text) %>% head() %>% knitr::kable()
```

| sentence_id | text                                                                                                                                                                                                                                                                       |
|------------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|           1 | The “anti-establishment” ideology is a major contributor to the belief systems that catapulted former President Donald Trump to power and the formation of the QAnon movement, according to findings published by the American Journal of Political Science and The Forum. |
|           2 | Speaking to PsyPost, Adam M. Enders — an assistant political science professor at the University of Louisville and co-author of the study — discussed how they compiled the information.                                                                                   |
|           3 | “While we discuss primarily historical and theoretical literature arguing that anti-establishment viewpoints are hardly new, no one has been empirically tracking them over time,” Enders explained.                                                                       |
|           4 | “Our study is a first cut at taking this ignored dimension of public opinion more seriously.                                                                                                                                                                               |
|           5 | We need to track anti-establishment orientations over time to better understand how they ebb and flow.                                                                                                                                                                     |
|           6 | We also need to track them across social and political contexts to see what role these ideas play in other countries with different political systems, economic systems, etc.”                                                                                             |

### Tokenization

> The `text_tokens` function from the `corpus` package provides a host
> of options for text tokenization.

``` r
tokens <- corpus::text_tokens(sentences$text,
                          
  filter = corpus::text_filter(
    map_case = FALSE, 
    map_quote = TRUE,
    remove_ignorable = TRUE,
    combine = abbrevs,
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

names(tokens) <-sentences$uid
tokens[[1]]
```

    ##  [1] "The"                "\""                 "anti-establishment"
    ##  [4] "\""                 "ideology"           "is"                
    ##  [7] "a"                  "major"              "contributor"       
    ## [10] "to"                 "the"                "belief"            
    ## [13] "systems"            "that"               "catapulted"        
    ## [16] "former"             "President"          "Donald"            
    ## [19] "Trump"              "to"                 "power"             
    ## [22] "and"                "the"                "formation"         
    ## [25] "of"                 "the"                "QAnon"             
    ## [28] "movement"           ","                  "according"         
    ## [31] "to"                 "findings"           "published"         
    ## [34] "by"                 "the"                "American"          
    ## [37] "Journal"            "of"                 "Political"         
    ## [40] "Science"            "and"                "The"               
    ## [43] "Forum"              "."

### Tokens to data frame

> A simple approach to reshaping token objects. Via the `textshape`
> package.

``` r
df <- textshape::tidy_list(tokens, 
                           id.name = 'doc_id', 
                           content.name = 'token')
  
####
df[, sentence_id := gsub('^.*\\.', '', doc_id)]
df[, doc_id := gsub('\\..*$', '', doc_id)]
df[, token_id := data.table::rowid(doc_id)]

df %>%  slice(1:10)
```

    ##     doc_id              token sentence_id token_id
    ##  1:      1                The           1        1
    ##  2:      1                  "           1        2
    ##  3:      1 anti-establishment           1        3
    ##  4:      1                  "           1        4
    ##  5:      1           ideology           1        5
    ##  6:      1                 is           1        6
    ##  7:      1                  a           1        7
    ##  8:      1              major           1        8
    ##  9:      1        contributor           1        9
    ## 10:      1                 to           1       10

### Vocabulary

``` r
vocab <- df[, list(text_freq = .N, 
                          doc_freq = length(unique(doc_id))), 
              by = list(token)]

head(vocab)
```

    ##                 token text_freq doc_freq
    ## 1:                The        52        7
    ## 2:                  "        59        2
    ## 3: anti-establishment        20        2
    ## 4:           ideology        18        8
    ## 5:                 is        77        9
    ## 6:                  a       165       10

## Annotation

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')
```

> The `udpipe` package can be used to annotate simple text or token
> objects. The utility of annotating a token object versus simple text,
> however, is that the user specifies what constitutes a token and what
> constitutes a sentence.

``` r
tokens1 <- lapply(tokens, c, '\n')
names(tokens1) <- gsub('\\..*$', '', names(tokens1))

tokens2 <- sapply(unique(names(tokens1)), 
       function(z) unname(unlist(tokens1[names(tokens1) == z])), 
       simplify=FALSE)
```

``` r
annotation <- udpipe::udpipe(object = udmodel,
                             x = tokens2,
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

| doc_id | sentence_id | token_id | token              | lemma              | upos  | xpos |
|:-------|------------:|:---------|:-------------------|:-------------------|:------|:-----|
| 1      |           1 | 1        | The                | the                | DET   | DT   |
| 1      |           1 | 2        | ”                  | ”                  | PUNCT | \`\` |
| 1      |           1 | 3        | anti-establishment | anti-establishment | NOUN  | NN   |
| 1      |           1 | 4        | ”                  | ”                  | PUNCT | ’’   |
| 1      |           1 | 5        | ideology           | ideology           | NOUN  | NN   |
| 1      |           1 | 6        | is                 | be                 | AUX   | VBZ  |

## Multiword expressions

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

| keyword                        | freq |    pmi |
|:-------------------------------|-----:|-------:|
| another person will not be     |    3 |  7.906 |
| moved further                  |    3 |  9.825 |
| arrival of President Joe Biden |    3 | 10.366 |
| link on each comment to        |    3 |  5.366 |
| history behind an              |    3 |  8.046 |
| likelihood of                  |    3 |  5.146 |

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

| keyword          | pattern | ngram |   n |
|:-----------------|:--------|------:|----:|
| Doug_Traubel     | NN      |     2 |   1 |
| legal_questions  | AN      |     2 |   1 |
| health_policy    | NN      |     2 |   1 |
| New_York         | NN      |     2 |   1 |
| work_requirement | NN      |     2 |   3 |

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
```

## Annotation to DTM

> Per the annotation structure above, we can (1) cast into a
> document-term matrix and (2) normalize vocabulary to the lemma in one
> fell swoop.

``` r
annotation0 <- annotation %>%
  filter(!is.na(newness)) %>%
  mutate(newness = ifelse(grepl('_', newness), newness, lemma),
         uid = paste0(doc_id, '.', sentence_id)) 

dtm <- annotation0 %>% 
  count(doc_id, newness) %>%
  tidytext::cast_sparse(row = doc_id,
                        column = newness,
                        value = n)
str(dtm)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:3976] 0 1 2 5 7 8 9 0 1 2 ...
    ##   ..@ p       : int [1:2532] 0 7 17 23 28 32 42 51 58 64 ...
    ##   ..@ Dim     : int [1:2] 10 2531
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:10] "1" "10" "2" "3" ...
    ##   .. ..$ : chr [1:2531] "-" "," ";" ":" ...
    ##   ..@ x       : num [1:3976] 20 1 10 1 2 4 1 136 10 79 ...
    ##   ..@ factors : list()

## doc2vec

``` r
new_text <- data.table::setDT(annotation0)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[1], width = 60)[1:5]
```

    ## [1] "the \" anti-establishment \" ideology be a"                 
    ## [2] "major_contributor_to_the_belief system that catapult"       
    ## [3] "former_president Donald Trump to power and the formation of"
    ## [4] "the QAnon movement , accord to findings publish by the"     
    ## [5] "american journal of political_science and the Forum . speak"

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

> doc2vec is a powerful NLP tool because it projects documents and terms
> in the same embedding space.

``` r
predict(model.d2v, 'find', 
        type = "nearest",
        which = "word2word")[[1]]
```

    ##    term1             term2 similarity rank
    ## 1   find work_requirements  0.9820647    1
    ## 2   find         exemption  0.9820389    2
    ## 3   find           require  0.9818855    3
    ## 4   find        researcher  0.9800472    4
    ## 5   find            assist  0.9757625    5
    ## 6   find         colleague  0.9756172    6
    ## 7   find              even  0.9718631    7
    ## 8   find         implement  0.9656573    8
    ## 9   find           content  0.9646798    9
    ## 10  find         physician  0.9637163   10

## Text summary via Pagerank

> Here, we use the annotated version of our corpus, and filter PoS to
> nouns and ajectives; however, non-annotated version of corpus could be
> used.

``` r
sent1 <- sentences[, c('sentence_id', 
                       'text', 
                       'doc_id', 
                       'uid')]

df1 <- subset(annotation0[, c('sentence_id', 
                              'lemma',
                              'upos',
                              'doc_id', 
                              'uid')],
              upos %in% c('PROPN', 'NOUN', 'ADJ'))

tr <- lapply(unique(df1$doc_id), function(x){
  
  ss <- subset(sent1, doc_id == x)
  tm <- subset(df1, doc_id == x)
  if(nrow(tm) < 4 | nrow(ss) < 4) {NA} else{
    textrank::textrank_sentences(data = ss[, c(1:2)], 
                                 terminology = tm[, c(1:2)]) }   })

s <- lapply(tr, 
            summary, 
            n = 3, 
            keep.sentence.order = TRUE)

names(s) <- full$title
s[1:3]
```

    ## $`This psychological factor explains the QAnon movement better than political ideology: scientists`
    ## [1] "As a historian of the Bible in American life, I can attest that such shallow reading in service of political and cultural agendas has long been a fixture of evangelical Christianity. "                                   
    ## [2] "But these evangelicals never developed their approach to understanding the Bible in complete isolation. "                                                                                                                  
    ## [3] "Like they did in the 19th century, evangelicals who refuse to get vaccinated today tend to follow the spiritual leaders who have built followings by baptizing political or cultural propaganda in a sea of Bible verses. "
    ## 
    ## $`Scientists uncover a psychological factor that explains support for QAnon better than political ideology`
    ## [1] "But anti-establishment sentiments were more strongly associated with endorsing these beliefs than political ideology. "                                                                                                                                                                                                                                                
    ## [2] "The researchers also found that support for Donald Trump was positively associated with anti-establishment orientations, but anti-establishment orientations were simultaneously associated with reduced support for both the Republican and Democratic parties, a finding which provided a “critical distinction” about the events at the U.S. Capitol on January 6. "
    ## [3] "“People espousing the most anti-establishment views are attracted to Donald Trump, the outsider, not Donald Trump, the leader of the Republican Party,” the researchers said. "                                                                                                                                                                                        
    ## 
    ## $`Explained: What Is Alt-Right Movement And Which Political Ideology It Follows`
    ## [1] "Rather they are a group of right-wing ideology believing people who reject many liberal ideas that are core to the US political setup. "                                        
    ## [2] "During this time many right-wing ideologues, white supremacists, and many neo-nazis joined the movement.  "                                                                     
    ## [3] "The alt-right however declined after 2017 due to many reasons like the backlash following Unite the Right rally, fractures within the movement, opposition from the US public. "

## Search

### Search in context

``` r
ctrialsgov::ctgov_kwic(term = 'political ideology|party affiliation', 
                       text = abstracts[[1]]$abstract, 
                       names = abstracts[[1]]$pmid, 
                       width = 35,
                       #use_color = T,
                       output = 'cat') #'data.frame'
```

    ## [34665062]  a priori moral values informed by |political ideology|. This perspective is particularly 
    ## [34630247]                       The dominant |political ideology| of recent decades, neoliberalism, 
    ## [34605280] ity, positive COVID diagnosis, and |political ideology|. Univariate analysis and logistic 
    ## [34594280] pers, blogs, and social networks), |political ideology|, vote, trust in institutions, and 
    ## [34592973] tion. As in studies of the public, |political ideology| and the observation of local clima
    ## [34580214]  variables and only weakly reflect |political ideology|. Moral cosmopolitanism also differ
    ## [34545946]  when assessed together, political |party affiliation| (e.g., Republican, Democrat) but n
    ## [34545946] .g., Republican, Democrat) but not |political ideology| (e.g., conservative, liberal) pred
    ## [34545946]  When assessed together, political |party affiliation| but not political ideology signifi
    ## [34545946] olitical party affiliation but not |political ideology| significantly predicted face mask

### Sentences containing X

``` r
eg_sentences <- df[, if(any(token %in% c('QAnon'))) .SD, 
                    by = list(doc_id, sentence_id)]

eg_sentences0 <- eg_sentences[, list(text = paste(token, collapse = " ")), 
                                by = list(doc_id,sentence_id)]

eg_sentences0 %>% head() %>% knitr::kable()
```

| doc_id | sentence_id | text                                                                                                                                                                                                                                                                                                                                      |
|:-------|:------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1      | 1           | The ” anti-establishment ” ideology is a major contributor to the belief systems that catapulted former President Donald Trump to power and the formation of the QAnon movement , according to findings published by the American Journal of Political Science and The Forum .                                                            |
| 2      | 2           | The findings indicate anti-establishment viewpoints have played a key role in some beliefs that came to prominence during the Trump era , such as the QAnon movement .                                                                                                                                                                    |
| 2      | 26          | Ender and Uscinski’s research published in The Forum , based on a national survey of 1,947 U.S. adults conducted between October 8 and 21 , 2020 , found that anti-establishment orientations were also strongly related to the endorsement of conspiracies related to COVID-19 , QAnon , Donald Trump , and the 2020 election .          |
| 2      | 27          | For example , agreement with statements such as “ Satanic sex traffickers control the government ” ( QAnon ) and “ There is a conspiracy to stop the U.S. Post Office from processing mail-in ballots ” ( election fraud ) were weakly related to political ideology , but strongly related to having an anti-establishment orientation . |

## Visualizing dependencies

``` r
sentence <- "The green giant wished for Jackie-boy only peace."
sent_depend <- udpipe::udpipe(udmodel, x = sentence)

textplot::textplot_dependencyparser(sent_depend, 
                                    title = sentence, 
                                    subtitle = NULL)
```

![](README_files/figure-markdown_github/unnamed-chunk-25-1.png)

## Appendix
