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
        -   [Sentence search](#sentence-search)
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

    ## [1] "Anti-establishment sentiments are a key component of"       
    ## [2] "political opinion and behavior in the United States and are"
    ## [3] "distinct from traditional indicators of political ideology,"
    ## [4] "according to new research. The findings indicate"           
    ## [5] "anti-establishment viewpoints have played a key role in"

### PubMed abstracts

``` r
pmids <- PubmedMTK::pmtk_search_pubmed(search_term = 'political ideology', 
                                       fields = c('TIAB','MH'))
```

    ## [1] "political ideology[TIAB] OR political ideology[MH]: 502 records"

``` r
abstracts <- PubmedMTK::pmtk_get_records2(pmids = pmids$pmid[1:10], 
                                          cores = 3 #, 
                                          #ncbi_key = key
                                          )

strwrap(abstracts[[1]]$abstract, width = 60)[1:10]
```

    ##  [1] "This paper critically comments on the state of affairs in"  
    ##  [2] "the UK relating to the pandemic and explores how a focus on"
    ##  [3] "inequities experienced by marginalized and vulnerable"      
    ##  [4] "groups is necessary for exposing the material realties of"  
    ##  [5] "everyday life, but also how such a focus has been hijacked" 
    ##  [6] "by center right politics to distract us from collective"    
    ##  [7] "responsibilities and building alliances for systemic"       
    ##  [8] "change. The paper critically reviews the impact of the"     
    ##  [9] "COVID-19 pandemic on the most marginalized and vulnerable"  
    ## [10] "in UK society and highlights the interconnected risk"

### Tweets

#### Search-based

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

    ## [1] "Would you ever put political messages in your art, for a"   
    ## [2] "commission or otherwise? — No. At least not right now. I"   
    ## [3] "have an ideology I believe in and I will do work that"      
    ## [4] "promotes what I believe to be right. But I will not do work"
    ## [5] "th… https://t.co/d9nyKolXFw"

#### US Congress

``` r
house_meta <- read.csv(url('https://theunitedstates.io/congress-legislators/legislators-current.csv')) 
  # mutate(district = ifelse(district == 0, 'AL', district),
  #        CD = paste0(state, '-', stringr::str_pad(district, 2, pad = '0')),
  #        twitter = toupper(twitter))

handles <- unique(subset(house_meta, twitter != '')$twitter)
  
congress_tweets <- rtweet::get_timeline( 
  handles, 
  n = 100,
  check = FALSE,
  token = tk) 

congress_tweets$created_at <- as.Date(gsub(' .*$', '', congress_tweets$created_at))
```

## Processing

### Sentence splitting

``` r
abbrevs <- c(corpus::abbreviations_en, 'Gov.', 'Sen.')
abbrevs
```

    ##   [1] "A."       "A.D."     "a.m."     "A.M."     "A.S."     "AA."     
    ##   [7] "AB."      "Abs."     "AD."      "Adj."     "Adv."     "Alt."    
    ##  [13] "Approx."  "Apr."     "Aug."     "B."       "B.V."     "C."      
    ##  [19] "C.F."     "C.O.D."   "Capt."    "Card."    "cf."      "Col."    
    ##  [25] "Comm."    "Conn."    "Cont."    "D."       "D.A."     "D.C."    
    ##  [31] "DC."      "Dec."     "Def."     "Dept."    "Diff."    "Dr."     
    ##  [37] "E."       "e.g."     "E.g."     "E.G."     "Ed."      "Est."    
    ##  [43] "etc."     "Etc."     "Ex."      "exec."    "Exec."    "F."      
    ##  [49] "Feb."     "Fn."      "Fri."     "G."       "Gb."      "H."      
    ##  [55] "Hon.B.A." "Hz."      "I."       "I.D."     "i.e."     "I.e."    
    ##  [61] "I.J."     "I.T."     "Id."      "In."      "J.B."     "J.D."    
    ##  [67] "J.K."     "Jam."     "Jan."     "Job."     "Joe."     "Jr."     
    ##  [73] "Jul."     "Jun."     "K."       "K.R."     "Kb."      "L."      
    ##  [79] "L.A."     "L.P."     "Lev."     "Lib."     "Lt."      "Lt.Cdr." 
    ##  [85] "M."       "M.I.T."   "M.R."     "M.T."     "Maj."     "Mar."    
    ##  [91] "Mart."    "Mb."      "Md."      "Mgr."     "Min."     "Misc."   
    ##  [97] "MM."      "Mr."      "MR."      "Mrs."     "Ms."      "Mt."     
    ## [103] "Mx."      "N."       "N.V."     "N.Y."     "Nov."     "Nr."     
    ## [109] "Num."     "O."       "Oct."     "Op."      "Org."     "P."      
    ## [115] "p.m."     "P.M."     "P.O."     "P.V."     "PC."      "Ph.D."   
    ## [121] "Phys."    "pp."      "PP."      "Prof."    "Pvt."     "Q."      
    ## [127] "R."       "R.L."     "R.T."     "Rep."     "Rev."     "S."      
    ## [133] "S.A."     "S.A.R."   "S.E."     "S.p.A."   "Sep."     "Sept."   
    ## [139] "Sgt."     "Sq."      "St."      "T."       "U."       "U.S."    
    ## [145] "U.S.A."   "U.S.C."   "V."       "Var."     "vs."      "VS."     
    ## [151] "W."       "X."       "Y."       "Yr."      "Z."       "Gov."    
    ## [157] "Sen."

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

| sentence_id | text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|           1 | Anti-establishment sentiments are a key component of political opinion and behavior in the United States and are distinct from traditional indicators of political ideology, according to new research.                                                                                                                                                                                                                                                                                                    |
|           2 | The findings indicate anti-establishment viewpoints have played a key role in some beliefs that came to prominence during the Trump era, such as the QAnon movement.                                                                                                                                                                                                                                                                                                                                       |
|           3 | The research has been published in the American Journal of Political Science and The Forum.                                                                                                                                                                                                                                                                                                                                                                                                                |
|           4 | “I was interested in this project because it increasingly seemed to me that polarization and political identities were increasingly bearing the brunt of the blame –– perhaps erroneously –– for socially undesirable beliefs and actions that were probably the product of other orientations, like conspiracy thinking and a tendency to view politics as a struggle between good and evil,” said co-author Adam M. Enders, an assistant professor of political science at the University of Louisville. |
|           5 | “American politics seems to be different than in previous decades and we wanted to know why,” added co-author Joseph E. Uscinski of the University of Miami.                                                                                                                                                                                                                                                                                                                                               |
|           6 | “Many people blame current political problems — conspiracy theories, fake news, political violence — on polarization.                                                                                                                                                                                                                                                                                                                                                                                      |

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

    ##  [1] "Anti-establishment" "sentiments"         "are"               
    ##  [4] "a"                  "key"                "component"         
    ##  [7] "of"                 "political"          "opinion"           
    ## [10] "and"                "behavior"           "in"                
    ## [13] "the"                "United"             "States"            
    ## [16] "and"                "are"                "distinct"          
    ## [19] "from"               "traditional"        "indicators"        
    ## [22] "of"                 "political"          "ideology"          
    ## [25] ","                  "according"          "to"                
    ## [28] "new"                "research"           "."

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
    ##  1:      1 Anti-establishment           1        1
    ##  2:      1         sentiments           1        2
    ##  3:      1                are           1        3
    ##  4:      1                  a           1        4
    ##  5:      1                key           1        5
    ##  6:      1          component           1        6
    ##  7:      1                 of           1        7
    ##  8:      1          political           1        8
    ##  9:      1            opinion           1        9
    ## 10:      1                and           1       10

### Vocabulary

``` r
vocab <- df[, list(text_freq = .N, 
                          doc_freq = length(unique(doc_id))), 
              by = list(token)]

head(vocab)
```

    ##                 token text_freq doc_freq
    ## 1: Anti-establishment         2        1
    ## 2:         sentiments         3        1
    ## 3:                are        69       10
    ## 4:                  a       199       10
    ## 5:                key         4        3
    ## 6:          component         1        1

## Annotation

``` r
setwd(paste0(udmodel_dir, 'model'))
udmodel <- udpipe::udpipe_load_model('english-ewt-ud-2.3-181115.udpipe')
```

> The `udpipe` package can be used to annotate simple text or token
> objects. One benefit of annotating a token object versus simple text
> is that the user can define token and sentence constitution.

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

| doc_id | sentence_id | token_id | token              | lemma              | upos | xpos |
|:-------|------------:|:---------|:-------------------|:-------------------|:-----|:-----|
| 1      |           1 | 1        | Anti-establishment | Anti-establishment | ADJ  | JJ   |
| 1      |           1 | 2        | sentiments         | sentiment          | NOUN | NNS  |
| 1      |           1 | 3        | are                | be                 | AUX  | VBP  |
| 1      |           1 | 4        | a                  | a                  | DET  | DT   |
| 1      |           1 | 5        | key                | key                | ADJ  | JJ   |
| 1      |           1 | 6        | component          | component          | NOUN | NN   |

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

| keyword           | freq |   pmi |
|:------------------|-----:|------:|
| economic issues   |    8 | 8.107 |
| is based          |    4 | 5.333 |
| the case that     |    4 | 6.497 |
| the University of |    5 | 5.454 |
| Democratic Party  |    3 | 8.688 |
| ability to        |    3 | 5.265 |

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

| keyword                       | pattern | ngram |   n |
|:------------------------------|:--------|------:|----:|
| D.\_Murphy_of_New_Jersey      | NNPNN   |     5 |   1 |
| American_Journal_of_Political | ANPN    |     4 |   2 |
| group_today                   | NN      |     2 |   1 |
| fairer_New_Jersey             | ANN     |     3 |   1 |
| Black_mayor_in_the_city’s     | ANPDN   |     5 |   1 |

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
    ##   ..@ i       : int [1:3568] 0 3 5 6 8 9 0 1 2 3 ...
    ##   ..@ p       : int [1:2288] 0 6 16 20 25 26 36 40 49 56 ...
    ##   ..@ Dim     : int [1:2] 10 2287
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:10] "1" "10" "2" "3" ...
    ##   .. ..$ : chr [1:2287] "-" "," ";" ":" ...
    ##   ..@ x       : num [1:3568] 10 6 5 4 12 1 79 54 30 69 ...
    ##   ..@ factors : list()

## doc2vec

``` r
new_text <- data.table::setDT(annotation0)[, list(text = paste(newness, collapse = " ")), 
                                  by = doc_id]

strwrap(new_text$text[1], width = 60)[1:5]
```

    ## [1] "anti-establishment_sentiments be a"                      
    ## [2] "key_component_of_political_opinion and behavior in the"  
    ## [3] "United States and be distinct from"                      
    ## [4] "traditional_indicators_of_political_ideology , accord to"
    ## [5] "new_research . the finding indicate"

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

    ##    term1              term2 similarity rank
    ## 1   find                not  0.9827810    1
    ## 2   find               must  0.9714175    2
    ## 3   find               also  0.9711717    3
    ## 4   find             Lawson  0.9691734    4
    ## 5   find political_ideology  0.9685147    5
    ## 6   find          fake_news  0.9682022    6
    ## 7   find           research  0.9659647    7
    ## 8   find              those  0.9616702    8
    ## 9   find         psychology  0.9578373    9
    ## 10  find        personality  0.9565152   10

## Text summary via Pagerank

> Here, we use the annotated version of our corpus, and filter PoS to
> nouns and adjectives; however, non-annotated version of corpus could
> be used. Via the `textrank` package.

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

    ## $`Scientists uncover a psychological factor that explains support for QAnon better than political ideology`
    ## [1] "But anti-establishment sentiments were more strongly associated with endorsing these beliefs than political ideology. "                                                                                                                                                                                                                                                
    ## [2] "The researchers also found that support for Donald Trump was positively associated with anti-establishment orientations, but anti-establishment orientations were simultaneously associated with reduced support for both the Republican and Democratic parties, a finding which provided a “critical distinction” about the events at the U.S. Capitol on January 6. "
    ## [3] "“People espousing the most anti-establishment views are attracted to Donald Trump, the outsider, not Donald Trump, the leader of the Republican Party,” the researchers said. "                                                                                                                                                                                        
    ## 
    ## $`Explained: What Is Alt-Right Movement And Which Political Ideology It Follows`
    ## [1] "Rather they are a group of right-wing ideology believing people who reject many liberal ideas that are core to the US political setup. "                                        
    ## [2] "During this time many right-wing ideologues, white supremacists, and many neo-nazis joined the movement.  "                                                                     
    ## [3] "The alt-right however declined after 2017 due to many reasons like the backlash following Unite the Right rally, fractures within the movement, opposition from the US public. "
    ## 
    ## $`Democrats' Big Political Tent Helps Explain DC Stalemate`
    ## [1] "On economic issues, Democrats nationally are about evenly split between those identifying as moderate (43%) versus liberal (41%), with 16% identifying as conservative. "
    ## [2] "Trend in the percentages of Democrats who consider themselves conservative, moderate or liberal on economic issues. "                                                    
    ## [3] "Trend in the percentages of Democrats who consider themselves conservative, moderate or liberal on social issues. "

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

    ## [34716621] emonstrate how these are linked to |political ideology|, policy, and practice. We conclude
    ## [34707531] to spoken statements, and people's |political ideology| has been shown to guide their sent
    ## [34707531] ing pupillometry, we asked whether |political ideology| and disgust sensitivity affect onl
    ## [34694858] ositively linked with conservative |political ideology|. However, such sweeping generaliza
    ## [34694858] explains the interactive effect of |political ideology| and conscientiousness on the shari
    ## [34665062]  a priori moral values informed by |political ideology|. This perspective is particularly 
    ## [34630247]                       The dominant |political ideology| of recent decades, neoliberalism, 
    ## [34605280] ity, positive COVID diagnosis, and |political ideology|. Univariate analysis and logistic 
    ## [34594280] pers, blogs, and social networks), |political ideology|, vote, trust in institutions, and 
    ## [34592973] tion. As in studies of the public, |political ideology| and the observation of local clima
    ## [34580214]  variables and only weakly reflect |political ideology|. Moral cosmopolitanism also differ

### Sentence search

``` r
eg_sentences <- df[, if(any(token %in% c('QAnon'))) .SD, 
                    by = list(doc_id, sentence_id)]

eg_sentences0 <- eg_sentences[, list(text = paste(token, collapse = " ")), 
                                by = list(doc_id,sentence_id)]

eg_sentences0 %>% head() %>% knitr::kable()
```

| doc_id | sentence_id | text                                                                                                                                                                                                                                                                                                                                      |
|:-------|:------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1      | 2           | The findings indicate anti-establishment viewpoints have played a key role in some beliefs that came to prominence during the Trump era , such as the QAnon movement .                                                                                                                                                                    |
| 1      | 26          | Ender and Uscinski’s research published in The Forum , based on a national survey of 1,947 U.S. adults conducted between October 8 and 21 , 2020 , found that anti-establishment orientations were also strongly related to the endorsement of conspiracies related to COVID-19 , QAnon , Donald Trump , and the 2020 election .          |
| 1      | 27          | For example , agreement with statements such as “ Satanic sex traffickers control the government ” ( QAnon ) and “ There is a conspiracy to stop the U.S. Post Office from processing mail-in ballots ” ( election fraud ) were weakly related to political ideology , but strongly related to having an anti-establishment orientation . |
| 8      | 1           | The ” anti-establishment ” ideology is a major contributor to the belief systems that catapulted former President Donald Trump to power and the formation of the QAnon movement , according to findings published by the American Journal of Political Science and The Forum .                                                            |

## Visualizing dependencies

``` r
sentence <- "The green giant wished for Jackie-boy only peace."
sent_depend <- udpipe::udpipe(udmodel, x = sentence)

textplot::textplot_dependencyparser(sent_depend, 
                                    title = sentence, 
                                    subtitle = NULL)
```

![](README_files/figure-markdown_github/unnamed-chunk-28-1.png)

## Appendix
