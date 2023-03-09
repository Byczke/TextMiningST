# załadowanie bibliotek
library(tm)

# utworzenie korpusu dokumentów
corpus_dir <- "./Literatura - streszczenia - oryginał"
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "UTF-8",
    "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# wstępne przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplist_file <- "./stopwords_pl.txt"
stoplist <- readLines(stoplist_file, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

# eksport przetworzonego korpusu do plików tekstowych
preprocessed_dir <- "./Literatura - streszczenia - przetworzone"
dir.create(preprocessed_dir)
writeCorpus(corpus, preprocessed_dir)
