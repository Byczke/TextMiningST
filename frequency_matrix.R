library(tm)

# utworzenie korpusu dokumentów
corpus_dir <- "./Literatura - streszczenia - przetworzone"
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
cut_extensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$", 
    "", 
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cut_extensions)

# utworzenie macierzzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
dtm_tf_all <- DocumentTermMatrix(corpus)

tdm_tfidf_all <- TermDocumentMatrix(
  corpus,
  control = list(
    weigthing = weightTfIdf
  )
)

tdm_tf_bounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)

tdm_tfidf_bounds <- TermDocumentMatrix(
  corpus,
  control = list(
    weigthing = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

dtm_tfidf_bounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weigthing = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

tdm_tf_all_m <- as.matrix(tdm_tf_all)
tdm_tf_all_df <- as.data.frame(tdm_tf_all_m)

tdm_tf_bounds_m <- as.matrix(tdm_tf_bounds)
tdm_tf_bounds_df <- as.data.frame(tdm_tf_bounds_m)

matrix_file <- "./tdm_tf_all.csv"
write.table(
  tdm_tf_all_m, 
  matrix_file, 
  sep = ";", 
  dec = ",", 
  col.names = NA
)
