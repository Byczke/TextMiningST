# wczytanie bibliotek
library(wordcloud)

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file = "./frequency_matrix.R"
source(source_file)

# przygotowanie katalogu na wyniki
clouds_dir <- "./clouds"
dir.create(clouds_dir)

# waga tf jako miara ważności słów
for (doc_no in 1:length(corpus)) {
  print(rownames(as.matrix(dtm_tf_all))[doc_no])
  print(head(sort(as.matrix(dtm_tf_all)[doc_no,],decreasing = T)))
}

# waga tfif jako miara ważności słów
for (doc_no in 1:length(corpus)) {
  print(rownames(t(as.matrix(tdm_tfidf_all)))[doc_no])
  print(head(sort(t(as.matrix(tdm_tfidf_all))[doc_no,],decreasing = T)))
}
