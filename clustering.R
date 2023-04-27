# wczytanie bibliotek
library(dendextend)
library(corrplot)
library(proxy)
library(flexclust)

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file = "./frequency_matrix.R"
source(source_file)

# przygotowanie katalogu na wyniki
clusters_dir <- "./clusters"
dir.create(clusters_dir)

# analiza supień
# metoda hierarchiczna
# parametry:
## 1. macierz częstości
## a. waga (weighting)
## b. zakres zmiennych (all/bounds)
## 2. miara odległości (euclidean, jaccard, cosine)
## 3. sposób wysnaczania odległości skupień (single, complete, ward.D2)

# przygotowanie
doc_names <- rownames(dtm_tf_all)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:length(doc_names),
    sep = ""
  ),
  doc_names,
  sep = " -> "
)
clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
cols <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink", "dodgerblue", "darkmagenta", "khaki", "darkolivegreen")
cols_pattern <- c()
for (doc in 1:doc_count) {
  cols_pattern[doc] <- cols[clusters_pattern[doc]]
}
names(clusters_pattern) <- doc_names
names(cols_pattern) <- doc_names

# eksperyment 1
dist_matrix_1 <- dist(dtm_tf_all_m, method = "euclidean")
h_clust_1 <- hclust(dist_matrix_1, method = "complete")
plot_file <- paste(
  clusters_dir,
  "dend_tf_all_base.png",
  sep = "/"
)
png(plot_file)
plot(h_clust_1)
dev.off()
barplot(h_clust_1$height, names.arg = (doc_count-1):1)
dend_1 <- as.dendrogram(h_clust_1)
clust_caunt <- find_k(dend_1)$k
plot_file <- paste(
  clusters_dir,
  "dend_tf_all_color.png",
  sep = "/"
)
png(plot_file, height = 600)
par(mai = c(4,1,1,1))
cols_dend_1 <- color_branches(
  dend_1,
  k = clust_caunt
)
plot(cols_dend_1)
dev.off()
plot_file <- paste(
  clusters_dir,
  "dend_tf_all_pattern.png",
  sep = "/"
)
png(plot_file, height = 600)
par(mai = c(4,1,1,1))
cols_dend_1 <- color_branches(
  dend_1,
  col = cols_pattern[dend_1 %>% labels]
)
plot(cols_dend_1)
dev.off()
clust_1 <- cutree(h_clust_1, k=clust_caunt)
clust_matrix_1 <- matrix(0, doc_count, clust_caunt)
rownames(clust_matrix_1) <- doc_names
for (doc in 1:doc_count) {
  clust_matrix_1[doc, clust_1[doc]] <- 1
}
plot_file <- paste(
  clusters_dir,
  "matrix_tf_all.png",
  sep = "/"
)
png(plot_file)
corrplot(clust_matrix_1)
dev.off()

# eksperyment 2
dist_matrix_2 <- dist(dtm_tfidf_bounds_m, method = "cosine")
h_clust_2 <- hclust(dist_matrix_2, method = "ward.D2")
plot_file <- paste(
  clusters_dir,
  "dend_tfidf_bounds_base.png",
  sep = "/"
)
png(plot_file)
plot(h_clust_2)
dev.off()
#barplot(h_clust_2$height, names.arg = (doc_count-1):1)
dend_2 <- as.dendrogram(h_clust_2)
clust_caunt <- find_k(dend_2)$k
plot_file <- paste(
  clusters_dir,
  "dend_tfidf_bounds_color.png",
  sep = "/"
)
png(plot_file, height = 600)
par(mai = c(4,1,1,1))
cols_dend_2 <- color_branches(
  dend_2,
  k = clust_caunt
)
plot(cols_dend_2)
dev.off()
plot_file <- paste(
  clusters_dir,
  "dend_tfidf_bounds_pattern.png",
  sep = "/"
)
png(plot_file, height = 600)
par(mai = c(4,1,1,1))
cols_dend_2 <- color_branches(
  dend_2,
  col = cols_pattern[dend_2 %>% labels]
)
plot(cols_dend_2)
dev.off()
clust_2 <- cutree(h_clust_2, k=clust_caunt)
clust_matrix_2 <- matrix(0, doc_count, clust_caunt)
rownames(clust_matrix_2) <- doc_names
for (doc in 1:doc_count) {
  clust_matrix_2[doc, clust_2[doc]] <- 1
}
plot_file <- paste(
  clusters_dir,
  "matrix_tfidf_bounds.png",
  sep = "/"
)
png(plot_file)
corrplot(clust_matrix_2)
dev.off()

# porównanie wyników eksperymentów
plot_file <- paste(
  clusters_dir,
  "FMIndex.png",
  sep = "/"
)
png(plot_file)
Bk_plot(
  dend_1,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a - Mallows'a"
)
dev.off()

rand_exp1_exp2 <- comPart(clust_1, clust_2)
rand_exp1_pattern <- comPart(clust_1, clusters_pattern)
rand_exp2_pattern <- comPart(clust_2, clusters_pattern)