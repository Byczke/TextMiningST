# wczytanie bibliotek
library(lsa)

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file = "./frequency_matrix.R"
source(source_file)

# przygotowanie danych
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_bounds)),
    sep = ""
  ),
  rownames(dtm_tfidf_bounds),
  sep = " -> "
)

options(scipen = 5)

# analiza głównyh składowych
pca_model <- prcomp(dtm_tfidf_bounds)

x <- pca_model$x[,1]
y <- pca_model$x[,2]

plot_file <- "./pca.png"
png(plot_file)
plot(
  x,
  y,
#  xlim = c(0,50),
#  ylim = c(-10,0),
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "purple",
  pch = 16

)
text(
  x,
  y,
  paste(
    "d",
    1:length(rownames(dtm_tfidf_bounds)),
    sep = ""
  ),
  col = "purple",
  pos = 1
)
legend(
  "top",
  legend,
  cex = 0.6,
  text.col = "purple"
)
dev.off()

# analiza ukrytych wymiarów semantycznych
# dekompozycja wg wartości osobliwych
lsa_model <- lsa(tdm_tf_bounds_m)

coord_docs <- lsa_model$dk%*%diag(lsa_model$sk)
coord_terms <- lsa_model$tk%*%diag(lsa_model$sk)
terms_importance <- diag(
  lsa_model$tk%*%diag(lsa_model$sk)%*%t(diag(lsa_model$sk))%*%t(lsa_model$tk)
)
important_terms <- names(
  tail(
    sort(terms_importance),
    30
  )
)
own_terms <- c("harry", "hermiona", "łucja", "edmund")
current_terms <- own_terms
x1 <- coord_docs[,1]
y1 <- coord_docs[,2]
x2 <- coord_terms[current_terms,1]
y2 <- coord_terms[current_terms,2]

plot_file <- "./lsa.png"
png(plot_file)
plot(
  x1,
  y1,
#  xlim = c(-50,0),
#  ylim = c(-10,20),
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = "purple",
  pch = 16
  
)
text(
  x1,
  y1,
  paste(
    "d",
    1:length(rownames(dtm_tfidf_bounds)),
    sep = ""
  ),
  col = "purple",
  pos = 1
)
points(
  x2, 
  y2,
  pch = 15,
  col = "magenta"
)
text(
  x2,
  y2,
  rownames(coord_terms[current_terms,]),
  col = "magenta"
)
legend(
  "topleft",
  legend,
  cex = 0.6,
  text.col = "purple"
)
dev.off()
