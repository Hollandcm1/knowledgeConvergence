


build_centroid <- function(df) {

  corpus <- build_corpus(df, text_col = text_col)

  dtm <- as.matrix(tm::TermDocumentMatrix(corpus))

  if (verbose) message("Computing k-reduced approximation of the matrix. This may take a minute...")
  lsa_result <- lsa::lsa(dtm, dims = k)

  centroid <- colMeans(lsa_result$dk)

  structure(list(
    lsa_result = lsa_result,
    centroid = centroid,
    corpus = corpus,
    dtm = dtm
  ), class = "overall_centroid_result")

}
