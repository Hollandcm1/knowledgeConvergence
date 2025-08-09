#' Build Global LSA Centroid
#'
#' Compute a Latent Semantic Analysis (LSA) centroid representing the semantic content
#' of the provided conversation data. The function builds a text corpus from the
#' `text` column, constructs a term–document matrix, performs LSA dimensionality
#' reduction, and returns the reduced-space representation along with the global
#' centroid.
#'
#' @param df A data frame containing at least a `text` column. Any other columns
#'   are ignored by this function.
#' @param k Integer. The number of dimensions to retain in the LSA-reduced space.
#'   Defaults to 100.
#' @param verbose Logical. Should progress messages be printed? Defaults to `TRUE`.
#'
#' @return A list of class `overall_centroid_result` containing:
#' \describe{
#'   \item{lsa_result}{The result of [lsa::lsa()] containing reduced-space matrices.}
#'   \item{centroid}{A numeric vector: the average position of all documents in LSA space.}
#'   \item{corpus}{The [tm::Corpus] used to create the term–document matrix.}
#'   \item{dtm}{The term–document matrix used for LSA (as a base matrix).}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_centroid(my_data, k = 100)
#' str(result$centroid)
#' }
#'
#' @importFrom tm Corpus VectorSource TermDocumentMatrix
#' @importFrom lsa lsa
#' @export
build_centroid <- function(df, k = 100, verbose = TRUE) {

  corpus <- build_corpus(df)

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