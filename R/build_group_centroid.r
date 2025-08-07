#' Build Group-Level LSA Centroid
#'
#' This function computes a Latent Semantic Analysis (LSA)-based centroid representing
#' the semantic content of a group conversation. It performs dimensionality reduction
#' on a term-document matrix constructed from a provided data frame and returns a
#' structured object containing the centroid, LSA results, and other relevant metadata.
#'
#' @param df A data frame containing at minimum a participant identifier, text column, and time column.
#' @param corpus An optional prebuilt `tm::Corpus` object. If `NULL`, a corpus is generated from the `text` column.
#' @param participant_num The name of the column identifying participants. Defaults to `"participant_num"`.
#' @param text The name of the column containing text data. Defaults to `"text"`.
#' @param time The name of the column containing time or ordering information. Defaults to `"X"`.
#' @param k The number of dimensions to retain in the LSA-reduced space. Defaults to 100.
#' @param verbose Logical. Should progress messages be printed? Defaults to `TRUE`.
#'
#' @return A list of class `"build_group_centroid_result"` containing:
#' \describe{
#'   \item{lsa_result}{The result of `lsa::lsa()` containing the reduced-space representation.}
#'   \item{centroid}{A vector representing the average position of all documents in LSA space.}
#'   \item{corpus}{The `tm::Corpus` object used.}
#'   \item{dtm}{The term-document matrix.}
#'   \item{df}{The input data frame, relabeled to standard internal columns.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- build_group_centroid(my_data, k = 100)
#' str(result$centroid)
#' }
build_group_centroid <- function(df, corpus = NULL, 
                participant_num = "participant_num", 
                text = "text", 
                time = "X",
                k = 100,
                verbose = TRUE) {

  if (!requireNamespace("tm", quietly = TRUE)) stop("Package 'tm' is required.")
  if (!requireNamespace("lsa", quietly = TRUE)) stop("Package 'lsa' is required.")

  required_cols <- c(participant_num, text, time)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Relabel columns to standard internal names
  df <- df[, required_cols]
  colnames(df) <- c("participant", "text", "time")

  if (is.null(corpus)) {
    if (verbose) message("No corpus provided, creating from data frame.")
    corpus <- tm::Corpus(tm::VectorSource(df[["text"]]))
  } else {
    if (verbose) message("Using provided corpus.")
  }

  dtm <- as.matrix(tm::TermDocumentMatrix(corpus))

  if (verbose) message("Computing k-reduced approximation of the matrix. This may take a minute...")
  lsa_result <- lsa::lsa(dtm, dims = k)

  centroid <- colMeans(lsa_result$dk)

  structure(list(
    lsa_result = lsa_result,
    centroid = centroid,
    corpus = corpus,
    dtm = dtm,
    df = df
  ), class = "build_group_centroid_result")
}