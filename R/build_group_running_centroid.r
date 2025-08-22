#' Build Running Cosine Similarity to a Reference Centroid
#'
#' Given a matrix of document vectors (rows = utterances, columns = LSA dimensions)
#' and a reference centroid vector, compute at each step the cosine similarity between
#' the cumulative running centroid and the reference centroid.
#'
#' @param group_centroid Numeric vector. The reference centroid in the same space as `dk`.
#' @param dk A numeric matrix where each row is a document/utterance vector in reduced space.
#' @param verbose Logical; print progress messages. Defaults to `TRUE`.
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item `row_index`: the step index (1..nrow(dk)).
#'   \item `cosine_similarity`: cosine similarity between the running centroid and `group_centroid`.
#' }
#'
#' @examples
#' \dontrun{
#' sims <- build_group_running_centroid(group_centroid, dk)
#' head(sims)
#' }
#'
#' @importFrom lsa cosine
#' @export
build_group_running_centroid <- function(group_centroid, dk, verbose) {

  # Preallocate cosine similarity array
  cosine_similarity_array <- numeric(nrow(dk))

  # Cumulative sum for efficient running centroid calculation
  cumulative_sum <- matrix(0, nrow = nrow(dk), ncol = ncol(dk))
  cumulative_sum[1, ] <- dk[1, ]
  if (nrow(dk) > 1) {
    for (i in 2:nrow(dk)) {
      cumulative_sum[i, ] <- cumulative_sum[i - 1, ] + dk[i, ]
    }
  }
  for (i in 1:nrow(dk)) {
    running_centroid <- cumulative_sum[i, ] / i
    cosine_similarity_array[i] <- lsa::cosine(running_centroid, group_centroid)
  }
  # Return a data frame with row index and similarity
  return(data.frame(
    row_index = seq_along(cosine_similarity_array),
    cosine_similarity = cosine_similarity_array
  ))
}

