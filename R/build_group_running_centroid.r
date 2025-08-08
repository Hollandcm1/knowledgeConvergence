#' Build Running Cosine Similarity to Group Centroid
#'
#' Computes the running centroid of all utterances in the dataset and calculates
#' cosine similarity at each step compared to the full group centroid.
#'
#' @param group_centroid_result A list returned from `build_group_centroid()`, containing:
#'   \itemize{
#'     \item \code{lsa_result}: the LSA result object containing the document-term matrix.
#'     \item \code{centroid}: the group-level centroid vector.
#'   }
#'
#' @return A data frame with:
#'   \itemize{
#'     \item \code{row_index}: the row number in the original data.
#'     \item \code{cosine_similarity}: the similarity between the running centroid and the full group centroid.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- build_group_centroid(my_data)
#' group_sim_df <- build_group_running_centroid(result)
#' }
build_group_running_centroid <- function(group_centroid_result, verbose = TRUE) {
  
  if (verbose) message("Building group running centroid...")
  
  # Extract the centroid vector
  centroid <- group_centroid_result$centroid

  # Extract lsa result
  lsa_result <- group_centroid_result$lsa_result

  # Preallocate cosine similarity array
  cosine_similarity_array <- numeric(nrow(lsa_result$dk))

  # Cumulative sum for efficient running centroid calculation
  cumulative_sum <- matrix(0, nrow = nrow(lsa_result$dk), ncol = ncol(lsa_result$dk))
  cumulative_sum[1, ] <- lsa_result$dk[1, ]
  if (nrow(lsa_result$dk) > 1) {
    for (i in 2:nrow(lsa_result$dk)) {
      cumulative_sum[i, ] <- cumulative_sum[i - 1, ] + lsa_result$dk[i, ]
    }
  }

  for (i in 1:nrow(lsa_result$dk)) {
    running_centroid <- cumulative_sum[i, ] / i
    cosine_similarity_array[i] <- lsa::cosine(running_centroid, centroid)
  }

  # Return a data frame with row index and similarity
  return(data.frame(
    row_index = seq_along(cosine_similarity_array),
    cosine_similarity = cosine_similarity_array
  ))
}
