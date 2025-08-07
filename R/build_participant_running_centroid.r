#' Build Running Cosine Similarity to Group Centroid by Participant
#'
#' For each participant, computes a running centroid across their utterances and
#' calculates the cosine similarity of that running centroid to the full group centroid.
#'
#' @param group_centroid_result A list returned from `build_group_centroid()`, containing:
#'   \itemize{
#'     \item \code{lsa_result}: the LSA result object containing the document-term matrix.
#'     \item \code{centroid}: the group-level centroid vector.
#'     \item \code{df}: a data frame with at least a `participant` column.
#'   }
#'
#' @return A named list where each element corresponds to a participant and contains a data frame with:
#'   \itemize{
#'     \item \code{row_index}: the row number from the original data.
#'     \item \code{cosine_similarity}: similarity to the full group centroid at each utterance.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- build_group_centroid(my_data)
#' participant_sims <- build_participant_running_centroid(result)
#' }
build_participant_running_centroid <- function(group_centroid_result) {
  # Extract LSA result
  lsa_result <- group_centroid_result$lsa_result

  # Get unique participants
  unique_participants <- unique(group_centroid_result$df$participant)

  # Prepare results
  result_list <- list()

  for (participant in unique_participants) {
    participant_indices <- which(group_centroid_result$df$participant == participant)
    dk_participant <- lsa_result$dk[participant_indices, , drop = FALSE]
    
    # Calculate cumulative sum
    cumulative_sum <- matrix(0, nrow = nrow(dk_participant), ncol = ncol(dk_participant))
    cumulative_sum[1, ] <- dk_participant[1, ]
    if (nrow(dk_participant) > 1) {
      for (i in 2:nrow(dk_participant)) {
        cumulative_sum[i, ] <- cumulative_sum[i - 1, ] + dk_participant[i, ]
      }
    }

    # Calculate cosine similarities
    cosine_similarity_array <- numeric(nrow(dk_participant))
    for (i in 1:nrow(dk_participant)) {
      running_centroid <- cumulative_sum[i, ] / i
      cosine_similarity_array[i] <- lsa::cosine(running_centroid, group_centroid_result$centroid)
    }

    # Save row indices and similarities as a data.frame
    result_list[[participant]] <- data.frame(
      row_index = participant_indices,
      cosine_similarity = cosine_similarity_array
    )
  }

  return(result_list)
}
