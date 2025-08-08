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
build_participant_running_centroid <- function(single_group_df, group_centroid, verbose) {

  if (verbose) message("Building participant running centroids...")

  participant_names <- unique(single_group_df$participant)

  participant_running_centroids <- lapply(participant_names, function(participant) {
    if (verbose) message("Processing participant: ", participant)
    participant_df <- single_group_df[single_group_df$participant == participant, ]
    # preserve original row index from the `X` column
    row_index <- participant_df$X

    # select all columns except participant, group, X, and text
    participant_vectors <- participant_df %>%
      select(-participant, -group, -X, -text)

    # calculate the participant centroid
    participant_centroid <- colMeans(participant_vectors)

    # build participant running centroid
    participant_running_centroid <- build_group_running_centroid(group_centroid, as.matrix(participant_vectors), verbose = verbose)
    # attach the preserved original row index as `row_index` so downstream plotting aligns with original rows
    participant_running_centroid$row_index <- row_index

    # add participant identifier to the result
    # participant_running_centroid$participant <- participant

    return(participant_running_centroid)
  })

  names(participant_running_centroids) <- participant_names

  return(participant_running_centroids)
}
