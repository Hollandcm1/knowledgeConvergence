#' Build Participant Running Similarity to Group Centroid
#'
#' For each participant in a single-group data frame, compute a running centroid of
#' their own utterances (in reduced LSA space) and the cosine similarity of that
#' running centroid to the group's global centroid.
#'
#' @param single_group_df A data frame for one group containing columns `participant`,
#'   `group`, `X` (time/order), `text`, and the reduced-space dimensions (from LSA)
#'   as additional numeric columns.
#' @param group_centroid Numeric vector. The group's centroid in reduced space.
#' @param verbose Logical; print progress messages. Defaults to `TRUE`.
#'
#' @return A named list where each element corresponds to a participant and contains
#'   a data frame with:
#' \itemize{
#'   \item `row_index`: the preserved original row index from column `X`.
#'   \item `cosine_similarity`: similarity of the participant's running centroid to the group centroid.
#' }
#'
#' @examples
#' \dontrun{
#' pcs <- build_participant_running_centroid(single_group_df, group_centroid)
#' str(pcs[[1]])
#' }
#'
#' @importFrom dplyr select
#' @importFrom lsa cosine
#' @export
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
    # participant_centroid <- colMeans(participant_vectors)

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
