#' Run Knowledge Convergence Analysis
#'
#' This is the primary function of the `knowledgeConvergence` package. It computes semantic similarity measures
#' between participants and their group by calculating centroids of meaning across time. These results are used to assess
#' the degree and trajectory of conceptual alignment within a group conversation.
#'
#' Specifically, this function:
#' \itemize{
#'   \item Computes a semantic centroid for each group.
#'   \item Tracks the running centroid of each group over the course of a conversation.
#'   \item Computes running centroids for each participant and their evolving similarity to the group centroid.
#' }
#'
#' The returned object can be used for further visualization or statistical modeling using other functions in the package.
#'
#' @param data A cleaned and pre-processed data frame of conversation transcripts.
#'   Must include variables such as `group_num`, `participant_num`, and `text`.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{group_centroid}{A data frame representing the group-level semantic centroid.}
#'   \item{group_running_centroid}{A data frame tracking the group centroid's cosine similarity trajectory.}
#'   \item{participant_running_centroids}{A named list of data frames with cosine similarity trajectories for each participant.}
#' }
#'
#' @examples
#' \dontrun{
#' cleaned_data <- apply_cleaning_steps(raw_data, list_of_cleaning_functions)
#' results <- run_kc(cleaned_data)
#' }
#'
#' @export
run_kc <- function(data) {

  result_group_centroid <- build_group_centroid(data)

  result_group_running_centroid <- build_group_running_centroid(result_group_centroid)

  result_participant_running_centroid <- build_participant_running_centroid(result_group_centroid)

  final_results <- list(
    group_centroid = result_group_centroid,
    group_running_centroid = result_group_running_centroid,
    participant_running_centroids = result_participant_running_centroid
  )

  return(final_results)
}