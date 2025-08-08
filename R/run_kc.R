#' Run Knowledge Convergence Analysis
#'
#' This function performs the main analysis for measuring conceptual alignment across participants in a group conversation. It calculates semantic centroids using Latent Semantic Analysis (LSA), tracks group-level trajectories over time, and computes each participant's evolving similarity to their group's centroid. If a grouping column is provided, the analysis is performed separately for each group; otherwise, all data is treated as a single group.
#'
#' @param df A cleaned and pre-processed data frame containing at minimum the columns `group_num`, `participant_num`, and `text`. An additional column specified by `time` is required for tracking temporal progression.
#' @param participant_col Name of the column identifying participants. Defaults to `"participant_num"`.
#' @param group_col Optional. Name of the column identifying groups. If NULL, data is treated as a single group.
#' @param text_col Name of the column containing text data. Defaults to `"text"`.
#' @param time_col Name of the column indicating temporal order (e.g., turn number or timestamp). Defaults to `"X"`.
#' @param k The number of dimensions to retain in the LSA-reduced space. Defaults to 100.
#' @param verbose Logical flag to indicate whether progress messages should be printed. Defaults to `TRUE`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{group_centroid}{A list containing the LSA output, semantic centroid, and intermediate representations for the full group conversation.}
#'   \item{group_running_centroid}{A data frame showing the trajectory of the group-level semantic centroid over time.}
#'   \item{participant_running_centroids}{A named list of data frames, each representing a participant's similarity to the group centroid over time.}
#' }
#'
#' @examples
#' \dontrun{
#' cleaned_data <- apply_cleaning_steps(raw_data, list_of_cleaning_functions)
#' results <- run_kc(cleaned_data)
#' print(results$group_running_centroid)
#' }
#'
#' @export
run_kc <- function(df,
                   participant_col = "participant",
                   group_col = NULL,
                   text_col = "text",
                   time_col = "X",
                   k = 100,
                   verbose = TRUE) {

  required_cols <- c(participant_col, text_col, time_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # rename columns to standard internal names
  colnames(df)[colnames(df) == participant_col] <- "participant"
  colnames(df)[colnames(df) == group_col] <- "group"
  colnames(df)[colnames(df) == text_col] <- "text"
  colnames(df)[colnames(df) == time_col] <- "X"

  # remove any extra columns that are not needed
  df <- df[, c("participant", "group", "X", "text")]

  # if a group column is provided, sort the data frame by groupd and time
  if (!is.null(group_col)) {
    df <- df[order(df$group, df$X), ]
  } else {
    df <- df[order(df$X), ]
  }

  # this is a centroid for all the data (in the case of a single group, it is also the group centroid)
  overall_centroid_result <- build_centroid(df, k, verbose = verbose)
  
  # extract lsa_result$dk and append to the original data frame
  df <- cbind(df, overall_centroid_result$lsa_result$dk)

  # if no group provided, create placeholder for group column
  if (is.null(group_col)) {
    df$group <- "overall"
  }

  grouped_dfs <- df %>%
      group_split(group)

  group_names <- unique(df$group)

  result <- grouped_dfs %>%
    set_names(group_names) %>%
    map(~ {
      single_group_df <- .

      # select all columns except participant, group, X, and text
      single_group_df_vectors <- single_group_df %>%
        select(-participant, -group, -X, -text)

      group_centroid <- colMeans(single_group_df_vectors)

      # build group running centroid
      group_running_centroid <- build_group_running_centroid(group_centroid, as.matrix(single_group_df_vectors), verbose = verbose)

      # build participant running centroids
      participant_running_centroids <- build_participant_running_centroid(single_group_df, group_centroid, verbose = verbose)

      # visulalize
      visuals <- visualize_kc_plot(
        group_running_centroid,
        participant_running_centroids
      )

      return(list(
        group_running_centroid = group_running_centroid,
        participant_running_centroids = participant_running_centroids,
        visualizations = visuals,
        df = single_group_df
      ))
    })

  # final_result <- list(
  #   group_running_centroid = group_running_centroid,
  #   participant_running_centroids = participant_running_centroids,
  #   visualizations = visuals,
  #   df = df
  # )

}