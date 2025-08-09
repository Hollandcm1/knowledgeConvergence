#' Run Knowledge Convergence Analysis
#'
#' Perform the main analysis for measuring conceptual alignment across participants
#' in a conversation. The function builds a global LSA space, computes a group
#' centroid and running group trajectory, and for each participant computes a running
#' similarity trajectory to the group centroid. If a grouping column is supplied,
#' the analysis is applied separately within each group; otherwise all rows are
#' treated as a single group.
#'
#' @param df A cleaned data frame containing at minimum the columns specified by
#'   `participant_col`, `text_col`, and `time_col`. If `group_col` is provided,
#'   it must also exist in `df`.
#' @param participant_col Column name for participants. Default: "participant".
#' @param group_col Optional column name for groups. Default: `NULL`.
#' @param text_col Column name containing the text. Default: "text".
#' @param time_col Column name indicating temporal order (e.g., turn index or timestamp).
#'   Default: "X".
#' @param k Integer. Number of LSA dimensions. Default: 100.
#' @param verbose Logical; print progress messages. Default: `TRUE`.
#'
#' @return A named list keyed by group (or a single element "overall" when `group_col` is `NULL`).
#'   Each element is a list with components:
#' \describe{
#'   \item{group_running_centroid}{Data frame of the group's running similarity to the group centroid.}
#'   \item{participant_running_centroids}{Named list of participant running similarity data frames.}
#'   \item{visualizations}{List of `ggplot` objects from [visualize_kc_plot()].}
#'   \item{df}{The group's data frame with reduced-space columns appended.}
#' }
#'
#' @examples
#' \dontrun{
#' results <- run_kc(cleaned_data,
#'                   participant_col = "participant",
#'                   group_col = NULL,
#'                   text_col = "text",
#'                   time_col = "X",
#'                   k = 100)
#' names(results)
#' results[[1]]$visualizations$combined_plot
#' }
#'
#' @importFrom dplyr select group_split
#' @importFrom purrr map set_names
#' @importFrom magrittr %>%
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
  colnames(df)[colnames(df) == text_col] <- "text"
  colnames(df)[colnames(df) == time_col] <- "X"
  if (!is.null(group_col)) {
    colnames(df)[colnames(df) == group_col] <- "group"
  }

  # remove any extra columns that are not needed
  keep <- c("participant", "X", "text")
  if (!is.null(group_col)) keep <- c(keep, "group")
  df <- df[, keep, drop = FALSE]

  # if no group provided, create placeholder for group column
  if (is.null(group_col)) {
    df$group <- "overall"
  }

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

  grouped_dfs <- df %>%
      group_split(group)

  group_names <- dplyr::group_keys(dplyr::group_by(df, group))$group

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

  return(result)

}