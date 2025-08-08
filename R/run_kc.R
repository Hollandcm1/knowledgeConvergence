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

  if (verbose) message("Starting knowledge convergence analysis...")

  # rename columns to standard internal names
  colnames(df)[colnames(df) == participant_col] <- "participant"
  colnames(df)[colnames(df) == group_col] <- "group"
  colnames(df)[colnames(df) == text_col] <- "text"
  colnames(df)[colnames(df) == time_col] <- "X"

  # if a group column is provided, sort the data frame by groupd and time
  if (!is.null(group_col)) {
    df <- df[order(df$group, df$X), ]
  } else {
    df <- df[order(df$X), ]
  }

  if (!is.null(group_col)) {

    if (verbose) message("Grouping data by: ", group_col)

    overall_corpus <- build_corpus(df)
    result_overall_centroid <- build_group_centroid(df, k = k, verbose = verbose)

    # append the lsa#dk result onto the original data frame
    lsa_result <- result_overall_centroid$lsa_result
    df <- cbind(df, lsa_result$dk)

    grouped_dfs <- df %>%
      group_split(group)

    group_names <- map_chr(grouped_dfs, ~ as.character(unique(.x$group)))


    results_list <- grouped_dfs %>%
      set_names(group_names) %>%
      map(~ {
        single_group_df <- .
        # single_group_df <- grouped_dfs[[1]]
        # result_group_centroid <- build_group_centroid(single_group_df, corpus = overall_corpus, k = k, verbose = verbose)
        # separate the lsa result from the grouped_df (columns names 1-100)
        single_group_df_vectors <- single_group_df %>%
          select(matches("^[0-9]+$")) # %>%
          # rename_with(~ paste0("V", seq_along(.)), matches("^[0-9]+$"))
        # single_group_df$lsa_result$dk <- lsa_result$dk[, as.character(single_group_df$X)]
        # separate the rest of the data
        single_group_df_rest <- single_group_df %>%
          select(participant, group, X, text)

        # create a new result_group_centroid object
        result_group_centroid <- list()
        result_group_centroid$df <- single_group_df_rest
        
        result_group_centroid$lsa_result <- list(
          dk = as.matrix(single_group_df_vectors)
        )

        centroid <- colMeans(single_group_df_vectors)

        result_group_centroid <- list(
          lsa_result = result_group_centroid$lsa_result,
          centroid = centroid,
          corpus = result_overall_centroid$corpus,
          dtm = NULL,
          df = single_group_df_rest
        )

   
        result_group_running_centroid <- build_group_running_centroid(result_group_centroid, verbose = verbose)
        result_participant_running_centroid <- build_participant_running_centroid(result_group_centroid, verbose = verbose)
        result_visualizations <- visualize_kc_plot(result_group_running_centroid, result_participant_running_centroid, verbose = verbose)

        list(
          group = unique(single_group_df$group),
          group_centroid = result_group_centroid,
          group_running_centroid = result_group_running_centroid,
          participant_running_centroids = result_participant_running_centroid,
          visualizations = result_visualizations,
          df = single_group_df
        )
      })

    return(results_list)

  } else {

    if (verbose) message("No grouping column provided, treating all data as a single group.")
  
    result_group_centroid <- build_group_centroid(df, k = k, verbose = verbose)
    result_group_running_centroid <- build_group_running_centroid(result_group_centroid, verbose = verbose)
    result_participant_running_centroid <- build_participant_running_centroid(result_group_centroid, verbose = verbose)
    result_visualizations <- visualize_kc_plot(result_group_running_centroid, result_participant_running_centroid, verbose = verbose)

    final_results <- list(
      group_centroid = result_group_centroid,
      group_running_centroid = result_group_running_centroid,
      participant_running_centroids = result_participant_running_centroid,
      visualizations = result_visualizations,
      df = df
    )
  }

  if (verbose) message("Knowledge convergence analysis complete.")
  return(final_results)
}