

result <- run_kc(df, group="group")


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
  overall_centroid_result <- build_centroid(df)
  
  # extract lsa_result$dk and append to the original data frame
  df <- cbind(df, overall_centroid_result$lsa_result$dk)

  # if no group provided, create placeholder for group column
  if (is.null(group_col)) {
    df$group <- "overall"
  }

  grouped_dfs <- df %>%
      group_split(group)

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
