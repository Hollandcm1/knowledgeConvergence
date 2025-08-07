#' Visualize Knowledge Convergence Trajectories
#'
#' This function creates line plots of cosine similarity trajectories between each participant's
#' running semantic centroid and the group-level centroid over time. It also includes the group trajectory.
#'
#' @param group_running_centroid A data frame containing `row_index` and `cosine_similarity` columns
#'   for the group's running centroid (as returned by `build_group_running_centroid()`).
#' @param participant_running_centroids A named list of data frames, one per participant,
#'   each with `row_index` and `cosine_similarity` columns (as returned by `build_participant_running_centroid()`).
#'
#' @return A list of `ggplot` objects:
#' \describe{
#'   \item{group_plot}{A line plot showing the group's running cosine similarity.}
#'   \item{participant_plot}{A multi-line plot showing each participant's similarity trajectory.}
#'   \item{combined_plot}{A combined plot showing all trajectories.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- visualize_kc_plot(group_running, participant_running)
#' plots$combined_plot
#' }
visualize_kc_plot <- function(group_running_centroid, participant_running_centroids) {
  library(ggplot2)

  # Create a data frame for the group running centroid
  group_df <- data.frame(
    participant = "Group",
    time = group_running_centroid$row_index,
    cosine_similarity = group_running_centroid$cosine_similarity
  )

  # Create a data frame for participant running centroids (now includes row_index)
  participant_df <- do.call(rbind, lapply(names(participant_running_centroids), function(participant) {
    df <- participant_running_centroids[[participant]]
    df$participant <- participant  # add a column for participant ID
    df
  }))

  # Rename 'row_index' to 'time' for plotting consistency
  colnames(participant_df)[colnames(participant_df) == "row_index"] <- "time"

  # Combine the group and participant data frames
  combined_df <- rbind(group_df, participant_df)

  # Plot the group running centroid
  p1 <- ggplot(group_df, aes(x = time, y = cosine_similarity)) +
    geom_line(color = "blue") +
    labs(title = "Group Running Centroid", x = "Time", y = "Cosine Similarity") +
    theme_minimal()

  # Plot the participant running centroids
  p2 <- ggplot(participant_df, aes(x = time, y = cosine_similarity, color = participant)) +
    geom_line() +
    labs(title = "Participant Running Centroids", x = "Row Index", y = "Cosine Similarity") +
    theme_minimal()

  # Plot the combined data
  p3 <- ggplot(combined_df, aes(x = time, y = cosine_similarity, color = participant)) +
    geom_line() +
    labs(title = "Combined Running Centroids", x = "Row Index", y = "Cosine Similarity") +
    theme_minimal()

  p4 <- ggplot(combined_df, aes(x = time, y = cosine_similarity, color = participant)) +
    geom_point() +
    labs(title = "Combined Running Centroids", x = "Row Index", y = "Cosine Similarity") +
    theme_minimal()

  return(list(group_plot = p1, participant_plot = p2, combined_plot = p3, combined_plot_points = p4))
}