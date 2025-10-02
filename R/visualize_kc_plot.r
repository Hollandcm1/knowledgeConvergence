#' Visualize Knowledge Convergence Trajectories
#'
#' Create line/point plots of cosine similarity trajectories between each participant's
#' running centroid and the group-level centroid over time (row order). Includes a
#' group trajectory and combined plots.
#'
#' @param group_running_centroid A data frame with columns `row_index` and `cosine_similarity`.
#' @param participant_running_centroids A named list of data frames, one per participant,
#'   each with `row_index` and `cosine_similarity`.
#' @param verbose Logical; print progress messages. Defaults to `TRUE`.
#'
#' @return A list of `ggplot` objects:
#' \describe{
#'   \item{group_plot}{Line plot for the group trajectory.}
#'   \item{participant_plot}{Multi-line plot of participant trajectories.}
#'   \item{combined_plot}{All trajectories combined (lines).}
#'   \item{combined_plot_points}{All trajectories combined (points).}
#' }
#'
#' @examples
#' \dontrun{
#' plots <- visualize_kc_plot(group_running, participant_running)
#' plots$combined_plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' @export
visualize_kc_plot <- function(group_running_centroid, participant_running_centroids, verbose = TRUE) {

  if (verbose) message("Preparing visualization...")

  # Create a data frame for the group running centroid
  group_df <- data.frame(
    participant = "Group",
    time = group_running_centroid$row_index,
    cosine_similarity = group_running_centroid$cosine_similarity
  )

  # Use indices to avoid [[NA]] lookups when a name is missing
  nms <- names(participant_running_centroids)
  cleaned_list <- lapply(seq_along(participant_running_centroids), function(i) {
    df <- participant_running_centroids[[i]]

    # Skip NULL elements explicitly
    if (is.null(df)) {
      stop(sprintf("Element %d of `participant_running_centroids` is NULL.", i))
    }

    # Determine participant id (fallback to index if name is NA/empty)
    pid <- nms[i]
    if (is.null(pid) || is.na(pid) || identical(pid, "") ) pid <- as.character(i)

    # Basic checks with informative errors
    required_cols <- c("row_index", "cosine_similarity")
    if (!all(required_cols %in% names(df))) {
      missing <- setdiff(required_cols, names(df))
      stop(sprintf(
        "Participant '%s' (index %d) is missing required column(s): %s\nAvailable columns: %s",
        pid, i, paste(missing, collapse = ", "), paste(names(df), collapse = ", ")
      ))
    }

    # Keep only required columns in a fixed order
    df <- df[, required_cols, drop = FALSE]

    # Rename to match plotting semantics and coerce types
    names(df)[names(df) == "row_index"] <- "time"
    df$time <- as.numeric(df$time)
    df$cosine_similarity <- as.numeric(df$cosine_similarity)

    # Tag with participant id as a character column
    df$participant <- as.character(pid)

    # Ensure final column order is consistent
    df <- df[, c("participant", "time", "cosine_similarity")]
    df
  })

  # Combine cleaned participant data frames
  participant_df <- do.call(rbind, cleaned_list)

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
    labs(title = "Participant Running Centroids", x = "Time", y = "Cosine Similarity") +
    theme_minimal()

  # Plot the combined data
  p3 <- ggplot(combined_df, aes(x = time, y = cosine_similarity, color = participant)) +
    geom_line() +
    labs(title = "Combined Running Centroids", x = "Time", y = "Cosine Similarity") +
    theme_minimal()

  p4 <- ggplot(combined_df, aes(x = time, y = cosine_similarity, color = participant)) +
    geom_point() +
    labs(title = "Combined Running Centroids", x = "Time", y = "Cosine Similarity") +
    theme_minimal()

  return(list(group_plot = p1, participant_plot = p2, combined_plot = p3, combined_plot_points = p4))
}