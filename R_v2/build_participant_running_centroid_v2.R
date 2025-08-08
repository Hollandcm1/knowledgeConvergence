
build_participant_running_centroid <- function(single_group_df, group_centroid verbose) {

  if (verbose) message("Building participant running centroids...")

  participant_names <- unique(single_group_df$participant)

  participant_running_centroids <- lapply(participant_names, function(participant) {
    if (verbose) message("Processing participant: ", participant)
    participant_df <- single_group_df[single_group_df$participant == participant, ]

    # select all columns except participant, group, X, and text
    participant_vectors <- participant_df %>%
      select(-participant, -group, -X, -text)

    # calculate the participant centroid
    participant_centroid <- colMeans(participant_vectors)

    # build participant running centroid
    participant_running_centroid <- build_group_running_centroid(group_centroid, as.matrix(participant_vectors), verbose = verbose)

    # add participant identifier to the result
    # participant_running_centroid$participant <- participant

    return(participant_running_centroid)
  })

  names(participant_running_centroids) <- participant_names

  return(participant_running_centroids)
}