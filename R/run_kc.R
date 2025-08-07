
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