# Example Pipeline



install.packages(c("jsonlite", "dplyr", "tidyr"))

library(jsonlite)

# Path to the JSON file
file_path <- here("examples", "NaturalConv_Release_20210318", "dialog_release.json")

# Read the JSON into R (UTF-8 encoding by default)
dialog_list <- fromJSON(file_path)



json_text <- readLines(file_path, encoding = "UTF-8")
dialog_list <- fromJSON(paste(json_text, collapse = ""))








library(here)
devtools::load_all(here())
library(knowledgeConvergence)
library(lsa)
library(ggplot2)
library(dplyr)
library(purrr)

# # load the data
# data <- read.csv(here("examples", "example_data.csv"))

test_data <- here("testing_data", "test_data.rds")
test_data <- readRDS(test_data)

# pull onl the group 1 data
group_1_data <- test_data[test_data$group_num == 1, ]

# calculate the overall group centroid
result  <- build_group_centroid(group_1_data,
                     corpus = NULL, 
                     participant_col = "participant_num", 
                     text_col = "text", 
                     time_col = "X",
                     k = 100,
                     verbose = TRUE)


# build the group running centroid
group_running_centroid <- build_group_running_centroid(result)

# build the participant running centroids
participant_running_centroids <- build_participant_running_centroid(result)

# visualize the results
plots <- visualize_kc_plot(group_running_centroid, participant_running_centroids)

# display the plots
plots$combined_plot_points
