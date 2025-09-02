# Example Pipeline

# libraries
library(here)
library(devtools)
devtools::install_github("Hollandcm1/knowledgeConvergence", force = TRUE)
library(knowledgeConvergence)

# library(lsa) # also required, but should get installed with knowledgeConvergence
# library(ggplot2)
# library(dplyr)
# library(purrr)

# load the data
data_path <- here("examples", "example_data", "conversation_example.csv")
df <- read.csv(data_path)

# add an X column since example data does not have one
df$X <- 1:nrow(df)

result <- run_kc(df,
                 participant_col = "participant",
                 group_col = NULL,
                 text_col = "text",
                 time_col = "X",
                 k = 100,
                 verbose = TRUE)

save_path <- here("examples", "example_output")
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
# save the "combined_plot_points" plot for each group (only one group in this case)
ggsave(file.path(save_path, "combined_plot_points.png"), plot = result$overall$visualizations$combined_plot_points)


# Access to the underlying data is also available
names(result$overall)