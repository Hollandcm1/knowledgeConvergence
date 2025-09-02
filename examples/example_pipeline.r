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
