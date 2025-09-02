


data_path <- here("examples", "example_data", "conversation_example.csv")
df <- read.csv(data_path)
df$X <- 1:nrow(df)

participant_col <- "participant"
group_col <- NULL
text_col <- "text"
time_col <- "X"
k <- 100
verbose <- TRUE
