#' Build Corpus from Text Data
#'
#' This function takes a data frame and constructs a text corpus from a specified column.
#' It uses the \code{tm} package to create a corpus suitable for text mining operations.
#'
#' @param df A data frame containing text data.
#' @param text_col The name of the column containing the text to be converted into a corpus. Defaults to "text".
#'
#' @return A text corpus of class \code{Corpus} from the \code{tm} package.
#'
#' @examples
#' df <- data.frame(text = c("This is a test.", "Another sentence."))
#' corpus <- build_corpus(df)
#' inspect(corpus)
#'
#' @export
build_corpus <- function(df, text_col = "text") {
  corpus <- tm::Corpus(tm::VectorSource(df[[text_col]]))
  return(corpus)
}