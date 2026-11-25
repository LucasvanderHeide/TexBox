
#' Count the number of words in text
#'
#' Counts the number of words in each element of a character vector.
#' Words are separated by whitespace.
#'
#' @param text A character vector containing text.
#'
#' @return An integer vector containing the number of words
#'   in each element. Missing values return NA.
#'
#' @examples
#' word_count("Hello world!")
#' word_count(c("Hello world", "Welcome to TexBox"))
#'
#' @export
word_count <- function(text) {

  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  vapply(text, function(x) {

    if (is.na(x)) {
      return(NA_integer_)
    }

    x <- trimws(x)

    if (x == "") {
      return(0L)
    }

    length(strsplit(x, "\\s+")[[1]])

  }, integer(1))
}
