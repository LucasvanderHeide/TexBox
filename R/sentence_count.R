
#' Count the number of sentences in text
#'
#' Counts the number of sentences in each element
#' of a character vector. Sentences are separated by
#' periods, exclamation marks or question marks.
#'
#' @param text A character vector containing text.
#'
#' @return An integer vector containing the number of
#'   sentences in each element. Missing values return NA.
#'
#' @examples
#' sentence_count("Hello world!")
#' sentence_count("Hello! How are you?")
#' sentence_count(c("First sentence.", "Second sentence."))
#'
#' @seealso [word_count()]
#' @export
sentence_count <- function(text) {

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

    sentences <- strsplit(x, "[.!?]+")[[1]]
    sentences <- trimws(sentences)

    as.integer(sum(nzchar(sentences)))

  }, integer(1))
}
