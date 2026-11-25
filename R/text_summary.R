#' Create a summary of text
#'
#' Calculates the number of characters, words and
#' sentences for each element of a character vector.
#'
#' @param text A character vector containing text.
#'
#' @return A data frame with one row per input text
#'   and columns for the original text, number of
#'   characters, words and sentences.
#'   Missing values return NA for the counts.
#'
#' @examples
#' text_summary("Hello world!")
#' text_summary(c("Hello world!", "Welcome to TexBox."))
#'
#' @seealso [word_count()], [sentence_count()], [clean_text()]
#' @export
text_summary <- function(text) {

  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  data.frame(
    text = text,
    characters = nchar(text),
    words = unname(word_count(text)),
    sentences = unname(sentence_count(text)),
    stringsAsFactors = FALSE
  )
}
