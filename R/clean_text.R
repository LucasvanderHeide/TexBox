
#' Clean text by removing unnecessary whitespace
#'
#' Removes unnecessary spaces at the beginning and end
#' of text and replaces multiple spaces, tabs and line
#' breaks with a single space.
#'
#' @param text A character vector containing text.
#'
#' @return A character vector containing cleaned text.
#'   Missing values remain NA.
#'
#' @examples
#' clean_text("  Hello    world!  ")
#' clean_text("This   is   TexBox")
#' clean_text(c(" Hello ", "  Welcome   to TexBox  "))
#'
#' @seealso [word_count()], [sentence_count()]
#' @export
clean_text <- function(text) {

  if (!is.character(text)) {
    stop("`text` must be a character vector.", call. = FALSE)
  }

  text <- gsub("[[:space:]]+", " ", text)

  trimws(text)
}
