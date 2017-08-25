#' Get text from a file and clean for searching
#'
#' @param file Path to the file to be read
#' @return A single-element text vector of clean text for searching
#' @export
get_text <- function(file) {
  text <- readLines(con = file)
  text <- clean_text(text)
  return(text)
}

clean_text <- function(text) {
  t1 <- cat_text(text)
  t2 <- clean_multispace(t1)
  return(t2)
}

#' @importFrom stringr str_replace_all
cat_text <- function(x) {
  cln <- stringr::str_replace_all(x, pattern = "\n", replacement = "")
  res <- paste(cln, collapse = " ")
  return(res)
}

#' @importFrom stringr str_replace_all
clean_multispace <- function(x) {
  res <- stringr::str_replace_all(x, " {2,}", " ")
  return(res)
}