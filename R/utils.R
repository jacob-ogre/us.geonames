#' Load \code{geonames.lite} and other startup code
#'
#' @export
#'
init <- function() {
  if(!("geonames.lite" %in% ls(envir = .GlobalEnv))) {
    message("
      Loading the geonames dataset, please wait...
    ")
    load(
      system.file("extdata", "geonames.lite.rda", package = "us.geonames"),
      .GlobalEnv
    )
  } else {
    message("
      The geonames dataset is already loaded.
    ")
  }
}

#' Change R variable names to db-safe versions.
#'
#' @param names The vector of variable names
#'
#' @export
#'
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

#' Tokenize word ngrams (1-7 default) of text
#'
#' The default of unigram to 7-gram tokenization was chosen because 99% of all
#' placenames in the geonames gazetteer are <= 7 words long.
#'
#' @param text A character vector to be tokenized
#' @param min The minimum ngram length to create (default = 1)
#' @param max The maximum ngram length to create (default = 7)
#'
#' @export
#'
gn_tokenizer <- function(text, min = 1, max = 7) {
  terms <- quanteda::tokens(
    text,
    "word",
    ngrams = 1:7,
    concatenator = " "
  )
  return(terms[[1]])
}

#' Read a text file and clean up spaces
#'
#' @param f Path to the text file to process
#'
#' @export
#'
gn_read_txt <- function(f) {
  text <- paste(readLines(f), collapse = " ")
  text <- str_replace_all(text, "[ ]{2,}", " ")
  return(text)
}



