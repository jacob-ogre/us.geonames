#' Search a file f for all geonames using \link{fastmatch}
#'
#' Fast reverse-lookup search of all 2.27M place names in \code{geonames}.
#' Whereas \link{gn_search_all} searches the 2.27M geonames against the
#' \code{text}, this function tokenizes
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param ngram_min Min ngram length to create of each \code{texts} (default = 1)
#' @param ngram_max Max ngram length to create of each \code{texts} (default = 7)
#'
#' @export
#'
gn_search_all <- function(text, ngram_min = 1, ngram_max = 7) {
  terms <- gn_tokenizer(text)
  matches <- fastmatch::fmatch(terms, geonames.lite$feature_name)
  res <- dplyr::data_frame(terms = terms, match = matches)
  res <-  dplyr::filter(res, !is.na(res$match))
  result <- suppressWarnings(dplyr::left_join(
    res, geonames.lite,
    by = c("terms" = "feature_name")
  ))
  return(result)
}

#' Search a file f for all geonames using stringi
#'
#' Costly but thorough search of all 2.27M place names in \code{geonames}.
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#'
#' @export
#'
gn_search_all_stri <- function(text) {
  cur_set <- geonames.lite
  cur_set$N_HIT <- stringi::stri_count_fixed(
    str = text,
    pattern = geonames.lite$FEATURE_NAME
  )
  hits <- dplyr::filter(cur_set, N_HIT > 0)
  return(hits)
}

#' Search text for all geonames from a given state
#'
#' Why search all of geonames if you know the state(s) of interest?
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param state Two-letter state abbreviation, e.g., VA for Virginia
#'
#' @export
#'
gn_search_state <- function(text, states) {
  terms <- gn_tokenizer(text)
  states <- c(states, "")
  cur_set <- dplyr::filter(geonames.lite, state_alpha %in% states)
  matches <- fastmatch::fmatch(terms, cur_set$feature_name)
  res <- dplyr::data_frame(terms = terms, match = matches)
  res <- dplyr::filter(res, !is.na(res$match))
  result <- suppressWarnings(dplyr::left_join(
    res, geonames.lite,
    by = c("terms" = "feature_name")
  ))
  result <- dplyr::filter(
    result,
    state_alpha %in% states
  )
  return(result)
}

#' Search text for all geonames from a given state using stringi
#'
#' Why search all of geonames if you know the state(s) of interest?
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param state Two-letter state abbreviation, e.g., VA for Virginia
#'
#' @export
#'
gn_search_state_stri <- function(text, states) {
  states <- c(states, "")
  cur_set <- dplyr::filter(geonames.lite, STATE_ALPHA %in% states)
  cur_set$N_HIT <- stringi::stri_count_fixed(
    str = text,
    pattern = cur_set$FEATURE_NAME
  )
  hits <- dplyr::filter(cur_set, N_HIT > 0)
  return(hits)
}

#' Search text for all geonames from a given county and state
#'
#' Reduce the search space by specifying a list of state, county combinations.
#' This is particularly useful when one knows the area of interest is
#' restricted to one or a few counties.
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param st_cnty List of 2-element vectors of form c("ST", "County")
#'
#' @export
#'
gn_search_county <- function(text, st_cnty) {
  cur_set <- st <- co <- NULL
  for(i in st_cnty) {
    prt <- dplyr::filter(
      geonames.lite,
      (state_alpha == i[1] | state_alpha == "") &
        (county_name == i[2] | county_name == "")
    )
    st <- c(st, i[1])
    co <- c(co, i[2])
    cur_set <- rbind(cur_set, prt)
  }
  terms <- gn_tokenizer(text)
  matches <- fastmatch::fmatch(terms, cur_set$feature_name)
  res <- dplyr::data_frame(terms = terms, match = matches)
  res <- dplyr::filter(res, !is.na(res$match))
  result <- suppressWarnings(dplyr::left_join(
    res, geonames.lite,
    by = c("terms" = "feature_name")
  ))
  result <- dplyr::filter(
    result,
    state_alpha %in% st & county_name %in% co
  )
  return(result)
}

#' Search text for all geonames from a given county and state using stringi
#'
#' Reduce the search space by specifying a list of state, county combinations.
#' This is particularly useful when one knows the area of interest is
#' restricted to one or a few counties.
#'
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param st_cnty List of 2-element vectors of form c("ST", "County")
#'
#' @export
#'
gn_search_county_stri <- function(text, st_cnty) {
  cur_set <- NULL
  for(i in st_cnty) {
    prt <- dplyr::filter(
      geonames.lite,
      (state_alpha == i[1] | state_alpha == "") &
        (county_name == i[2] | county_name == "")
    )
    cur_set <- rbind(cur_set, prt)
  }
  cur_set$N_HIT <- stringi::stri_count_fixed(
    str = text,
    pattern = cur_set$FEATURE_NAME
  )
  hits <- dplyr::filter(cur_set, N_HIT > 0)
  return(hits)
}

#' Search text for all geonames from a given state (db version)
#'
#' Why search all of geonames if you know the state(s) of interest? This uses
#' a database connection rather than relying on reading the entire 2.3M rows
#' of geonames from file.
#'
#' @param con A db connection from \link{DBI} or a \link[dplyr]{tbl} object; the
#'   tbl is preferred
#' @param text Text to be searched for geonames; should be 'clean,' i.e., no
#'   eol characters, multi-spaces replaced with singles, etc.
#' @param state Two-letter state abbreviation, e.g., VA for Virginia
#'
#' @export
#'
gndb_search_state <- function(con, text, states) {
  if(!("tbl" %in% class(con))) {
    con <- try(con <- dplyr::tbl(con, "geonames"))
    if(class(con) == "try-error") {
      stop("Please supply a valid SQL connection.")
    }
  }
  states <- c(states, "")
  cur_set <- con %>%
    dplyr::filter(state_alpha %in% states) %>%
    as_data_frame()
  cur_set$N_HIT <- stringi::stri_count_fixed(
    str = text,
    pattern = cur_set$feature_name
  )
  hits <- dplyr::filter(cur_set, N_HIT > 0)
  return(hits)
}

