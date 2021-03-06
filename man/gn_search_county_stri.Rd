% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{gn_search_county_stri}
\alias{gn_search_county_stri}
\title{Search text for all geonames from a given county and state using stringi}
\usage{
gn_search_county_stri(text, st_cnty)
}
\arguments{
\item{text}{Text to be searched for geonames; should be 'clean,' i.e., no
eol characters, multi-spaces replaced with singles, etc.}

\item{st_cnty}{List of 2-element vectors of form c("ST", "County")}
}
\description{
Reduce the search space by specifying a list of state, county combinations.
This is particularly useful when one knows the area of interest is
restricted to one or a few counties.
}
