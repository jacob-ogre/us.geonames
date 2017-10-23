#' Keep 'natural' feature types in df
#'
#' @param df A data frame from one of the \code{us.geonames} search functions
#'
#' @export
#'
gn_filter_natural_places <- function(df) {
  nat <- c("Arroyo", "Bar", "Basin", "Bay", "Beach", "Bench", "Bend", "Cape",
           "Channel", "Cliff", "Crater", "Falls", "Dam", "Flat", "Forest",
           "Gap", "Glacier", "Gut", "Harbor", "Island", "Isthmus", "Lake",
           "Lava", "Park", "Pillar", "Plain", "Range", "Rapids", "Reserve",
           "Reservoir", "Ridge", "Sea", "Slope", "Spring", "Stream", "Summit",
           "Sqamp", "Valley", "Woods")
  dplyr::filter(df, feature_class %in% nat)
}
