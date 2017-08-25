#' Basic leaflet map of points identified in a \code{us.geonames} search
#'
#' @export
#'
gn_leaflet_basic <- function(df) {
  leaflet::leaflet(df) %>%
    leaflet::addProviderTiles("Stamen.Terrain") %>%
    leaflet::addCircleMarkers(
      lng = df$prim_long_dec,
      lat = df$prim_lat_dec,
      weight = 1,
      color = "red",
      fillColor = "red",
      fillOpacity = 0.3,
      radius = 5,
      popup = ~paste0("<b>", df$terms, "</b><br>",
                      "<smaller>Class: ", df$feature_class, "</smaller>",
                      "<br><b>Long & Lat:</b><br>", df$prim_long_dec, "<br>",
                      df$prim_lat_dec)
    )
}

#' Calculate the minimum convex polygon of points, and map
#'
#' NOT YET IMPLEMENTED
#' 
#' @export
gn_add_mcp <- function(df, map, pct) {
  # create XYs, run mcp, addPolygons
  # ...
  stop("Not yet implemented.")
}

#' Calculate a basic kernel density estimator of points, and map
#'
#' NOT YET IMPLEMENTED
#' 
#' @export
add_kde <- function(df, map, pct) {
  # create XYs, run ud, addPolygons
  # ...
  stop("Not yet implemented.")
}
