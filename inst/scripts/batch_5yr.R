library(dplyr)
library(ggplot2)
library(leaflet)
library(parallel)
library(plotly)
library(sp)
library(stringr)
library(us.geonames)

# init()
load("geonames.lite.rda")

files <- list.files("~/Downloads/five_year_review/",
# files <- list.files("~/Data/ESAdocs_text/five_year_review/",
                    full.names = TRUE,
                    recursive = TRUE)
files_df <- data_frame(
  file = files,
  doc = basename(files)
)

# Get 5y rev data table:
# load("/Users/jacobmalcom/Downloads/ECOS_species_tables_2016-12-17.rda")
load("ECOS_species_tables_2016-12-17.rda")
head(fiveyr_table)
fiveyr_table$doc <- basename(fiveyr_table$Doc_Link) %>%
  str_replace_all("pdf$", "txt")
fiveyr <- as_tibble(fiveyr_table)

# check multi-spp docs
mult <- tapply(fiveyr$Species,
               INDEX = fiveyr$Doc_Link,
               FUN = function(x) length(unique(x)))
head(sort(mult, decreasing = TRUE), 10)
sum(mult[mult > 1]) # 6.2% of 5yr
mm <- as_data_frame(mult)
ggplotly(qplot(data = mm, x = value, geom = "histogram", bins = 9))

# Get counties data and prep
# load("/Users/jacobmalcom/Work/Data/esadocs/rda/counties_table_2016-12-10.rda")
load("counties_table_2016-12-10.rda")
names(counties_table)
states <- data_frame(state_alpha = state.abb, State = state.name)
# cnt_occ <- as_tibble(left_join(counties_table, states, by = "State"))
cnt_occ <- readr::read_tsv("~/Downloads/EndSp_county_occurrences.tsv")

run_gn_search <- function(file) {
  NAs <- data_frame(
    "terms" = NA, "match" = NA, "feature_id" = NA, "feature_class" = NA,
    "state_alpha" = NA, "state_numeric" = NA, "county_name" = NA,
    "county_numeric" = NA, "prim_lat_dec" = NA, "prim_long_dec" = NA,
    "elev_in_m" = NA, "elev_in_ft" = NA, "map_name" = NA,
    "date_created" = NA, "date_edited" = NA, "species" = NA, "file" = file
  )
  cat("\n\n")
  cat(paste("Processing:", file, "\n"))
  f2 <- file
  sub_dat <- filter(fiveyr, doc == file) %>% distinct(doc, .keep_all = TRUE)
  fil_dat <- filter(files_df, doc == f2)
  data <- left_join(fil_dat, sub_dat, by = "doc")
  print(dim(data))
  cos <- filter(cnt_occ, cnt_occ$SCI == data$Species)
  cat("\n\n")
  cat(paste("Processing:", data$Species, "\n"))

  if(dim(cos)[1] > 0) {
    cos_ls <- vector("list", dim(cos)[1])
    for(i in 1:dim(cos)[1]) { cos_ls[[i]] <- c(cos$STABBREV[i], cos$NAME[i]) }
    text <- gn_read_txt(fil_dat$file)
    srch <- gn_search_county(text, cos_ls)
    if(dim(srch)[1] > 0) {
      srch$species <- data$Species
      srch$file <- file
      map <- us.geonames::gn_leaflet_basic(srch)
    } else {
      srch <- NAs
      srch$species <- data$Species
      map <- NA
    }
  } else {
    srch <- NAs
    map <- NA
    cos_ls <- list(NA)
    text <- gn_read_txt(fil_dat$file)
  }
  return(list(file = file,
              species = data$Species,
              found = srch,
              map = map,
              st_co = cos_ls,
              doc_nchar = nchar(text) ))
}

bind_all_res <- function(res_list) {
  dfs <- lapply(res_list, FUN = `[[`, 3)
  res <- bind_rows(dfs)
  return(res)
}

ts <- files_df$doc[21:25]
ot <- lapply(ts, FUN = run_gn_search)

ot_all <- bind_all_res(ot)

# ts <- files_df$doc[1:25]
ts <- files_df$doc[1:100]
ot <- parallel::mclapply(
  ts,
  FUN = run_gn_search,
  mc.cores = 4,
  mc.preschedule = FALSE
)

ts2 <- files_df$doc[101:length(files_df$doc)]
ot2 <- parallel::mclapply(
  ts2,
  FUN = run_gn_search,
  mc.cores = 4,
  mc.preschedule = FALSE
)

ts3 <- files_df$doc[1:100]
ot3 <- parallel::mclapply(
  ts3,
  FUN = run_gn_search,
  mc.cores = 4,
  mc.preschedule = FALSE
)

ts4 <- files_df$doc[101:length(files_df$doc)]
ot4 <- parallel::mclapply(
  ts4,
  FUN = run_gn_search,
  mc.cores = 4,
  mc.preschedule = FALSE
)

counties <- readr::read_tsv("~/Downloads/EndSp_county_occurrences.tsv")

#############################################
# LOADING DATA
dat <- readRDS("~/Downloads/fiveyr_results_manual_counties.rds")
all_res <- lapply(dat, FUN = `[[`, 3)
all_res_df <- dplyr::bind_rows(all_res)
nna <- filter(all_res_df, !is.na(terms))

phx_nun <- filter(nna, species == "Phoxinus cumberlandensis")
sp_place <- distinct(nna, species, terms, county_name, .keep_all = TRUE)
table(sp_place$species) %>% sort(decreasing = TRUE) %>% head(10)

## Look at a few examples:
phox <- filter(sp_place, species == "Phoxinus cumberlandensis")
table(phox$feature_class)
phox <- filter(phox, feature_class != "Civil" & feature_class != "Populated Place")
gn_leaflet_basic(phox)
# phox_gbif <- readr::read_tsv("~/Downloads/phox_gbif.csv")
phox_gbif <- occ_data(scientificName = "Phoxinus cumberlandensis")[["data"]]
gn_leaflet_basic(phox) %>%
    addMarkers(
      lng = phox_gbif$decimalLongitude,
      lat = phox_gbif$decimalLatitude,
      popup = ~paste0("<b>Date: ", as.Date(phox_gbif$eventDate), "</b><br>",
                      "<smaller><b>Locality:</b> ", phox_gbif$locality, "</smaller>",
                      "<br><b>Long & Lat:</b><br>",
                      phox_gbif$decimalLongitude,
                      ", ",
                      phox_gbif$decimalLatitude,
                      "<br><b style='color:red'>Coord. issue: ",
                      phox_gbif$issues, "</b><br>")
    )

abma <- filter(sp_place, species == "Abronia macrocarpa")
abma_gbif <- filter(tspp_occ_df, name == "Abronia macrocarpa")
gn_leaflet_basic(abma) %>%
    addMarkers(
      lng = abma_gbif$decimalLongitude,
      lat = abma_gbif$decimalLatitude,
      popup = ~paste0("<b>Date: ", as.Date(abma_gbif$eventDate), "</b><br>",
                      "<smaller><b>Locality:</b> ", abma_gbif$locality, "</smaller>",
                      "<br><b>Long & Lat:</b><br>",
                      abma_gbif$decimalLongitude,
                      ", ",
                      abma_gbif$decimalLatitude,
                      "<br><b style='color:red'>Coord. issue: ",
                      abma_gbif$issues, "</b><br>")
    )


make_mcp <- function(df) {
  pnts <- cbind(df$prim_long_dec,
                df$prim_lat_dec)
  print(head(pnts))
  mcp <- chull(pnts)
  coords <- pnts[c(mcp, mcp[1]), ]
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  return(sp_poly_df)
}

phox_mcp <- make_mcp(phox)
leaflet(phox_mcp) %>% addTiles() %>% addPolygons()

library(adehabitatHR)
xys <- SpatialPoints(
  data_frame(
    lon = phox$prim_long_dec,
    lat = phox$prim_lat_dec
  )
)
ph90 <- mcp(xys, percent = 90)
ph80 <- mcp(xys, percent = 80)


leaflet(phox_mcp) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addPolygons(color = "red", weight=1, fillOpacity = 0.1) %>%
  addPolygons(lng = ph90@polygons[[1]]@Polygons[[1]]@coords[,1],
              lat = ph90@polygons[[1]]@Polygons[[1]]@coords[,2],
              color = "blue", weight=1) %>%
  addPolygons(lng = ph80@polygons[[1]]@Polygons[[1]]@coords[,1],
              lat = ph80@polygons[[1]]@Polygons[[1]]@coords[,2],
              color = "gray", weight=1, fillOpacity = 0.3) %>%
  addCircleMarkers(
    lng = phox$prim_long_dec,
    lat = phox$prim_lat_dec,
    color = "gray",
    radius = 3
  )
