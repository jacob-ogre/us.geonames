library(dplyr)
library(ecosscraper)
library(rgbif)
library(stringr)

TECP_table <- get_TECP_table()
dom <- filter_domestic(TECP_table) %>%
  filter_listed()
names(dom)[1] <- "species"
head(dom$species, 15)

try_taxon_keys <- function(sp) {
  # Sys.sleep(runif(1, 0, 1))
  orig <- sp
  if(grepl(sp, pattern = "\\(")) {
    sp <- str_replace(sp, "\\([A-Za-z0-9 .,=]+\\) ", "")
  }
  sp <- str_replace(sp, "\\.", "")
  t1 <- name_backbone(sp)
  if(length(t1) < 15) {
    t1 <- list(
      usageKey = NA,
      speciesKey = NA,
      canonicalName = sp,
      species = NA,
      confidence = NA)
  }
  if(!("species" %in% names(t1))) {
    t1$species <- NA
  }
  if(!("speciesKey" %in% names(t1))) {
    t1$speciesKey <- NA
  }
  t1$search_term <- orig
  return(t1)
}

spp <- unique(dom$species)
spp_keys <- sapply(spp, try_taxon_keys)
spp_srch <- lapply(spp_keys, `[[`, "search_term") %>% unlist()
spp_use_key <- lapply(spp_keys, `[[`, "usageKey") %>% unlist()
spp_species_key <- lapply(spp_keys, `[[`, "speciesKey") %>% unlist()
spp_canon <- lapply(spp_keys, `[[`, "canonicalName") %>% unlist()
spp_species <- lapply(spp_keys, `[[`, "species") %>% unlist()
spp_conf <- lapply(spp_keys, `[[`, "confidence") %>% unlist()
spp_df <- data_frame(
  search = spp_srch,
  usage_key = spp_use_key,
  species_key = spp_species_key,
  canonical_name = spp_canon,
  species = spp_species,
  confidence = spp_conf
)
saveRDS(spp_df, file = "data/gbif_ecos_xref.rds")
spp_d2 <- filter(spp_df, !is.na(species_key))
spp_na <- filter(spp_df, is.na(species_key))
spp_na_2 <- filter(spp_df, is.na(usage_key))

spp_ls <- split(spp_d2$usage_key, ceiling(seq_along(spp_d2$usage_key)/30))
# spp_occ <- occ_data(taxonKey = spp_d2$key)
test1 <- lapply(spp_ls[1:2], function(x) {
  Sys.sleep(0.5)
  occ_data(taxonKey = x)
})
t2 <- unlist(test1, recursive = F)
t2_data <- lapply(t2, `[[`, "data")
t2_df <- bind_rows(t2_data)
t2_name <- lapply(t2_data, `[[`, "name") %>% unlist()
t2_key <- lapply(t2_data, `[[`, "key") %>% unlist()

remain <- lapply(spp_ls[3:length(spp_ls)], function(x) {
  Sys.sleep(runif(1, 0, 2))
  occ_data(taxonKey = x)
})
remain_ul <- unlist(remain, recursive = F)
esa_occ_ls <- c(remain_ul, t2)
esa_occ <- lapply(esa_occ_ls, `[[`, "data")

lap_var <- function(x, var) {
  if(var %in% names(x)) {
    x[[var]]
  } else {
    rep(NA, length(x$name))
  }
}

name <- lapply(esa_occ, `[[`, "name") %>% unlist()
taxon_key <- lapply(esa_occ, `[[`, "taxonKey") %>% unlist()
dataset_key <- lapply(esa_occ, lap_var, var = "datasetKey") %>% unlist()
gbif_id <- lapply(esa_occ, lap_var, var = "gbifID") %>% unlist()
dec_latitude <- lapply(esa_occ, lap_var, var = "decimalLatitude") %>% unlist()
dec_longitude <- lapply(esa_occ, lap_var, var = "decimalLongitude") %>% unlist()
issues <- lapply(esa_occ, lap_var, var = "issues") %>% unlist()
locality <- lapply(esa_occ, lap_var, var = "locality") %>% unlist()
country_code <- lapply(esa_occ, lap_var, var = "countryCode") %>% unlist()
country <- lapply(esa_occ, lap_var, var = "country") %>% unlist()
date <- lapply(esa_occ, lap_var, var = "eventDate") %>% unlist()
date <- as.Date(date)
basis_record <- lapply(esa_occ, lap_var, var = "basisOfRecord") %>% unlist()
license <- lapply(esa_occ, lap_var, var = "license") %>% unlist()
info_withheld <- lapply(esa_occ, lap_var, var = "informationWithheld") %>% unlist()

esa_gbif <- data_frame(
  name,
  taxon_key,
  issues,
  dataset_key,
  gbif_id,
  dec_latitude,
  dec_longitude,
  locality,
  country_code,
  country,
  date,
  basis_record,
  license,
  info_withheld
)

saveRDS(esa_gbif, file = "~/Downloads/ESA_spp_GBIF-2017-09-27.rds")
rm(list=c("basis_record", "dataset_key", "gbif_id"))
rm(list=c("date", "dec_latitude", "dec_longitude", "info_withheld", "issues",
          "license", "locality", "name", "taxon_key", "country_code", "country"))

esa_gbif_coord <- filter(esa_gbif, !is.na(dec_longitude))
length(unique(esa_gbif_coord$name))
table(esa_gbif_coord$name) %>% sort(decreasing = TRUE) %>% head(50)
table(esa_gbif_coord$name) %>% sort() %>% head(50)

dom_rec <- filter(esa_gbif, country_code %in% c("US", "MX", "CA"))
dom_rec_coord <- filter(esa_gbif, !is.na(dec_latitude))
table(dom_rec_coord$name) %>% sort(decreasing = TRUE) %>% head(50)
KIWA <- filter(dom_rec_coord, name == "Setophaga kirtlandii")

library(leaflet)
leaflet(data = KIWA) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(
    lng = KIWA$dec_longitude,
    lat = KIWA$dec_latitude,
    popup = ~paste0("<b>Date: ", as.Date(KIWA$date), "</b><br>",
                    "<smaller><b>Locality:</b> ", KIWA$locality, "</smaller>",
                    "<br><b>Long & Lat:</b><br>",
                    KIWA$dec_longitude,
                    ", ",
                    KIWA$dec_latitude,
                    "<br><b style='color:red'>Coord. issue: ",
                    issues, "</b><br>")
  )

table(KIWA$issues)
KIWA_rnd <- filter(KIWA, issues != "")
KIWA_nornd <- filter(KIWA, issues == "")
leaflet(data = KIWA_nornd) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addMarkers(
    lng = KIWA_nornd$dec_longitude,
    lat = KIWA_nornd$dec_latitude,
    popup = ~paste0("<b>Date: ", as.Date(KIWA_nornd$date), "</b><br>",
                    "<smaller><b>Locality:</b> ", KIWA_nornd$locality, "</smaller>",
                    "<br><b>Long & Lat:</b><br>",
                    KIWA_nornd$dec_longitude,
                    ", ",
                    KIWA_nornd$dec_latitude,
                    "<br><b style='color:red'>Coord. issue: ",
                    issues, "</b><br>")
  )


BEVI <- filter(dom_rec_coord, grepl(dom_rec_coord$name, pattern = "Vireo bellii"))
