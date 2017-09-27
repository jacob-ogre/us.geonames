library(adehabitatHR)
library(dplyr)
library(ggplot2)
library(ggthemes)

make_mcp <- function(df) {
  if(dim(df)[1] > 5) {
    xys <- SpatialPoints(
      data_frame(
        lon = df$prim_long_dec,
        lat = df$prim_lat_dec
      )
    )
    xy90 <- mcp(xys, percent = 90)
    xy80 <- mcp(xys, percent = 80)
    xy70 <- mcp(xys, percent = 70)
    return(list(xy70 = xy70, xy80 = xy80, xy90 = xy90))
  } else {
    return(list(xy70 = NA, xy80 = NA, xy90 = NA))
  }
}

make_gbif_mcp <- function(df) {
  if(length(df$dec_longitude) >= 5) {
    xys <- try(
      SpatialPoints(
        data_frame(
          lon = df$dec_longitude,
          lat = df$dec_latitude
        )
      )
    )
    if(class(xys) == "try-error") {
      return(list(xy70 = NA, xy80 = NA, xy90 = NA))
    }
    xy90 <- try(mcp(xys, percent = 90))
    if(class(xy90) == "try-error") {
      xy90 <- Polygon(xys)
    }
    xy80 <- try(mcp(xys, percent = 80))
    if(class(xy80) == "try-error") {
      xy80 <- Polygon(xys)
    }
    xy70 <- try(mcp(xys, percent = 70))
    if(class(xy70) == "try-error") {
      xy70 <- Polygon(xys)
    }
    return(list(xy70 = xy70, xy80 = xy80, xy90 = xy90))
  } else {
    return(list(xy70 = NA, xy80 = NA, xy90 = NA))
  }
}

make_combo_mcp <- function(df) {
  if(dim(df)[1] > 5) {
    xys <- SpatialPoints(
      data_frame(lon = df$lon, lat = df$lat)
    )
    xy90 <- mcp(xys, percent = 90)
    xy80 <- mcp(xys, percent = 80)
    xy70 <- mcp(xys, percent = 70)
    return(list(xy70 = xy70, xy80 = xy80, xy90 = xy90))
  } else {
    return(list(xy70 = NA, xy80 = NA, xy90 = NA))
  }
}

get_area <- function(mcp) {
  if("area" %in% names(mcp)) {
    crs(mcp) <- CRS('+init=epsg:4326')
    raster::area(mcp) / 1000000
  } else {
    1
  }
}

proc_sp <- function(sp, df) {
  cur_df <- filter(df, species == sp)
  cur_n <- dim(cur_df)[1]
  cur_mcps <- make_mcp(cur_df)
  cur_areas <- lapply(cur_mcps, FUN = get_area) %>% unlist()
  cur_conf_levs <- names(cur_mcps)
  data_frame(
    species = sp,
    conf_level = cur_conf_levs,
    area = cur_areas,
    sample_size = cur_n,
    mcps = cur_mcps
  )
}

proc_sp_gbif <- function(sp, df) {
  cur_df <- filter(df, name == sp)
  cur_n <- dim(cur_df)[1]
  cur_mcps <- make_gbif_mcp(cur_df)
  cur_areas <- lapply(cur_mcps, FUN = get_area) %>% unlist()
  cur_conf_levs <- names(cur_mcps)
  data_frame(
    species = sp,
    taxon_key = rep(df$taxon_key[1], length(cur_conf_levs)),
    conf_level = cur_conf_levs,
    area = cur_areas,
    sample_size = cur_n,
    mcps = cur_mcps
  )
}

proc_combo_sp <- function(sp, df) {
  cur_df <- filter(df, species == sp)
  cur_n <- dim(cur_df)[1]
  cur_mcps <- make_combo_mcp(cur_df)
  cur_areas <- lapply(cur_mcps, FUN = get_area) %>% unlist()
  cur_conf_levs <- names(cur_mcps)
  data_frame(
    species = sp,
    conf_level = cur_conf_levs,
    area = cur_areas,
    sample_size = cur_n,
    mcps = cur_mcps
  )
}

# Process geoname processed species
spp <- unique(sp_place$species)
test1 <- lapply(spp[1:10], proc_sp, df = sp_place)
test1_df <- bind_rows(test1)

top100 <- lapply(spp[1:100], proc_sp, df = sp_place)
top100_df <- bind_rows(top100)
t100_xy70 <- filter(top100_df, conf_level == "xy70")
hist(t100_xy70$area, breaks = 21)

geoname_mcps <- lapply(spp, proc_sp, df = sp_place)
geoname_mcps_df <- bind_rows(geoname_mcps)
geoname_write <- dplyr::select(geoname_mcps_df, -mcps)
rio::export(geoname_write, file = "data/geoname_gb_gn_mcps.xlsx")

gnmcps_xy70 <- filter(geoname_mcps_df, conf_level == "xy70")
hist(gnmcps_xy70$area, breaks = 100)
g70_small <- filter(gnmcps_xy70, area < 50000)
hist(g70_small$area, breaks = 100)

spp_xref <- readRDS("data/gbif_ecos_xref.rds")
geoname_areas <- left_join(geoname_mcps_df, spp_xref, by = c("species" = "search"))

# Process GBIF
esa_gbif <- readRDS("data/ESA_spp_GBIF-2017-09-27.rds")
esa_gbif_coord <- filter(esa_gbif, !is.na(esa_gbif$dec_latitude))
gbif_spp <- unique(esa_gbif_coord$name)
gbif_key <- unique(esa_gbif_coord$taxon_key)
gbif_mcps <- lapply(gbif_spp, proc_sp_gbif, df = esa_gbif_coord)
gbif_mcps_df <- bind_rows(gbif_mcps)
gbif_write <- dplyr::select(gbif_mcps_df, -mcps)
rio::export(gbif_write, file = "data/gbif_gb_gn_mcps.xlsx")



gbmcps_xy70 <- filter(gbif_mcps_df, conf_level == "xy70")
hist(gbmcps_xy70$area, breaks = 100)

gb70_small <- filter(gbmcps_xy70, area < 50000)
hist(gb70_small$area, breaks = 100)

gbif_areas <- left_join(gbif_mcps_df, spp_xref, by = c("species" = "name"))

ac_gb <- filter(esa_gbif_coord, name == "Anaxyrus californicus") %>%
  dplyr::select(name, dec_latitude, dec_longitude)
ac_gn <- filter(sp_place, species == "Anaxyrus californicus") %>%
  dplyr::select(species, prim_lat_dec, prim_long_dec)
names(ac_gb) <- names(ac_gn) <- c("species", "lat", "lon")
ac <- rbind(ac_gb, ac_gn)

xys <- SpatialPoints(
  data_frame(
    lon = ac$lon,
    lat = ac$lat
  )
)
xy90 <- mcp(xys, percent = 90)
xy80 <- mcp(xys, percent = 80)
xy70 <- mcp(xys, percent = 70)

# Combined coord sets
gb_min <- dplyr::select(esa_gbif_coord, name, dec_latitude, dec_longitude)
gn_min <- dplyr::select(sp_place, species, prim_lat_dec, prim_long_dec)
names(gb_min) <- names(gn_min) <- c("species", "lat", "lon")
combo <- rbind(gb_min, gn_min)
combo_spp <- unique(combo$species)
combo_mcp <- lapply(combo_spp, proc_combo_sp, df = combo)
combo_mcp_df <- bind_rows(combo_mcp)
combo_write <- dplyr::select(combo_mcp_df, -mcps)
rio::export(combo_write, file = "data/combo_gb_gn_mcps.xlsx")

combo_mcp_70 <- filter(combo_mcp_df, conf_level == "xy70") %>%
  arrange(area)

combo_mcp_70_sm <- filter(combo_mcp_df, area < 5000)
qplot(combo_mcp_70_sm$area, geom = "histogram") +
  labs(x = "area (km^2)") +
  theme_hc()
hist(combo_mcp_70$area, breaks = 100)

synonyms <- filter(spp_df, search != species)
combo_mcp_70syn <- left_join(combo_mcp_70, synonyms, by = "species")

#
