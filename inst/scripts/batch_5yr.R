library(dplyr)
library(parallel)
library(stringr)
library(us.geonames)

init()

files <- list.files("~/Downloads/five_year_review/",
                    full.names = TRUE,
                    recursive = TRUE)
files_df <- data_frame(
  file = files,
  doc = basename(files)
)

# Get 5y rev data table:
load("/Users/jacobmalcom/Downloads/ECOS_species_tables_2016-12-17.rda")
head(fiveyr_table)
fiveyr_table$doc <- basename(fiveyr_table$Doc_Link) %>%
  str_replace_all("pdf$", "txt")
fiveyr <- as_tibble(fiveyr_table)

# Get counties data and prep
load("/Users/jacobmalcom/Work/Data/esadocs/rda/counties_table_2016-12-10.rda")
names(counties_table)
states <- data_frame(state_alpha = state.abb, State = state.name)
cnt_occ <- as_tibble(left_join(counties_table, states, by = "State"))

run_gn_search <- function(file) {
  cat("\n\n")
  cat(paste("Processing:", file, "\n"))
  f2 <- file
  sub_dat <- filter(fiveyr, doc == file) %>% distinct(doc, .keep_all = TRUE)
  fil_dat <- filter(files_df, doc == f2)
  data <- left_join(fil_dat, sub_dat, by = "doc")
  # cat("data:\n")
  # print(dim(data))
  cos <- filter(cnt_occ, cnt_occ$Scientific_Name == data$Species)
  cos_ls <- vector("list", dim(cos)[1])
  for(i in 1:dim(cos)[1]) {
    cos_ls[[i]] <- c(cos$state_alpha[i], cos$County[i])
  }

  # search =======================
  text <- gn_read_txt(fil_dat$file)
  srch <- gn_search_county(text, cos_ls)
  if(dim(srch)[1] > 0) {
    srch$species <- data$Species
    srch$file <- file
    map <- us.geonames::gn_leaflet_basic(srch)
  } else {
    srch <- data_frame(
      "terms" = NA, "match" = NA, "feature_id" = NA, "feature_class" = NA,
      "state_alpha" = NA, "state_numeric" = NA, "county_name" = NA, 
      "county_numeric" = NA, "prim_lat_dec" = NA, "prim_long_dec" = NA, 
      "elev_in_m" = NA, "elev_in_ft" = NA, "map_name" = NA, 
      "date_created" = NA, "date_edited" = NA, "species" = NA, "file" = file
    )
    map <- NA
  }
  return(list(species = data$Species,
              found = srch,
              map = map,
              st_co = cos_ls,
              doc_nchar = nchar(text) ))
}

bind_all_res <- function(res_list) {
  dfs <- lapply(res_list, FUN = `[[`, 1)
  res <- bind_rows(dfs)
  return(res)
}

ts <- files_df$doc[21:25]
ot <- lapply(ts, FUN = run_gn_search)

ot_all <- bind_all_res(ot)

ts <- files_df$doc[26:50]
ot <- parallel::mclapply(
# ot <- lapply(
  ts, 
  FUN = run_gn_search,
  mc.cores = 3,
  mc.preschedule = FALSE
)

ot_all <- bind_all_res(ot)
brun <- ot[[19]]$found
brun_nat <- gn_natural_places(brun)
gn_leaflet_basic(brun_nat)

tally <- sort(table(brun$terms), decreasing = TRUE)
good <- tally[tally > 2]
brun_2 <- filter(brun, terms %in% names(good))
gn_leaflet_basic(brun_2)






