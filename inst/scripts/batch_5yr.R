library(dplyr)
library(ggplot2)
library(parallel)
library(plotly)
library(stringr)
library(us.geonames)

init()
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
  # dfs <- lapply(res_list, FUN = `[[`, 1)
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

