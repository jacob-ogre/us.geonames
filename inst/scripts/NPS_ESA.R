# NPS and ESA

library(dplyr)
library(RPostgreSQL)
library(readr)
library(secret)
library(stringr)
library(us.geonames)

##############################################################
# Connections
con <- dbConnect(
  dbDriver("PostgreSQL"),
  dbname = "postgres",
  user = ESC_DB_USR,
  password = get_secret(ESC_DB_PW, key = priv_key, vault),
  host = ESC_DB
)

fyr <- dbSendQuery(con, "select * from ecos_fiveyr_review_docs") %>% dbFetch()
fyr$doc_file <- basename(fyr$doc_link) %>%
  str_replace("pdf$|PDF$", "txt")

data(geonames.lite, package = "us.geonames")
nps <- read_csv("data/nps_boundary.csv")

nps_gn <- filter(geonames.lite, feature_name %in% nps$UNIT_NAME) %>%
  filter(feature_name != "White House")

nps_gn$base_name <- str_replace_all(
  nps_gn$feature_name,
  pattern = " National Historic Site| National Park| National Historic(al)* Park| National Monument| Memorial| National Preserve| Wild and Scenic River| National Seashore| National Lakeshore| National Recreation Area| National Scenic River",
  replacement = ""
)

get_nps_places <- function(text) {
  cur_set <- nps_gn
  cur_set$n_primary <- stringi::stri_count_fixed(
    str = text,
    pattern = nps_gn$feature_name
  )
  cur_set$n_secondary <- stringi::stri_count_fixed(
    str = text,
    pattern = nps_gn$base_name
  )
  hits <- dplyr::filter(cur_set, n_primary > 0)
  return(hits)
}

fiveyr_fils <- list.files(
  "~/Downloads/five_year_review",
  full.names = TRUE,
  recursive = TRUE
)

# nps_res1 <- lapply(fiveyr_fils[1:100], FUN = function(fil) {
nps_res1 <- lapply(fiveyr_fils, FUN = function(fil) {
  txt <- gn_read_txt(fil)
  n_words <- str_split(txt, " ") %>% unlist() %>% length()
  res <- get_nps_places(txt)
  if(dim(res)[1] > 0) {
    res$doc_path <- fil
    res$doc_file <- basename(fil)
    res$doc_length <- n_words
  }
  return(res)
})

nps_res2 <- bind_rows(nps_res1)
nps_spp_fyr <- left_join(nps_res2, fyr, by = "doc_file")
nps_spp_filt <- filter(nps_spp_fyr,
                       n_primary > 1 | (n_primary == 1 & n_secondary > 1))
table(nps_spp_filt$feature_name) %>% sort(decreasing = TRUE) %>% head(10)
unique(nps_spp_filt$feature_name) %>% length()
unique(nps_spp_filt$species) %>% length()




