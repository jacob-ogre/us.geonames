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

# con <- dbConnect(
#   dbDriver("PostgreSQL"),
#   dbname = "esc-dev",
#   user = "postgres",
#   password = "SillySilly1!",
#   host = "localhost"
# )


fyr <- dbSendQuery(con, "select * from ecos_fiveyr_review_docs") %>% dbFetch()
fyr$doc_file <- basename(fyr$doc_link) %>%
  str_replace("pdf$|PDF$", "txt")

rp <- dbSendQuery(con, "select * from ecos_recovery_plan_docs") %>% dbFetch()
rp$doc_file <- basename(rp$doc_link) %>%
  str_replace("pdf$|PDF$", "txt")

fr <- dbSendQuery(con, "select * from ecos_fed_reg_docs") %>% dbFetch()
fr$doc_file <- basename(fr$doc_link) %>%
  str_replace("pdf$|PDF$", "txt")

###########################
# Now geonames and NPS data
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
  # "~/Downloads/five_year_review",
  "~/Work/Data/ESAdocs_text/five_year_review",
  full.names = TRUE,
  recursive = TRUE
)

rp_fils <- list.files(
  "~/Work/Data/ESAdocs_text/recovery_plan",
  full.names = TRUE,
  recursive = TRUE
)

fr_fils <- list.files(
  "~/Work/Data/ESAdocs_text/federal_register",
  full.names = TRUE,
  recursive = TRUE
)

##########################################################################
# five-year reviews first
nps_res1 <- lapply(fiveyr_fils, fun = function(fil) {
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

#######################################################################
# recovery plans
nps_rp1 <- parallel::mclapply(
  rp_fils,
  mc.cores = 3,
  mc.preschedule = FALSE,
  FUN = function(fil) {
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

nps_rp2 <- bind_rows(nps_rp1)
nps_spp_rp <- left_join(nps_rp2, rp, by = "doc_file")
nps_spp_rp_filt <- filter(nps_spp_rp,
                       n_primary > 1 | (n_primary == 1 & n_secondary > 1))
table(nps_spp_rp_filt$feature_name) %>% sort(decreasing = TRUE) %>% head(10)
unique(nps_spp_rp_filt$feature_name) %>% length()
unique(nps_spp_rp_filt$species) %>% length()
nps_spp_rp_filt <- select(nps_spp_rp_filt, -plan_action_status, -plan_status)

#######################################################################
# Fed Reg
nps_fr1 <- parallel::mclapply(
  fr_fils,
  mc.cores = 3,
  mc.preschedule = FALSE,
  FUN = function(fil) {
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

nps_fr2 <- bind_rows(nps_fr1)
nps_spp_fr <- left_join(nps_fr2, fr, by = "doc_file")
nps_spp_fr_filt <- filter(nps_spp_fr,
                       n_primary > 1 | (n_primary == 1 & n_secondary > 1))
table(nps_spp_fr_filt$feature_name) %>% sort(decreasing = TRUE) %>% head(10)
unique(nps_spp_fr_filt$feature_name) %>% length()
unique(nps_spp_fr_filt$species) %>% length()
nps_spp_fr_filt <- select(nps_spp_fr_filt, -citation_page)

# Side-tracked
fyr_rp_combo <- rbind(nps_spp_filt, nps_spp_rp_filt)
table(fyr_rp_combo$feature_name) %>% sort(decreasing = TRUE) %>% head(10)
unique(fyr_rp_combo$feature_name) %>% length()
unique(fyr_rp_combo$species) %>% length()

sm_fyr_rp <- select(fyr_rp_combo, c(2,8:10,15:17,19,21:24)) %>%
  distinct(feature_name, species, .keep_all = TRUE)
rio::export(sm_fyr_rp, file = "~/Downloads/NPS_places.xlsx")

# After manual cleanup...
NPS_dat <- readxl::read_excel("~/Downloads/NPS_places_ESA.xlsx")
unique(NPS_dat$species) %>% length()
unique(NPS_dat$feature_name) %>% length()

# now back to adding hits from FR
sm_fr <- select(nps_spp_fr_filt, c(2,8:10,15:17,19,21:24)) %>%
  distinct(feature_name, species, .keep_all = TRUE)
fyr_rp_fr_combo <- rbind(NPS_dat, sm_fr)
fyr_rp_fr_filt <- filter(fyr_rp_fr_combo,
                         !grepl(fyr_rp_fr_combo$title,
                               pattern = "Review of Native Species") &
                         !grepl(fyr_rp_fr_combo$title,
                               pattern = "Listing 38 Species on Molokai"))

unique(fyr_rp_fr_filt$feature_name) %>% length()
unique(fyr_rp_fr_filt$species) %>% length()
rio::export(fyr_rp_fr_filt, "~/Downloads/NPS_fyr_rp_fr.xlsx")
