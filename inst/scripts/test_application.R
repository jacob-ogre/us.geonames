library(parallel)
library(us.geonames)

gn <- system.file("extdata", "geonames.lite.rda", package = "us.geonames")
gn
load(gn)

based <- "/home/jacobmalcom/Data/ESAdocs/candidate"
fils <- list.files(based, full.names = TRUE, recursive = TRUE)

txts <- sapply(
  fils[1:5],
  FUN = readLines,
  warn = FALSE
)

res <- parallel::mclapply(
  X = txts,
  FUN = us.geonames::gn_search_all
)