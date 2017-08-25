library(dplyr)
library(magrittr)

geonames <- read.table("~/Work/Data/geonames/NationalFile_20170601.txt",
                       header = TRUE,
                       sep = "|",
                       quote = "",
                       fill = TRUE,
                       stringsAsFactors = FALSE)
names(geonames)

geonames.lite <- geonames %>%
  select(
    -PRIMARY_LAT_DMS,
    -PRIM_LONG_DMS,
    -SOURCE_LAT_DMS,
    -SOURCE_LONG_DMS,
    -SOURCE_LAT_DEC,
    -SOURCE_LONG_DEC
  )

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

names(geonames.lite) <- dbSafeNames(names(geonames.lite))

# saveRDS(geonames.lite, "~/Work/Data/geonames/geonames_lite.rds")
# saveRDS(geonames, "~/Work/Data/geonames/geonames.rds")

devtools::use_data(geonames.lite, overwrite = TRUE)
# devtools::use_data(geonames, overwrite = TRUE)
