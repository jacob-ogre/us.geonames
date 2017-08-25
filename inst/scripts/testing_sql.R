library(dplyr)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 dbname = "jacobmalcom",
                 host = "localhost", port = 5432,
                 user = "jacobmalcom", password = "")

part <- geonames.lite[1:10000, ]
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}
names(part) <- dbSafeNames(names(part))
dbWriteTable(con, "part", part, row.names = FALSE)
dbExistsTable(con, "part")

d2 <- dbGetQuery(con, "SELECT * FROM part LIMIT 5;")
head(d2)
d3 <- dbGetQuery(con, "SELECT * FROM part WHERE county_name = \'Cochise\';")

prt <- tbl(con, "part")
coch <- prt %>% dplyr::filter(county_name == 'Cochise') %>% as_data_frame()

db_drop_table(con, "part")

geonames <- geonames.lite
names(geonames) <- dbSafeNames(names(geonames))
dbWriteTable(con, "geonames", geonames, row.names = FALSE)
dbExistsTable(con, "geonames")

gnc <- tbl(con, "geonames")
gn <- gnc %>%
  dplyr::filter(state_alpha == "VA" & county_name == "Smyth") %>%
  as_data_frame()

db_create_index(con, "geonames", "state_alpha")
