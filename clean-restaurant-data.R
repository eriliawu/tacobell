### clean up raw restaurant data exported from big purple

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

library(tools)
library(dplyr)
#install.packages("tidyr")
library(tidyr)

### read data ----
sample <- read.csv("data/from-bigpurple/restaurants.csv",
                   sep = ";", header = FALSE, nrow=10, quote = "")
head(sample)
sapply(sample, class)
rm(sample)

restaurants <- read.csv("data/from-bigpurple/restaurants.csv",
                  sep = ";", header = FALSE, quote = "\"'", stringsAsFactors = FALSE,
                  col.names = c("restid", "status_desc", "ownership", "kfc_ownership",
                                "tbc_ownership", "phi_ownership", "ljs_onwership",
                                "awr_ownership", "address1", "address2", "address3",
                                "city", "county", "state", "zip", "open", "temp_close",
                                "reopen", "close", "lat", "lon", "cencept",
                                "concept_begin", "concept_end", "drive_thru",
                                "drive_thru_type"))
sapply(restaurants, class)

### clean house ----
# convert dates, drop restaurants that closed before 2007-01-01
convert_to_date <- function(x) {
      # replace 0000-00-00 dates as NA
      x[x=="0000-00-00"] <- NA
      # convert characters to dates
      x <- as.Date(x)
      return(x)
      print(class(x))
}
restaurants[, c(16:19, 23:24)] <- lapply(restaurants[, c(16:19, 23:24)], convert_to_date)
rm(convert_to_date)
restaurants <- restaurants[restaurants$close>"2007-01-01"|is.na(restaurants$close), ]

# clean up random non-sense values
# ownership, address1, address2, county, lat, lon, drive_thru, drive_thru_type
# lon, lat
clean_up_signs <- function(x) {
      x[x==""|x=="\""|x=="\'"|x==" "] <- NA
      return(x)
}
restaurants[, c(3:14, 25:26)] <- sapply(restaurants[, c(3:14, 25:26)], clean_up_signs)
rm(clean_up_signs)
restaurants$lat[restaurants$lat==0.0000] <- NA
restaurants$lon[restaurants$lon==0.0000] <- NA

# drop restaurants with no open and close dates
restaurants$status_desc <- trimws(restaurants$status_desc, which="both")
restaurants <- subset(restaurants, !(is.na(restaurants$open)&is.na(restaurants$close)))

# clean up state column
table(restaurants$state)
restaurants$state <- trimws(restaurants$state, which="both")
restaurants$zip <- trimws(restaurants$zip, which="both")
table(nchar(restaurants$zip))
restaurants$zip[nchar(restaurants$zip)==0] <- NA
colnames(restaurants)[15] <- "longzip"
restaurants$zip <- substr(restaurants$longzip, 1, 5)

# clean up drive thru var
table(restaurants$drive_thru)
restaurants$drive_thru[restaurants$drive_thru==""] <- NA
restaurants$drive_thru[restaurants$drive_thru=="Y"] <- "1"
restaurants$drive_thru[restaurants$drive_thru=="N"] <- "0"
restaurants$drive_thru <- as.integer(restaurants$drive_thru)

# clean up address data
# keep only first letter capitalized
restaurants$address1 <- toTitleCase(tolower(trimws(restaurants$address1, which="both")))
restaurants$address2 <- toTitleCase(tolower(trimws(restaurants$address2, which="both")))
restaurants$address3 <- toTitleCase(tolower(trimws(restaurants$address3, which="both")))
restaurants$city <- toTitleCase(tolower(trimws(restaurants$city, which="both")))
restaurants$county <- toTitleCase(tolower(trimws(restaurants$county, which="both")))

restaurants$address1 <- gsub(x=restaurants$address1, pattern=" s ", replacement=" S ")
restaurants$address1 <- gsub(x=restaurants$address1, pattern=" n ", replacement=" N ")
restaurants$address1 <- gsub(x=restaurants$address1, pattern=" w ", replacement=" W ")
restaurants$address1 <- gsub(x=restaurants$address1, pattern=" e ", replacement=" E ")

# drop restaurants in overseas military bases
table(restaurants$status_desc)
table(restaurants$city[restaurants$status_desc=="UNKNOWN"])
restaurants <- restaurants[restaurants$status_desc!="UNKNOWN", ]
restaurants <- restaurants[restaurants$state!="CU", ]

# investigate status
# closed, dead site, erp, excess property, pre-open
#restaurants[restaurants$status_desc=="CLOSED", c(3, 9, 12, 14:15, 16, 19:22)]
#restaurants[restaurants$status_desc=="DEAD SITE", c(3, 9, 12, 14:15, 16, 19:22)]
#restaurants[restaurants$status_desc=="ERP", c(3, 9, 12, 14:15, 16, 19:22)]
#restaurants[restaurants$status_desc=="EXCESS PROPERTY", c(3, 9, 12, 14:15, 16, 19:22)]
#restaurants[restaurants$status_desc=="PRE-OPEN", c(3, 9, 12, 14:15, 16, 19:22)]

restaurants$address <- paste0(restaurants$address1, ", ", restaurants$city, ", ",
                              restaurants$state, " ", restaurants$zip)

# re-order columns
names(restaurants)
restaurants <- restaurants[order(restaurants$state, restaurants$address, 
                                 restaurants$open, restaurants$close),
                           c(1, 28, 14, 16:22, 25:26, 2:13, 15, 23:24, 27)]

### export address data for census bureau geocoder ----
address <- restaurants[, c("address1","city", "state", "zip")]
address <- address[!duplicated(address), ]
write.csv(address, "data/geocoding/address.csv")
rm(address)

coords <- read.csv("data/geocoding/GeocodeResults.csv",
                   stringsAsFactors = FALSE, header=FALSE, skip=0)
coords <- coords[coords$V3=="Match", c(2, 6, 9:12)]
colnames(coords)[1:6] <- c("address_match", "coords", "state_num", "county_num", "tract_num", "block_num")
coords$lon_bureau <- as.numeric(unlist(strsplit(coords$coords, ","))[seq(from=1, to=(2*dim(coords)[1]-1), by=2)])
coords$lat_bureau <- as.numeric(unlist(strsplit(coords$coords, ","))[seq(from=2, to=(2*dim(coords)[1]), by=2)])
coords$coords <- NULL

### merge census geocoding results ----
restaurants$address_match <- paste0(restaurants$address1, ", ", restaurants$city, ", ",
                              restaurants$state, ", ", restaurants$zip)
restaurants <- merge(restaurants, coords, by="address_match", all=TRUE)
names(restaurants)
restaurants$address_match <- NULL
rm(coords)

### examine duplicate addresses ----
address <- restaurants[, c("restid", "address", "open", "close", "lat", "lon",
                           "ownership", "drive_thru", "status_desc", "temp_close", "reopen")]
address <- address[address$address %in% address$address[duplicated(address$address)],]
address[address$status_desc=="DEAD SITE"&!(is.na(address$open)&!is.na(address$close)),] #dead sites can be dropped

# re-order address, by open/close date
address <- address[order(address$address, address$open, address$close), ]
address <- address[, -c(1, 7:9)]

# create id numbers correspondent to addresses
id <- data.frame(address[!duplicated(address$address), 1])
id$id <- seq(1, dim(id)[1], by=1)
colnames(id)[1] <- "address"
address <- merge(address, id, by="address", all=TRUE)
rm(id)

# create duplicate tags for each unique address
address <- address %>%
      group_by(address) %>%
      mutate(count=n()) %>%
      mutate(rank <- seq(1, count[1], 1))
address <- address[, -(8:9)]
colnames(address)[8] <- "dup"

address <- address %>%
      pivot_wider(names_from = dup, values_from = c(open, close, lat, lon, temp_close, reopen))
names(address)
address <- address[, c(1:2, 7, 22, 27,
                       3, 8, 23, 28, 
                       4, 9, 22, 29,
                       5, 10, 23, 30,
                       6, 11, 24, 31,
                       12:21)]

# merge restaurant level info
# create new restaurant id, each unique address is one row
id <- data.frame(unique(restaurants$address))
id$newid <- seq(1, dim(id)[1], by=1)
colnames(id)[1] <- "address"
restaurants <- merge(restaurants, id, by="address", all=TRUE)
rm(id)

# export to csv
write.csv(restaurants, "data/restaurants/analytic_restaurants.csv", row.names = FALSE)

# drop dead sites, drop ownership columns
#restaurants <- read.csv("data/restaurants/analytic_restaurants.csv", stringsAsFactors = FALSE)
restaurants <- restaurants[restaurants$status_desc!="DEAD SITE",
                           c("newid", "address", "state", "city", "open", "temp_close", "reopen", "close",
                             "lat", "lon", "state_num", "county_num", "tract_num",
                             "block_num", "lon_bureau", "lat_bureau")]
colnames(restaurants)[5:10] <- c("open_1", "temp_close_1", "reopen_1", "close_1",
                                 "lat_1", "lon_1")
restaurants <- merge(address, restaurants, by="address", all=TRUE)

# clean up duplicate columns after merge
restaurants$open_1.x[is.na(restaurants$open_1.x)] <- restaurants$open_1.y[is.na(restaurants$open_1.x)]
restaurants$close_1.x[is.na(restaurants$close_1.x)] <- restaurants$close_1.y[is.na(restaurants$close_1.x)]
restaurants$temp_close_1.x[is.na(restaurants$temp_close_1.x)] <- restaurants$temp_close_1.y[is.na(restaurants$temp_close_1.x)]
restaurants$reopen_1.x[is.na(restaurants$reopen_1.x)] <- restaurants$reopen_1.y[is.na(restaurants$reopen_1.x)]
restaurants$lat_1.x[is.na(restaurants$lat_1.x)] <- restaurants$lat_1.y[is.na(restaurants$lat_1.x)]
restaurants$lon_1.x[is.na(restaurants$lon_1.x)] <- restaurants$lon_1.y[is.na(restaurants$lon_1.x)]
names(restaurants)
restaurants <- restaurants[, -(35:40)]
colnames(restaurants)[c(2:5, 22, 27)] <- c("open_1", "close_1", "temo_close_1",
                                           "reopen_1", "lat_1", "lon_1")
restaurants$temp_close_1.x.1 <- NULL
restaurants$temp_close_2.1 <- NULL
restaurants <- restaurants[!duplicated(restaurants), ] #drop all dup rows
restaurants <- restaurants[, c(30, 1)]







