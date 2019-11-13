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
library(ggplot2)
#install.packages("stringr")
library(stringr)

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
restaurants <- read.csv("data/restaurants/analytic_restaurants.csv", stringsAsFactors = FALSE)
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
restaurants <- restaurants[, c(30, 1, 31:32, 2:3, 20, 25, 4:19, 21:24, 26:29, 33:38)]
rm(address)

### merge restaurant with transaction data ----
temp_address <- read.csv("data/restaurants/analytic_restaurants.csv",
                         stringsAsFactors = FALSE)
names(temp_address)
temp_address <- temp_address[, c(1:2, 35)]

# clean up transaction data
transaction <- NULL
for (i in c(2007:2015)) {
      for (j in c(1:4)) {
            if (i==2015 & j==4) {
                  next
            } else {
                  transaction_temp <- read.csv(paste0("data/from-bigpurple/transaction-by-restaurant/by_restaurant_transaction_", i, "q", j, ".csv"),
                                               stringsAsFactors = FALSE, sep = ";", header = FALSE, quote = "\"'",
                                               col.names = c("restid", "volume", "dollar"))
                  #print(sapply(transaction_temp, class))
                  transaction_temp$year <- i
                  transaction_temp$quarter <- paste0("Q", j)
                  transaction <- rbind(transaction, transaction_temp)
            }
      }
}
rm(i, j, transaction_temp)

transaction <- merge(transaction, temp_address, by="restid")
rm(temp_address)

# sum volume and dollar by address
transaction <- aggregate(cbind(volume, dollar) ~ year+quarter+newid, transaction, sum)
transaction <- merge(transaction, restaurants, by="newid")
names(transaction)
transaction <- transaction[order(transaction$newid, transaction$year, transaction$quarter), ]
write.csv(transaction, "data/restaurants/transaction-by-address.csv", row.names = FALSE)

### by-address analysis, by region ----
#transaction <- read.csv("data/restaurants/transaction-by-address.csv", stringsAsFactors = FALSE)

# standardize sales by number of weeks in a quarter
# 12 weeks for quarters 1-3, 16 weeks for quarter 4
transaction$volume_std <- ifelse(transaction$quarter=="Q4",
                                 transaction$volume/16, transaction$volume/12)
transaction$dollar_std <- ifelse(transaction$quarter=="Q4",
                                 transaction$dollar/16, transaction$dollar/12)

# create mean spending per order
transaction$mean_spending <- transaction$dollar / transaction$volume

# create region code for all states
transaction$region[transaction$state=="NH"|transaction$state=="VT"|transaction$state=="MA"|transaction$state=="ME"|transaction$state=="RI"|transaction$state=="CT"|transaction$state=="NJ"|transaction$state=="PA"|transaction$state=="NY"] <- "northeast"
transaction$region[transaction$state=="ND"|transaction$state=="SD"|transaction$state=="NE"|transaction$state=="KS"|transaction$state=="MN"|transaction$state=="IA"|transaction$state=="MO"|transaction$state=="WI"|transaction$state=="IL"|transaction$state=="MI"|transaction$state=="IN"|transaction$state=="OH"] <- "midwest"
transaction$region[transaction$state=="DE"|transaction$state=="DC"|transaction$state=="MD"|transaction$state=="OK"|transaction$state=="TX"|transaction$state=="AR"|transaction$state=="LA"|transaction$state=="MS"|transaction$state=="KY"|transaction$state=="TN"|transaction$state=="AL"|transaction$state=="WV"|transaction$state=="GA"|transaction$state=="VA"|transaction$state=="NC"|transaction$state=="SC"|transaction$state=="FL"] <- "south"
transaction$region[transaction$state=="AK"|transaction$state=="HI"|transaction$state=="WA"|transaction$state=="OR"|transaction$state=="CA"|transaction$state=="ID"|transaction$state=="NV"|transaction$state=="MT"|transaction$state=="WY"|transaction$state=="UT"|transaction$state=="AZ"|transaction$state=="CO"|transaction$state=="NM"] <- "west"

# make column quarter numeric
transaction$quarter <- as.integer(substr(transaction$quarter, 2, 2))

# sales by region
#qplot(x=volume_std, data=transaction, color=as.factor(region))
ggplot(data=subset(transaction, year==2015&quarter==2),
            aes(x=volume_std, group=as.factor(region), fill=as.factor(region))) +
      geom_histogram(bins=200) +
      labs(title="Number of transactions, 2015 Q2",
           x="Number of transactions, standardized by week", y="Frequency", fill="Region",
           caption="Data source: Taco Bell") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/by-restaurant-transaction/num-transaction-2015Q2.jpeg", width=20, height=10, unit="cm")

# mean spending
ggplot(data=subset(transaction, year==2015&quarter==2),
       aes(x=mean_spending, group=as.factor(region), fill=as.factor(region))) +
      geom_histogram(bins=200) +
      labs(title="Mean spending per order, 2015 Q2",
           x="Spending ($)", y="Frequency", fill="Region",
           caption="Data source: Taco Bell") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/by-restaurant-transaction/mean-spending-2015Q2.jpeg", width=20, height=10, unit="cm")

# look at numbers by region, state, county, tract
region <- aggregate(cbind(volume_std, dollar_std) ~ year+quarter+region, transaction, sum)
region$mean_spending <- region$dollar_std/region$volume_std
count <- aggregate(address~year+quarter+region, transaction, length)
colnames(count)[4] <- "n"
region <- merge(region, count, by=c("year", "quarter", "region"))
rm(count)

state <- aggregate(cbind(volume_std, dollar_std) ~ year+quarter+state, transaction, sum)
county <- aggregate(cbind(volume_std, dollar_std) ~ year+quarter+state+county_num, transaction, sum)

tract <- aggregate(cbind(volume_std, dollar_std) ~ year+quarter+state+county_num+tract_num, transaction, sum)
tract$mean_spending <- tract$dollar_std/tract$volume_std
count <- aggregate(address~year+quarter+state+county_num+tract_num, transaction, length)
colnames(count)[6] <- "n"
tract <- merge(tract, count, by=c("year", "quarter", "state", "county_num", "tract_num"))
rm(count)

# num of transactions by region, over time
ggplot(data=region, aes(x=paste(year, "Q", quarter, sep=""), y=volume_std,
                       group=as.factor(region), col=as.factor(region))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Number of transactions, by region",
           x="Year", y="Number of transactions, standardized by week", col="Region",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/by-restaurant-transaction/num-transactions-by-region.jpeg", width=20, height=10, unit="cm")

# mean spending by region, over time
ggplot(data=region, aes(x=paste(year, "Q", quarter, sep=""), y=mean_spending,
                        group=as.factor(region), col=as.factor(region))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean spending per order, by region",
           x="Year", y="Spending ($)", col="Region",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/by-restaurant-transaction/mean-spending-by-region.jpeg", width=20, height=10, unit="cm")

# num of restaurants by region, over time
ggplot(data=region, aes(x=paste(year, "Q", quarter, sep=""), y=n,
                        group=as.factor(region), col=as.factor(region))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Number of restaurants, by region",
           x="Year", y="Number of restaurants", col="Region",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/by-restaurant-transaction/num-restaurants-by-region.jpeg", width=20, height=10, unit="cm")

# export lon/lat coordinates for geocode census county and tract numbers ----
# in ArcGIS
# county
geocode <- restaurants[, c(1:2, 7:8)]
geocode <- geocode[!duplicated(geocode), ]
colnames(geocode)[3:4] <- c("lat", "lon")
write.csv(geocode, "data/geocoding/geocoding-county.csv", row.names = FALSE)

geocode <- read.csv("data/geocoding/geocoding-county_results.csv", stringsAsFactors = FALSE)
names(restaurants)
restaurants <- restaurants[, -c(33:38)]
names(geocode)
geocode<- geocode[, c(4, 11, 8)]
colnames(geocode)[3:4] <- c("county_num", "state_num")
restaurants <- merge(restaurants, geocode, by="address", all=TRUE)
sum(is.na(restaurants$address))
rm(geocode)
write.csv(restaurants, row.names = FALSE,
          "data/restaurants/unique-address-w-county_num.csv")

# tracts
geocode <- read.csv("data/geocoding/geocoding-tract_results.csv", stringsAsFactors = FALSE)
names(geocode)
geocode <- geocode[, c(3:4, 8:12)]
colnames(geocode)[3:7] <- c("state_num", "county_num", "short_tract_num", "tract_num", "tract_name")
geocode$tract_num <- as.character(geocode$tract_num)
restaurants <- merge(restaurants, geocode, by="address", all=TRUE)
write.csv(restaurants, row.names = FALSE,
          "data/restaurants/unique-address-w-tract_num.csv")
rm(geocode)

### by address analysis, by county ----
temp_address <- read.csv("data/restaurants/analytic_restaurants.csv",
                         stringsAsFactors = FALSE)
names(temp_address)
temp_address <- temp_address[, c(1:2, 35)]

# clean up transaction data
transaction <- NULL
for (i in c(2007:2015)) {
      for (j in c(1:4)) {
            if (i==2015 & j==4) {
                  next
            } else {
                  transaction_temp <- read.csv(paste0("data/from-bigpurple/transaction-by-restaurant/by_restaurant_transaction_", i, "q", j, ".csv"),
                                               stringsAsFactors = FALSE, sep = ";", header = FALSE, quote = "\"'",
                                               col.names = c("restid", "volume", "dollar"))
                  #print(sapply(transaction_temp, class))
                  transaction_temp$year <- i
                  transaction_temp$quarter <- paste0("Q", j)
                  transaction <- rbind(transaction, transaction_temp)
            }
      }
}
rm(i, j, transaction_temp)
transaction <- transaction[, -c(1, 6)]

transaction <- merge(restaurants, transaction, by="newid", all=TRUE)
names(transaction)
transaction <- transaction[, c(1:3, 33:38)]
transaction <- transaction[, c(1:2, 8:9, 6:7, 3:5)]
transaction$quarter <- as.integer(substr(transaction$quarter, 2, 2))
transaction <- transaction[order(transaction$newid, transaction$year, transaction$quarter), ]

# aggregate to county level
county <- aggregate(cbind(volume, dollar) ~ year+quarter+county_num, transaction, sum)
county <- county[order(county$county_num, county$year, county$quarter), ] 
county$volume_std <- ifelse(county$quarter==4, county$volume/16, county$volume/12)
county$dollar_std <- ifelse(county$quarter==4, county$dollar/16, county$dollar/12)
county$mean_spending <- county$dollar_std/county$volume_std

### by address analysis, by census tract ----
temp_address <- read.csv("data/restaurants/analytic_restaurants.csv",
                         stringsAsFactors = FALSE)
names(temp_address)
temp_address <- temp_address[, c(1:2, 35)]

# clean up transaction data
transaction <- NULL
for (i in c(2007:2015)) {
      for (j in c(1:4)) {
            if (i==2015 & j==4) {
                  next
            } else {
                  transaction_temp <- read.csv(paste0("data/from-bigpurple/transaction-by-restaurant/by_restaurant_transaction_", i, "q", j, ".csv"),
                                               stringsAsFactors = FALSE, sep = ";", header = FALSE, quote = "\"'",
                                               col.names = c("restid", "volume", "dollar"))
                  #print(sapply(transaction_temp, class))
                  transaction_temp$year <- i
                  transaction_temp$quarter <- paste0("Q", j)
                  transaction <- rbind(transaction, transaction_temp)
            }
      }
}
rm(i, j, transaction_temp)

transaction <- merge(transaction, temp_address, by="restid", all=TRUE)
transaction$restid <- NULL
transaction <- aggregate(cbind(volume, dollar) ~ year+quarter+address, transaction, sum)
rm(temp_address)

transaction <- merge(restaurants, transaction, by="address", all=TRUE)
names(transaction)
transaction <- transaction[, c(1:4, 33:39)]
transaction$quarter <- as.integer(substr(transaction$quarter, 2, 2))
transaction <- transaction[order(transaction$newid, transaction$year, transaction$quarter), ]

# aggregate to tract level
tract <- aggregate(cbind(volume, dollar) ~ year+quarter+tract_num, transaction, sum)
tract <- tract[order(tract$tract_num, tract$year, tract$quarter), ] 
tract$volume_std <- ifelse(tract$quarter==4, tract$volume/16, tract$volume/12)
tract$dollar_std <- ifelse(tract$quarter==4, tract$dollar/16, tract$dollar/12)
tract$mean_spending <- tract$dollar_std/tract$volume_std
tract <- tract[, -c(4:5)]

### clean up income and race data ----
# from IPUMS, year 2015, 5-year ACS data
income <- read.csv("data/census-data/tract/nhgis0003_income_race_ethnicity/nhgis0003_ds215_20155_2015_tract.csv",
                   stringsAsFactors = FALSE)
names(income)
income <- income[, c(1, 6, 8, 11, 37, 59, 38, 40:41, 43, 49)]
colnames(income)[1:11] <- c("tract_num", "state_num", "county_num", "short_tract_num",
                           "tract_name", "income", "pop", 
                           "white", "black", "asian", "hisp")

# fix census tract number
# format: 2-digit state num, 3-digit county num, 6-digit tract num
income$state_num <- str_pad(as.character(income$state_num), 2, side="left", pad="0")
income$county_num <- str_pad(as.character(income$county_num), 3, side="left", pad="0")
income$short_tract_num <- str_pad(as.character(income$short_tract_num), 6, side="left", pad="0")
income$tract_num <- paste0(income$state_num, income$county_num, income$short_tract_num)
income <- income[, -c(2:5)]
tract$tract_num <- str_pad(as.character(tract$tract_num), 11, side="left", pad="0")

# calculate income quintiles and other indices for ethnicity
breaks <- quantile(income$income, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
income$income5 <- cut(income$income, breaks=breaks, labels=1:5, include.lowest=TRUE)

# make ethnicity percetnage
income$white <- income$white/income$pop
income$black <- income$black/income$pop
income$asian <- income$asian/income$pop
income$hisp <- income$hisp/income$pop
breaks <- quantile(income$white, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
income$white5 <- cut(income$white, breaks=breaks, labels=1:5, include.lowest=TRUE)

tract <- merge(tract, income, by="tract_num", all=TRUE)
rm(income)
tract <- tract[order(tract$tract_num, tract$year, tract$quarter), ]
tract <- tract[!is.na(tract$year), ]

# merge ruca index from USDA
ruca <- read.csv("data/census-data/tract/RUCA_USDA_2010.csv", stringsAsFactors = FALSE)
ruca$tract_num <- str_pad(as.character(ruca$tract_num), 11, side="left", pad="0")
test <- merge(ruca, tract, by="tract_num")







# count number of restaurants that ever existed in a census tract
number <- restaurants[, c(1, 35)]
number$tract_num <- str_pad(as.character(number$tract_num), 9, side="left", pad="0")
number <- aggregate(address~tract_num, data=number, FUN=length)
tract <- merge(tract, number, by="tract_num", all=TRUE)
names(tract)
colnames(tract)[13] <- "n"
tract <- tract[, c(1, 13, 2:12)]
rm(number)
tract <- tract[order(tract$tract_num, tract$year, tract$quarter), ]

tract$n <- NULL



