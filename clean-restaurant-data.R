### clean up raw restaurant data exported from big purple

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load libraries ----
library(tools)
library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(ggplot2)
#install.packages("stringr")
library(stringr)
library(tidyverse)
library(maps)

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
                                "reopen", "close", "lat", "lon", "concept",
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

### examine duplicate addresses ----
address <- restaurants[, c("restid", "address", "open", "close", "lat", "lon",
                           "ownership", "drive_thru", "status_desc", "temp_close", "reopen")]
#address <- address[address$address %in% address$address[duplicated(address$address)],]
address[address$status_desc=="DEAD SITE"&!(is.na(address$open)&!is.na(address$close)),] #dead sites can be dropped

# re-order address, by open/close date
address <- address[order(address$address, address$open, address$close), ]
address <- address[, -c(1, 7:9)]

# create duplicate tags for each unique address
address <- address %>%
      group_by(address) %>%
      mutate(count=n()) %>%
      mutate(rank <- seq(1, count[1], 1))
colnames(address)[9] <- "dup"
address$count <- NULL

address <- address %>%
      pivot_wider(names_from = dup, values_from = c(open, close, lat, lon, temp_close, reopen))
names(address)

# drop dead sites and export restaurant data to csv
restaurants  <- restaurants[restaurants$status_desc!="DEAD SITE", ]
#write.csv(restaurants, "data/restaurants/analytic_restaurants.csv", row.names = FALSE)
#write.csv(address, "data/restaurants/analytic_address.csv", row.names = FALSE)

### merge restaurant with transaction data ----
temp <- read.csv("data/restaurants/analytic_restaurants.csv",
                         stringsAsFactors = FALSE)
names(temp)
temp <- temp[, c(1:2)]

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

transaction <- merge(transaction, temp, by="restid")
rm(temp)

# sum volume and dollar by address
transaction <- aggregate(cbind(volume, dollar) ~ year+quarter+address, transaction, sum)
names(transaction)
transaction <- transaction[order(transaction$address, transaction$year, transaction$quarter), ]

# merge with other restaurant info
transaction <- merge(transaction, address, by="address", all=TRUE)
names(transaction)
transaction <- transaction[, -c()]
temp <- restaurants[, c(2:3, 28)]
temp <- temp[!duplicated(temp$address), ]
transaction <- merge(transaction, temp, by="address", all=TRUE)
rm(temp)
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
      labs(title="Number of transactions per restaurant, 2015 Q2",
           x="Number of transactions, standardized by week",
           y="Number of restaurants", fill="Region",
           caption="Data source: Taco Bell") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/by-restaurant-transaction/num-transaction-2015Q2.jpeg", width=20, height=10, unit="cm")

# look at numbers by region, state, county, tract
region <- aggregate(cbind(volume_std, dollar_std) ~ year+quarter+region, transaction, sum)
region$mean_spending <- region$dollar_std/region$volume_std
count <- aggregate(address~year+quarter+region, transaction, length)
colnames(count)[4] <- "n"
region <- merge(region, count, by=c("year", "quarter", "region"))
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

rm(region)

### export lon/lat coordinates for geocode census county and tract numbers ----
# in ArcGIS
# county
geocode <- restaurants[, c(1:2, 7:8)]
geocode <- geocode[!duplicated(geocode), ]
colnames(geocode)[3:4] <- c("lat", "lon")
write.csv(geocode, "data/geocoding/geocoding-tract.csv", row.names = FALSE)

### merge geocode results ----
# tracts
geocode <- read.csv("data/geocoding/geocoding-tract_results.csv", stringsAsFactors = FALSE)
names(geocode)
geocode <- geocode[, c(3:4, 8:12)]
colnames(geocode)[3:7] <- c("state_num", "county_num", "short_tract_num", "tract_num", "tract_name")
geocode$tract_num <- paste0(str_pad(as.character(geocode$state_num), 2, side="left", pad="0"),
                            str_pad(as.character(geocode$county_num), 3, side="left", pad="0"),
                            str_pad(as.character(geocode$short_tract_num), 6, side="left", pad="0"))

address <- merge(address, geocode, by="address", all=TRUE)
write.csv(restaurants, row.names = FALSE,
          "data/restaurants/unique-address-w-tract_num.csv")
transaction <- merge(transaction, geocode, by="address", all=TRUE)
transaction$tract_name <- NULL
rm(geocode)
write.csv(transaction, row.names = FALSE,
          "data/restaurants/transaction-by-address-with-tract-num.csv")

# aggregate to tract level
#transaction <- read.csv("data/restaurants/transaction-by-address-with-tract-num.csv",
#                       stringsAsFactors = FALSE)
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
#tract <- tract[!is.na(tract$year), ]

# merge ruca index from USDA
ruca <- read.csv("data/census-data/tract/RUCA_USDA_2010.csv", stringsAsFactors = FALSE)
ruca$tract_num <- str_pad(as.character(ruca$tract_num), 11, side="left", pad="0")
tract <- merge(ruca, tract, by="tract_num", all=TRUE)
rm(ruca, breaks)

# count number of restaurants that ever existed in a census tract
number <- address[, c(1, 36)]
number <- aggregate(address~tract_num, data=number, FUN=length)
summary(number$address)
tract <- merge(tract, number, by="tract_num", all=TRUE)
names(tract)
colnames(tract)[17] <- "n"
tract <- tract[, c(1, 17, 2:16)]
rm(number)
tract <- tract[order(tract$tract_num, tract$year, tract$quarter), ]

tract$pop_density <- as.numeric(gsub(pattern = ",", tract$pop_density, replacement = ""))

### by-tract analysis ----
tract$n[is.na(tract$n)] <- 0

# median income
length(unique(tract$tract_num[tract$n==0]))
summary(tract$income[tract$n==0])
summary(tract$white[tract$n==0])
summary(tract$asian[tract$n==0])
summary(tract$black[tract$n==0])
summary(tract$hisp[tract$n==0])
summary(tract$pop_density[tract$n==0])

unique_tract <- tract[!duplicated(tract$tract_num), c(1:4, 10:17)]
summary(unique_tract$n[unique_tract$n==0])
summary(unique_tract$income[unique_tract$n==0])
summary(unique_tract$white[unique_tract$n==0])
summary(unique_tract$asian[unique_tract$n==0])
summary(unique_tract$black[unique_tract$n==0])
summary(unique_tract$hisp[unique_tract$n==0])
summary(unique_tract$pop_density[unique_tract$n==0])

summary(unique_tract$n[unique_tract$n==1])
summary(unique_tract$income[unique_tract$n==1])
summary(unique_tract$white[unique_tract$n==1])
summary(unique_tract$asian[unique_tract$n==1])
summary(unique_tract$black[unique_tract$n==1])
summary(unique_tract$hisp[unique_tract$n==1])
summary(unique_tract$pop_density[unique_tract$n==1])

tract_tb <- tract[tract$n!=0, ]
income <- aggregate(cbind(volume_std, dollar_std, mean_spending) ~ year+quarter+income5, tract_tb, mean)

# examine number of transactions
ggplot(data=income, aes(x=paste0(year, "Q", quarter),
                          y=volume_std,
                        group=as.factor(income5), col=as.factor(income5))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean number of transactions per tract, by income",
           x="Time", y="Number of transactions, standardized by week",
           col="Income", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/by-restaurant-transaction/mean-num-transactions-by-tract.jpeg", width=20, height=10, unit="cm")

# mean spending
ggplot(data=income, aes(x=paste0(year, "Q", quarter),
                        y=mean_spending,
                        group=as.factor(income5), col=as.factor(income5))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean spending per order, by income",
           x="Time", y="Spending",
           col="Income", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      #scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/by-restaurant-transaction/mean-spending-by-tract.jpeg", width=20, height=10, unit="cm")
rm(income)












### examine co-branded restaurants ----
# re-run lines thru 142
restaurants <- restaurants %>%
      filter(open<=as.Date("2015-03-31")&(is.na(close)|close>as.Date("2015-03-31")))
table(restaurants$concept)
restaurants$concept <- trimws(restaurants$concept, "both")
restaurants <- restaurants %>%
      filter(grepl("TBC", concept))
restaurants <- restaurants[order(restaurants$state, restaurants$address, restaurants$open), ]

# examine restaurants with other yum brands
other <- restaurants %>%
      filter(concept!="TBC")
table(other$ownership)
table(other$concept[other$ownership=="COMPANY"])

# clean up lon/lat data for mapping
summary(other$lon)
summary(other$lon[other$lon>0])
other[other$lon>0, c("address", "state", "lon", "lat")]
other$lon[other$lon==39.3409] <- -105.0659
other$lat[other$lat==-105.0659] <- 39.3409

# map co-branded restaurants
ggplot() +
      coord_fixed() +
      geom_polygon(data=map_data("state"),
                   aes(x=long, y=lat, group=group),
                   color="grey", fill="lightblue", size=0.1) +
      geom_point(data=subset(other, state!="AK"), 
                 aes(x=lon, y=lat, color=as.character(concept)), size=0.5) +
      scale_color_manual(labels=c("Taco Bell & KFC (n=779)", "Taco Bell & KFC & Pizza Hut (n=11)",
                                  "Taco Bell & AWR (n=1)", "Taco Bell & Long John Silver (n=98)",
                                  "Taco Bell & Pizza Hut (n=358)"),
                         values=c("blue", "yellow", "grey", "green", "red")) +
      labs(title="Locations of co-branded restaurants, 2015Q1", x="", y="", col="Brands",
           caption="Note: there were 1,248 co-branded restaurants, 18% of all restaurants. 1 restaurant in Alaska was excluded.") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = 'white', colour = 'white'), 
            axis.line = element_line(colour = "white"),
            axis.ticks=element_blank(), axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/tb-maps/cobrand-restaurants_2015q1.jpeg")

# look at co-branded restaurants, total sales
sales_all <- NULL
restaurants_all <- restaurants #re-run lines thru 142 to get master restaurant table
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                        sales <- read.csv(paste0("data/from-bigpurple/transaction-by-restaurant/by_restaurant_transaction_",
                                                 i, "q", j, ".csv"),
                                          stringsAsFactors = FALSE, sep = ";",
                                          header = FALSE, quote = "\"'",
                                          col.names = c("restid", "qty", "dollar"))
                        sales <- merge(restaurants_all, sales, by="restid") #merge restaurant type with sales
                        sales <- sales %>% #aggregate by multiple columns for multiple functions
                              group_by(concept) %>%
                              summarise_at(c("qty", "dollar"), funs(sum, mean))
                        sales$qty_pct <- sales$qty_sum/sum(sales$qty_sum)
                        sales$dollar_pct <- sales$dollar_sum/sum(sales$dollar_sum)
                        sales$quarter <- j
                        sales$year <- i
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, sales, restaurants_all)

#standardize sales by week
sales_all$qty_sum <- ifelse(sales_all$quarter==4, sales_all$qty_sum/16, sales_all$qty_sum/12)
sales_all$dollar_sum <- ifelse(sales_all$quarter==4, sales_all$dollar_sum/16, sales_all$dollar_sum/12)
sales_all$qty_mean <- ifelse(sales_all$quarter==4,  sales_all$qty_mean/16, sales_all$qty_mean/12)
sales_all$dollar_mean <- ifelse(sales_all$quarter==4, sales_all$dollar_mean/16, sales_all$dollar_mean/12)

# plot mean sales by restaurants type, over time
ggplot(data=sales_all, aes(x=interaction(year, quarter, lex.order = TRUE), y=qty_mean,
                           color=as.factor(concept), group=as.factor(concept))) +
      geom_point(size=0.75) +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:35, y=-200, label=c(rep(c(1:4),8), c(1:3)), size = 2) +
      ggplot2::annotate(geom="text", x=2.5+4*(0:8), y=-600, label=unique(sales_all$year), size=3) +
      coord_cartesian(ylim = c(0, 6000), expand = FALSE, clip = "off") +
      labs(title="Mean number of transactions, by restaurant brands",
           x="Time", y="Number of transactions",
           caption="- The dip in 2012 Q3 is likely due to a data anomoly, need to discuss with Taco Bell data team. \n- Number of transactions are standardized to weekly average.") +
      scale_color_manual(name="Brand(s)",
                           labels=c("Taco Bell & KFC", "Taco Bell & KFC & Pizza Hut",
                                    "Taco Bell", "Taco Bell & Long John Silver",
                                    "Taco Bell & Pizza Hut"),
                           values=c("dodgerblue1", "hotpink", "red1", "yellow3", "green4")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(vjust = -8, size = 12),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 12),
            plot.caption=element_text(hjust=0, vjust = -13, face="italic"))
#ggsave("tables/by-restaurant-transaction/mean-num-transactions-by-concept.jpeg", width=20, height=10, unit="cm")

# % of sales from cobranded restaurants total sales, over time
ggplot(data=sales_all, aes(x=interaction(year, quarter, lex.order = TRUE),
                           y=qty_pct, color=concept, group=concept)) +
      geom_point(size=0.75) +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:35, y=-0.04, label=c(rep(c(1:4),8), c(1:3)), size = 2) +
      ggplot2::annotate(geom="text", x=2.5+4*(0:8), y=-0.1, label=unique(sales_all$year), size=3) +
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = c(0, 1), expand = FALSE, clip = "off") +
      labs(title="Percent of sales, by brands", x="Time", y="% of sales",
           caption="Note: sales is measured as number of transactions.") +
      scale_color_manual(name="Brand(s)",
                         labels=c("Taco Bell & KFC", "Taco Bell & KFC & Pizza Hut",
                                  "Taco Bell", "Taco Bell & Long John Silver",
                                  "Taco Bell & Pizza Hut"),
                         values=c("dodgerblue1", "hotpink", "red1", "yellow3", "green4")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(vjust = -8, size = 12),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 12),
            plot.caption=element_text(hjust=0, vjust = -13, face="italic"))
#ggsave("tables/by-restaurant-transaction/pct-transactions-by-concept.jpeg", width=20, height=10, unit="cm")

# % of sales from other brand items, over time
sales <- read.csv("data/from-bigpurple/cobrand-sales-pct.csv", stringsAsFactors = FALSE)
sales$pct <- sales$qty/sales$total_qty

ggplot(data=sales, aes(x=interaction(year, quarter, lex.order = TRUE),
                           y=pct)) +
      geom_line(size=1, group=1) +
      ggplot2::annotate(geom="text", x=1:35, y=-0.015, label=c(rep(c(1:4),8), c(1:3)), size = 2) +
      ggplot2::annotate(geom="text", x=2.5+4*(0:8), y=-0.04, label=unique(sales$year), size=3) +
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = c(0, 0.25), expand = FALSE, clip = "off") +
      labs(title="Percent of sales with other brands items", x="Time", y="% of sales",
           caption="Note: sales includes transactions with at least one non-Taco Bell brand item.") +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(vjust = -8, size = 12),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 12),
            plot.caption=element_text(hjust=0, vjust = -13, face="italic"))
#ggsave("tables/by-restaurant-transaction/pct-transactions-other-brands.jpeg", width=20, height=8, unit="cm")

# histogram, by type, 2015Q1














