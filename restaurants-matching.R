### matching treatment and comparison groups

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("tigris") #geocoding, add census tract number to geo coordinates
#install.packages("sf")
#install.packages("tidycensus") #get census tract boundaries
library(tigris)
library(sf)
library(tidycensus)
#install.packages("rollmatch")
library(rollmatch)

### import acs data from ipums ----
acs <- read.csv("data/census-data/tract/nhgis0004_acs-for-matching/nhgis0004_ds201_20135_2013_tract.csv",
                stringsAsFactors = FALSE)
names(acs)
acs$hsbelow <- rowSums(acs[, c(123:137)], na.rm = TRUE)
acs$collegeup <- rowSums(acs[, c(143:146)])
acs$under18 <- rowSums(acs[, c(40:43, 57:62)])
acs$above65 <- rowSums(acs[, c(64:67, 81:86)])

acs <- acs %>%
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA, UEEE002,
         UEEE026, UEPE001, UEQE002, UEQE003, UEQE005, UEYE012,
         UHDE001, UJAE001, hsbelow, collegeup, under18, above65) 
colnames(acs)[1:15] <- c("geo", "state", "state_num", "county", "county_num", "tract_num",
                       "male", "female", "total", "white", "black", "asian", "hisp",
                       "median_income", "capital_income")
sapply(acs, class)

acs$geo1 <- substr(acs$geo, 2,3)
acs$geo2 <- substr(acs$geo, 5,7)
acs$geo3 <- substr(acs$geo, 9,14)
acs$geo <- paste0(acs$geo1, acs$geo2, acs$geo3)
acs <- acs[, -c(3,5:6,20:22)]
names(acs)
colnames(acs)[1] <- "tract_num"
acs[, c(4,5,7:10,13:16)] <- acs[, c(4,5,7:10,13:16)]/acs$total
#write.csv(acs, "data/census-data/tract/nhgis0004_acs-for-matching/nhgis0004_clean.csv",
#          row.names = FALSE)

### add census tract number to restaurants ----
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       stringsAsFactors = FALSE)
summary(restaurant$lat)
summary(restaurant$lon)
names(restaurant)
restaurant$tract_num <- substr(restaurant$tract_num, 2, 12) 

restaurant <- merge(restaurant, acs, by="tract_num") #lose 3 restaurants
names(restaurant)
restaurant <- restaurant %>%
  select(tract_num:close, concept:drive_thru_type, ownership, city, county.x, male:above65)
colnames(restaurant)[c(4,14)] <- c("state", "county")

#change date to date format
sapply(restaurant, class)
restaurant <- cbind(restaurant[c(1:4, 9:27)], lapply(restaurant[c(5:8)], function(x) as.Date(x, "%m/%d/%Y")))

### clean restaurant level transaction records ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-overall/mean-calorie_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid", "year", "month", "calorie", "fat",
                                     "sat_fat", "carb", "protein", "sodium", "count", "dollar"))
sapply(sample07q1, class)
sample07q1[, c(4:9, 11)] <- sample07q1[, c(4:9, 11)]/2

calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-overall/mean-calorie_restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid", "year", "month",
                                       "calorie", "fat", "sat_fat",
                                       "carb", "protein", "sodium",
                                       "count", "dollar"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

### add time information: month, year; keep yearno and monthno ----
time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
names(time)
time <- time[, c(4, 7, 17, 38)]
colnames(time) <- c("year", "month", "yearno", "monthno")
sapply(time, class)
time$yearno <- as.integer(substr(time$yearno, 2, 5))
time$monthno <- as.integer(substr(time$monthno, 6, 7))
time <- time[!duplicated(time) & time$yearno>=2006, ]

calorie <- merge(calorie, time, by=c("year", "month"))
colnames(calorie)[c(1:2, 12:13)] <- c("yearno", "monthno", "year", "month")
calorie <- aggregate(data=calorie, .~year+month+restid+yearno+monthno, sum) 
calorie <- calorie[order(calorie$year, calorie$month), ]
calorie[, c(6:11, 13)] <- calorie[, c(6:11, 13)]/calorie$count

### merge restaurant and calorie information ----
restaurant <- merge(restaurant, calorie, by="restid")

### add timeline for menu labeling in city/state ----
#C:\Users\wue04\NYU Langone Health\Elbel, Brian - Taco Bell labeling R01\PROPOSAL\Menu Labeling Legislation Research
restaurant$ml <- ifelse(restaurant$state=="NY"&
                          (restaurant$county=="New York"|restaurant$county=="Kings"|
                             restaurant$county=="Bronx"|restaurant$county=="Queens"|
                             restaurant$county=="Richmond")&
                          ((restaurant$year>=2008&restaurant$month>=5)|
                             restaurant$year>=2009), 1,
                        ifelse(restaurant$state=="WA"&restaurant$county=="King"&
                                 restaurant$year>=2009, 1,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Albany"&
                                 ((restaurant$year>=2010&restaurant$month>=2)|
                                  restaurant$year>=2011), 1,
                        ifelse(restaurant$state=="PA"&restaurant$city=="Philadelphia"&
                                 restaurant$year>=2010, 1, 
                        ifelse(restaurant$state=="CA"&(restaurant$city=="San Francisco"|
                                  restaurant$county=="San Francisco")&
                                 (restaurant$year>=2009|(restaurant$year==2008&restaurant$month>=10)), 1,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Westchester"&
                                 (restaurant$year>=2010|(restaurant$year==2009&restaurant$month>=5)), 1,
                        ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery"&
                                 (restaurant$year>=2011|(restaurant$year==2010&restaurant$month>=7)), 1,
                        ifelse(restaurant$state=="OR"&restaurant$county=="Multnomah"&
                                 (restaurant$year>=2011|(restaurant$year==2010&restaurant$month>=7)), 1,
                        ifelse(restaurant$state=="CA"&restaurant$year>=2011, 1,
                        ifelse(restaurant$state=="MA"&(restaurant$year>=2011|
                                  (restaurant$year==2010&restaurant$month>=11)), 1,
                        ifelse(restaurant$state=="ME"&(restaurant$year>=2012|
                                  (restaurant$year==2011&restaurant$month>=2)), 1,
                        ifelse(restaurant$state=="NJ"&(restaurant$year>=2012|
                                  (restaurant$year==2011&restaurant$month>=2)), 1,
                        ifelse(restaurant$state=="OR"&restaurant$year>=2011, 1,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Ulster"&
                                 (restaurant$year>=2010|(restaurant$year==2009&
                                  restaurant$month>=10)),1,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Nassau"&
                                 (restaurant$year>=2010)|(restaurant$year==2009)&
                                 restaurant$month>=11, 1,
                        ifelse(restaurant$state=="VT"&(restaurant$year>=2013|
                                (restaurant$year==2012&restaurant$month>=6)), 1, 0))))))))))))))))
rm(acs, calorie)

### prepare data for matching ----
restaurant$entry <- ifelse(restaurant$state=="NY"&
                          (restaurant$county=="New York"|restaurant$county=="Kings"|
                             restaurant$county=="Bronx"|restaurant$county=="Queens"|
                             restaurant$county=="Richmond"), 221,
                        ifelse(restaurant$state=="WA"&restaurant$county=="King", 229,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Albany", 242,
                        ifelse(restaurant$state=="PA"&restaurant$city=="Philadelphia", 241, 
                        ifelse(restaurant$state=="CA"&(restaurant$city=="San Francisco"|
                                                         restaurant$county=="San Francisco"), 226,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Westchester", 233,
                        ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery", 247,
                        ifelse(restaurant$state=="OR"&restaurant$county=="Multnomah", 247,
                        ifelse(restaurant$state=="CA", 253,
                        ifelse(restaurant$state=="MA", 251,
                        ifelse(restaurant$state=="ME", 254,
                        ifelse(restaurant$state=="NJ", 254,
                        ifelse(restaurant$state=="OR", 253,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Ulster",238,
                        ifelse(restaurant$state=="NY"&restaurant$county=="Nassau", 239,
                        ifelse(restaurant$state=="VT", 270, restaurant$monthno))))))))))))))))

restaurant$treat <- ifelse(restaurant$state=="NY"&
                             (restaurant$county=="New York"|restaurant$county=="Kings"|
                                restaurant$county=="Bronx"|restaurant$county=="Queens"|
                                restaurant$county=="Richmond"), 1,
                           ifelse(restaurant$state=="WA"&restaurant$county=="King", 1,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Albany", 1,
                           ifelse(restaurant$state=="PA"&restaurant$city=="Philadelphia", 1, 
                           ifelse(restaurant$state=="CA"&(restaurant$city=="San Francisco"|
                                                            restaurant$county=="San Francisco"), 1,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Westchester", 1,
                           ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery", 1,
                           ifelse(restaurant$state=="OR"&restaurant$county=="Multnomah", 1,
                           ifelse(restaurant$state=="CA", 1,
                           ifelse(restaurant$state=="MA", 1,
                           ifelse(restaurant$state=="ME", 14,
                           ifelse(restaurant$state=="NJ", 1,
                           ifelse(restaurant$state=="OR", 1,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Ulster",1,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Nassau", 1,
                           ifelse(restaurant$state=="VT", 1, 0))))))))))))))))

# fix NA values
restaurant$drive_thru[is.na(restaurant$drive_thru)] <- 0
table(restaurant$drive_thru)
restaurant <- restaurant[!is.na(restaurant$median_income), ]
restaurant$drive_thru_type[is.na(restaurant$drive_thru_type)] <- "Driver"

### matching ----
#reduce input data for matching
reduced_data <- reduce_data(data=subset(restaurant, select=-c(open:close)),
                            treat="treat", tm="monthno", entry="entry",
                            id="restid", lookback=3)

#select variables for matching
#calculate propsensity scores for each observation
names(reduced_data)
scored_data <- score_data(reduced_data = reduced_data, model_type = "logistic",
                          match_on = "logit", treat="treat",
                          tm="monthno", entry="entry", id="restid",
                          fm=as.formula(treat~concept+drive_thru+ownership+male+
                                          total+white+black+asian+hisp+median_income+
                                          capital_income+hsbelow+collegeup+under18+
                                          above65+calorie+count+dollar))

# apply rolling entry mathcing algorithm
matched_data <- rollmatch(scored_data = scored_data, data=restaurant,
                          treat="treat",
                          tm="monthno", entry="entry", id="restid",
                          vars=all.vars(as.formula(treat~concept+drive_thru+ownership+male+
                                            total+white+black+asian+hisp+median_income+
                                            capital_income+hsbelow+collegeup+under18+
                                            above65+calorie+count+dollar)),
                          lookback = 3, alpha = 0.2,
                          standard_deviation = "average", num_matches = 3,
                          replacement = FALSE)


