### matching treatment and comparison groups

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)

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
#install.packages("cobalt") #check and visualize balances for ps matching/rem
library(cobalt)
#install.packages("MatchIt")
library(MatchIt)
#install.packages("tableone") #calcualte SMD before matching
library(tableone)
#install.packages("CBPS") #post matching covariate balancing
library(CBPS)
#install.packages("Matching")
library(Matching)
#install.packages("optmatch")
library(optmatch)
#install.packages("ebal") #entropy balance weighting after matching
library(ebal)
#install.packages("WeightIt")
library(WeightIt)
#install.packages("broom") #change lm model results into dataframe for estimating individual slopes
library(broom)
#install.packages("sbw")
library(sbw)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("gbm")
library(gbm)

### import acs data from ipums ----
acs <- read.csv("data/census-data/tract/nhgis0004_acs-for-matching/nhgis0004_ds201_20135_2013_tract.csv",
                stringsAsFactors = FALSE)
names(acs)
acs$hsbelow <- rowSums(acs[, c(123:137)], na.rm = TRUE)
acs$collegeup <- rowSums(acs[, c(143:146)])
acs$under18 <- rowSums(acs[, c(40:43, 57:62)])
acs$above65 <- rowSums(acs[, c(64:67, 81:86)])

acs <- acs %>%
  dplyr::select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA, UEEE002,
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
  dplyr::select(tract_num:close, concept:drive_thru_type, ownership, city, county.x, male:above65)
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
#calorie[, c(6:11, 13)] <- calorie[, c(6:11, 13)]/calorie$count

### adding drive-thru and meal time breakdown ----
drive07 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_2007_Q1.csv",
                  stringsAsFactors=FALSE)
drive07$count <- drive07$count/2
meal07 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart/mean-calorie_restid_daypart_2007_Q1.csv",
                 stringsAsFactors=FALSE)
meal07$count <- meal07$count/2

drive <- NULL
meal <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_",
                                  i,"_Q",j,".csv"), stringsAsFactors = FALSE)
        drive <- rbind(drive, sample)
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart/mean-calorie_restid_daypart_",
                                  i,"_Q",j,".csv"), stringsAsFactors = FALSE)
        meal <- rbind(meal, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
drive <- rbind(drive07, drive)
drive <- drive %>%
  filter(DW_OCCASION==2) %>%
  dplyr::select(1,3,11) %>%
  rename(drive=count) %>%
  group_by(DW_RESTID, DW_MONTH) %>%
  summarise(drive = sum(drive))
  
meal <- rbind(meal07, meal)
meal <- meal %>%
  filter(DW_DAYPART==3|DW_DAYPART==5) %>%
  dplyr::select(1,3,11) %>%
  rename(meal=count) %>%
  group_by(DW_RESTID, DW_MONTH) %>%
  summarise(meal = sum(meal))
rm(drive07, sample, meal07, i, j)
drive <- merge(drive, meal, by=c("DW_RESTID", "DW_MONTH"))
colnames(drive)[1:2] <- c("restid", "monthno")
rm(meal)

### merge restaurant and calorie information, fix missing values, consolidate restaurants by address ----
restaurant <- merge(restaurant, calorie, by="restid")
restaurant <- restaurant[order(restaurant$address, restaurant$state, restaurant$monthno), ]
names(restaurant)
sapply(restaurant, function(x) sum(is.na(x)))
sapply(restaurant, function(x) sum(is.nan(x)))

#fix drive-thru information
table(restaurant$drive_thru, restaurant$drive_thru_type, useNA="always")
sum(is.na(restaurant$drive_thru))
restaurant$drive_thru[is.na(restaurant$drive_thru)] <- 0
restaurant$drive_thru_type[restaurant$drive_thru==0] <- "Neither"
restaurant$drive_thru_type[restaurant$drive_thru==1&is.na(restaurant$drive_thru_type)] <- "Driver"

# merge drive-thru and mealtime breakdown
restaurant <- merge(restaurant, drive, by=c("restid", "monthno"))
restaurant <- restaurant[, -c(8,25:28)]

# aggregate on address, instead of restid
tmp1 <- restaurant[, c(3:5, 9:23)]
tmp1 <- tmp1[!duplicated(tmp1), ]
length(unique(paste0(restaurant$address, restaurant$tract_num)))
tmp1 <- tmp1[order(tmp1$state, tmp1$address), ]

#tmp2 <- restaurant[, c(2:3, 5:8, 28:39)]
tmp2 <- restaurant[, c(2:4,6:8,24:36)]
tmp2 <- aggregate(data=tmp2,
                  .~tract_num+address+concept+drive_thru+ownership+year+month+yearno+monthno,
                  sum)
restaurant <- merge(tmp2, tmp1, by=c("address", "tract_num"))
rm(tmp1, tmp2, drive)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant <- restaurant[order(restaurant$state, restaurant$address, restaurant$ownership, restaurant$monthno), ]

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
                        ifelse(restaurant$state=="NY"&restaurant$county=="Suffolk"&
                                 (restaurant$year>=2011|(restaurant$year==2010&restaurant$month>=11)),1,
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
                                 ((restaurant$year>=2010)|((restaurant$year==2009)&
                                 restaurant$month>=11)), 1,
                        ifelse(restaurant$state=="VT"&(restaurant$year>=2013|
                                (restaurant$year==2012&restaurant$month>=6)), 1, 0)))))))))))))))))
rm(acs, calorie, time)
table(restaurant$state[restaurant$ml==1])

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
                                                                                   ifelse(restaurant$state=="NY"&restaurant$county=="Suffolk",251,
                                                                                          ifelse(restaurant$state=="MA", 251,
                                                                                                 ifelse(restaurant$state=="ME", 254,
                                                                                                        ifelse(restaurant$state=="NJ", 254,
                                                                                                               ifelse(restaurant$state=="OR", 253,
                                                                                                                      ifelse(restaurant$state=="NY"&restaurant$county=="Ulster",238,
                                                                                                                             ifelse(restaurant$state=="NY"&restaurant$county=="Nassau", 239,
                                                                                                                                    ifelse(restaurant$state=="VT", 270, NA)))))))))))))))))
restaurant$treat <- ifelse(!is.na(restaurant$entry),1,0)

### prepare data for matching ----
# mean spending per order, mean calorie, % drive-thru, % lunch/dinner
names(restaurant)
restaurant[, c(10:15,17:19)] <- restaurant[, c(10:15,17:19)]/restaurant$count

# create log vars
# replace 0 values with a small value
#restaurant <- restaurant %>% 
#  mutate(across(c(calorie,count,median_income,capital_income,total), ~ ifelse(.==0, log(.+1), log(.)), .names = "{col}"))

# take lagged measurements for dynamic vars
restaurant <- restaurant[order(restaurant$state, restaurant$address, restaurant$monthno), ]
restaurant <- restaurant %>%
  group_by(address, tract_num, concept, ownership) %>%
  mutate(across(c(count,dollar), ~ lag(., 1), .names = "{col}1")) %>%
  mutate(calorie1=dplyr::lag(calorie,1,default = NA)) %>%
  mutate(calorie2=dplyr::lag(calorie,2,default = NA)) %>%
  mutate(calorie3=dplyr::lag(calorie,3,default = NA)) %>%
  mutate(calorie4=dplyr::lag(calorie,4,default = NA)) %>%
  mutate(calorie5=dplyr::lag(calorie,5,default = NA)) %>%
  mutate(calorie6=dplyr::lag(calorie,6,default = NA)) %>%
  mutate(calorie7=dplyr::lag(calorie,7,default = NA)) %>%
  mutate(calorie8=dplyr::lag(calorie,8,default = NA)) %>%
  mutate(calorie9=dplyr::lag(calorie,9,default = NA)) %>%
  mutate(calorie10=dplyr::lag(calorie,10,default = NA)) %>%
  mutate(calorie11=dplyr::lag(calorie,11,default = NA)) %>%
  mutate(calorie12=dplyr::lag(calorie,12,default = NA)) 

### estimating individual slopes ----
master <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
    dollar <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-6 & monthno<i) %>%
      do(tidy(lm(dollar~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>%
      rename(slope_dollar = estimate) 
    count <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-6 & monthno<i) %>%
      do(tidy(lm(count~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>%
      rename(slope_count = estimate) 
    calorie <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-6 & monthno<i) %>%
      do(tidy(lm(calorie~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>%
      rename(slope_calorie = estimate)
    dollar <- merge(dollar, count, by=c("address","tract_num","ownership","concept"))
    dollar <- merge(dollar, calorie, by=c("address","tract_num","ownership","concept"))
    dollar$monthno <- i
    master <- rbind(master, dollar)
}
rm(i, dollar, count, calorie)

restaurant <- merge(restaurant, master, all = TRUE,
                    by=c("address","tract_num","ownership","concept", "monthno"))
rm(master)
restaurant$tract_num <- paste0("'",restaurant$tract_num,"'")
# identify ml policy by city/state
restaurant$policy <- ifelse(restaurant$state=="NY"&
                              (restaurant$county=="New York"|restaurant$county=="Kings"|
                                 restaurant$county=="Bronx"|restaurant$county=="Queens"|
                                 restaurant$county=="Richmond"), "nyc",
                            ifelse(restaurant$state=="WA"&restaurant$county=="King", "king",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Albany", "albany",
                            ifelse(restaurant$state=="PA"&restaurant$city=="Philadelphia", "philly", 
                            ifelse(restaurant$state=="CA"&(restaurant$city=="San Francisco"|
                                                             restaurant$county=="San Francisco"), "sf",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Westchester", "westchester",
                            ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery", "mont",
                            ifelse(restaurant$state=="OR"&restaurant$county=="Multnomah", "mult",
                            ifelse(restaurant$state=="CA", "ca",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Suffolk", "suffolk",
                            ifelse(restaurant$state=="MA", "ma",
                            ifelse(restaurant$state=="ME", "me",
                            ifelse(restaurant$state=="NJ", "nj",
                            ifelse(restaurant$state=="OR", "or",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Ulster","ulster",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Nassau", "nassau",
                            ifelse(restaurant$state=="VT", "vt", "none")))))))))))))))))
restaurant$concept <- ifelse(restaurant$concept=="TBC", 1, 0)
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY", 1, 0)
#export unmatched data
#write.csv(restaurant, "data/calorie-aims/unmatched-restaurants.csv", row.names = FALSE)

### label missing data in treated restaurants ----
tmp <- restaurant %>%
  filter(treat==1) %>%
  group_by(address) %>%
  mutate(relative=monthno-entry) %>%
  filter(relative<=5&relative>=-6) %>%
  mutate(n=n()) %>%
  mutate(open6 = ifelse(n==12, 1,0)) %>%
  filter(monthno==entry)
table(tmp$open6)

### finalize diff matching+weighting, no caliper ----
# ps+iptw, mahal+entrp,mahal+sbw, entrp only
formula <- treat~concept+ownership+calorie1+slope_calorie+
  count1+slope_count+dollar1+slope_dollar+drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65

#ps matching+iptw
master <- NULL
matched <- NULL
for (i in c(221,226,229,233,238,239,241,242,247,251,253,254,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    if (i==247) {
      for (j in c("mont", "mult")) {
        subset <- subset(restaurant_subset, (entry==i & monthno==i)&(treat==0|(treat==1&policy==j)))
        #matching
        set.seed(10)
        subset.match <- matchit(data=subset, formula = formula, 
                                distance="logit", method="nearest", #caliper=0.5,
                                replace=TRUE, ratio=3)
        match <- match.data(subset.match, distance="distance", weights = "s.weights") 
        #add distance to unmatched data
        subset$distance <- subset.match$distance
        #add ps balance
        bal <- weightit(data=match, formula = formula, method = "ps",
                        estimand = "ATT", s.weights = "s.weights")
        match$weights <- bal$weights
        match$match_place <- j
        # combine clusters of restaurants
        master <- rbind(master, subset)
        matched <- rbind(matched, match)
      }
    } else if (i==253) {
      for (j in c("ca", "or")) {
        subset <- subset(restaurant_subset, (entry==i & monthno==i)&(treat==0|(treat==1&policy==j)))
        #matching
        set.seed(10)
        subset.match <- matchit(data=subset, formula = formula, 
                                distance="logit", method="nearest", #caliper=0.5,
                                replace=TRUE, ratio=3)
        match <- match.data(subset.match, distance="distance", weights = "s.weights") 
        #add distance to unmatched data
        subset$distance <- subset.match$distance
        #add ps balance
        bal <- weightit(data=match, formula = formula, method = "ps",
                        estimand = "ATT", s.weights = "s.weights")
        match$weights <- bal$weights
        match$match_place <- j
        # combine clusters of restaurants
        master <- rbind(master, subset)
        matched <- rbind(matched, match)
      }
    } else if (i==254) {
      for (j in c("nj", "me")) {
        subset <- subset(restaurant_subset, (entry==i & monthno==i)&(treat==0|(treat==1&policy==j)))
        #matching
        set.seed(10)
        subset.match <- matchit(data=subset, formula = formula, 
                                distance="logit", method="nearest", #caliper=0.5,
                                replace=TRUE, ratio=3)
        match <- match.data(subset.match, distance="distance", weights = "s.weights") 
        #add distance to unmatched data
        subset$distance <- subset.match$distance
        #add ps balance
        bal <- weightit(data=match, formula = formula, method = "ps",
                        estimand = "ATT", s.weights = "s.weights")
        match$weights <- bal$weights
        match$match_place <- j
        # combine clusters of restaurants
        master <- rbind(master, subset)
        matched <- rbind(matched, match)
      }
    } else {
      subset <- subset(restaurant_subset, entry==i & monthno==i)
      #matching
      set.seed(10)
      subset.match <- matchit(data=subset, formula = formula, 
                              distance="logit", method="nearest", #caliper=0.5,
                              replace=TRUE, ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      #add distance to unmatched data
      subset$distance <- subset.match$distance
      #add ps balance
      bal <- weightit(data=match, formula = formula, method = "ps",
                      estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- as.character(i)
      # combine clusters of restaurants
      master <- rbind(master, subset)
      matched <- rbind(matched, match)
    }
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, subset.match, match, i, bal, j)
matched$match_place <- ifelse(matched$match_place=="229", "king",
                              ifelse(matched$match_place=="239", "nassau",
                              ifelse(matched$match_place=="241", "philly",
                              ifelse(matched$match_place=="242", "albany",
                              ifelse(matched$match_place=="251", "ma", matched$match_place)))))
table(matched$match_place)

#export table for aim 1 analysis
# identify comparison units that were matched to multiple restaurants
master_all <- NULL
for (i in c("albany","ca","king","ma","mont","mult","nassau","nj","or","philly")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(match_to = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(address:treat)
  master_all <- rbind(master_all, tmp)
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants.csv", row.names = FALSE)
rm(master_all, i)

#summary stats
#reduced data
length(unique(master$address[master$treat==1]))
length(unique(master$address[master$treat==0]))

#matched data
length(unique(matched$address[matched$treat==1]))
length(unique(matched$address[matched$treat==0]))
length(unique(matched$address[matched$treat==0&matched$weights>=0.1]))

hist(matched$s.weights[matched$treat==0], breaks = 100,
     main="PS matching weighting results, no caliper",
     xlab="Weights assigned to comparison units")

result <- cbind(col_w_smd(mat=subset(master,select = c(3:4,6,18:19,23,25:35,39:44,46)),
                          treat = master$treat,
                          std = TRUE, bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))),
                col_w_smd(mat=subset(matched, select = c(3:4,6,18:19,23,25:35,39:44,46)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))))
#ps+entrp
master <- NULL
matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    #matching
    set.seed(10)
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="nearest",
                            replace=TRUE, ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "entropy", #tol=rep(0.5,23),
                    estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, subset.match, match, i, bal)
#matched data
length(unique(matched$address[matched$treat==1]))
length(unique(matched$address[matched$treat==0]))
length(unique(matched$address[matched$treat==0&matched$weights>=0.1]))
result <- cbind(result,
                col_w_smd(mat=subset(matched, select = c(3:4,6,18:19,23,25:35,39:44,46)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))))

#ps+sbw
master <- NULL
matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    #matching
    set.seed(10)
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="nearest", #caliper=0.2,
                            replace=TRUE, ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    #bal <- weightit(data=match, formula = formula, method = "entropy",
    #                estimand = "ATT", s.weights = "s.weights")
    bal <- optweight(data=match, formula = formula, tol=rep(0.25,23),
                    estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, subset.match, match, i, bal)
matched <- matched[matched$weights!=2143289344, ]
#matched data
length(unique(matched$address[matched$treat==1]))
length(unique(matched$address[matched$treat==0]))
length(unique(matched$address[matched$treat==0&matched$weights>=0.1]))
result <- cbind(result,
                col_w_smd(mat=subset(matched, select = c(3:4,6,18:19,23,25:35,39:44,46)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))))
#mahal distance
# use pscore as one of the covariates to be balanced
vars <- c("concept","drive_thru","ownership","calorie1","slope_calorie",
          "count1","slope_count","dollar1", "slope_dollar", "drive", "meal",
          "total","male","white","black","asian","hisp",
          "median_income","capital_income","hsbelow","collegeup",
          "under18","above65", "pscore")

formula.m <- treat~concept+drive_thru+ownership+calorie1+slope_calorie+
  count1+slope_count+dollar1+slope_dollar+drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+pscore

# mahal+entrp
master <- NULL
matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    # combine clusters of restaurants
    master <- rbind(master, subset)
    #matching
    subset.match <- matchit(data=subset, formula = formula.m, 
                            distance="mahalanobis", method="nearest", 
                            replace=TRUE, ratio=3, mahvars=vars)
    #summary(subset.match)
    match <- match.data(subset.match, weights = "s.weights")
    match$distance <- NULL
    #add entropy balance
    bal <- weightit(data=match, formula = formula.m, method = "ebal", tol=rep(0.25,24),
                    estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    # combine clusters of restaurants
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, subset.match, match, i, bal)
#matched data
length(unique(matched$address[matched$treat==1]))
length(unique(matched$address[matched$treat==0]))
length(unique(matched$address[matched$treat==0&matched$weights>=0.1]))
result <- cbind(result,
                col_w_smd(mat=subset(matched, select = c(3:4,6,18:19,23,25:35,39:44,45)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))))

#mahal+sbw
master <- NULL
matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    # combine clusters of restaurants
    master <- rbind(master, subset)
    #matching
    subset.match <- matchit(data=subset, formula = formula.m, 
                            distance="mahalanobis", method="nearest", 
                            replace=TRUE, ratio=3, mahvars=vars)
    #summary(subset.match)
    match <- match.data(subset.match, weights = "s.weights")
    match$distance <- NULL
    #add sbw weights
    bal <- optweight(data=match, formula = formula.m, tols = c(rep(0.25,24)),
                     estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    # combine clusters of restaurants
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, subset.match, match, i, bal)
matched <- matched %>% filter(weights==2143289344)
#matched data
length(unique(matched$address[matched$treat==1]))
length(unique(matched$address[matched$treat==0]))
length(unique(matched$address[matched$treat==0&matched$weights>=0.1]))
result <- cbind(result,
                col_w_smd(mat=subset(matched, select = c(3:4,6,18:19,23,25:35,39:44,45)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE, 3), rep(FALSE, 21))))

colnames(result)[1:6] <- c("pre", "ps_weight", "ps_entrp", "ps_sbw", "mahal_entrp", "mahal_sbw")
result <- cbind(result,
                data.frame(old=row.names(result),
                           new=c("Joint brand", "Ownership", "Has drive through",
                                 "% drive-thru transactions", "% lunch/dinner transactions",
                                 "% male", "Total population","% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65","Mean calorie, t-1",
                                 "# of transactions, t-1","Mean spending per order, t-1",  
                                 "Mean spending per order trend", "# of transactions trend", "Mean calorie trend", 
                                 "Distance")))
names(result)
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:6]), v.names = "score",
                  idvar = c("old", "new"),
                  timevar = "method", times = c("pre","ps_weight", "ps_entrp", "ps_sbw", "mahal_entrp", "mahal_sbw"))

# replicate love.plot
result$label <- factor(result$new, levels=c("Distance", "Joint brand", "Has drive through","Ownership",
                                            "Mean calorie, t-1", "Mean calorie trend",
                                            "# of transactions, t-1", "# of transactions trend",
                                            "Mean spending per order, t-1", "Mean spending per order trend",
                                            "% drive-thru transactions", "% lunch/dinner transactions",
                                            "Total population", "% male", "% white", "% Black", "% Asian",
                                            "% Hispanic", "Household median income", "Income per capita",
                                            "% without HS degree", "% has college degree and up",
                                            "% under 18", "% above 65"))
result$method <- factor(result$method, levels = c("pre","ps_weight", "ps_entrp", "ps_sbw", "mahal_entrp", "mahal_sbw"))

ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_vline(xintercept = 23.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, overall comparisons",
       y="Standardized mean differences", x="",
       caption="Note: no transformations on any variables.") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+Entropy", "PS+SBW",
                                             "Mahalanobis+Entropy", "Mahalanobis+SBW"),
                     values =c("orange", "aquamarine3", "red", "blueviolet", "skyblue", "grey")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/finalize/covariate-balance-nolog.jpeg", dpi="retina")

### ps matching + iptw weighting, trim extrem weights ----
formula <- treat~concept+ownership+calorie1+slope_calorie+
  count1+slope_count+dollar1+slope_dollar+drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24
time <- data.frame(c("king","nassau","philly","mont","mult","suffolk","ma","ca","or","nj","vt"),
                   c(229,239,241,247,247,251,251,253,253,254,270))
colnames(time)[1:2] <- c("location","time")
master2 <- NULL
matched2 <- NULL
for (i in c(1:11)) {
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>%
      group_by(address) %>%
      mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>%
      mutate(n=n()) %>%
      mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>%
      mutate(n=n()) %>%
      mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>%
      mutate(n=n()) %>%
      mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-6) %>%
      mutate(n=n()) %>%
      mutate(open6 = ifelse(n==6, 1,0)) %>%
      dplyr::select(address,open6,open12,open18,open24) %>% #
      distinct()
    subset <- merge(subset,tmp,by="address")
    #table(subset$open6, subset$treat)
    subset <- subset(subset, open6==1&monthno==time[i,2], select=-c(calorie2:calorie12)) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="nearest", #caliper=0.5,
                            replace=TRUE, ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",
                    estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    # combine clusters of restaurants
    master2 <- rbind(master2, subset)
    matched2 <- rbind(matched2, match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}
rm(i,bal, time,subset, subset.match,tmp,match)
length(unique(paste0(matched$address[matched$treat==1],matched$match_place[matched$treat==1]))) #544,438

# trim small and large weights
summary(matched$weights)
hist(matched$weights, breaks = 100,xlab = "Weight",main="Histogram of comparison units weights")
tmp <- matched[matched$weights>=20,]
restaurant2 <- restaurant %>%
  filter(!grepl("241 S. Dupont Hwy, Dover, DE 19901|15663 127th Street, Lemont, IL 60439|3615 Mundy Mill Rd, Oakwood, GA 30566",address))
#repeat the matching process and check large weights
length(unique(paste0(matched2$address[matched2$treat==0],matched2$match_place[matched2$treat==0]))) #544,441
summary(matched2$weights)
par(mfrow=c(2,1))
hist(matched$weights[matched$treat==0], breaks = 100,xlab = "Weight",main="Histogram of comparison units weights")
hist(matched2$weights[matched$treat==0], breaks = 100,xlab = "Weight",main="After trimming")

result <- cbind(col_w_smd(mat=subset(master,select = c(3:4,18:19,23,25:35,39:44,47:50)),
                          treat = master$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,18:19,23,25:35,39:44,47:50)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched2, select = c(3:4,18:19,23,25:35,39:44,47:50)),
                          weights = matched2$weights, treat = matched2$treat, s.weights = matched2$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- cbind(result,
                data.frame(old=row.names(result),
                           new=c("Ownership", "Joint brand",
                                 "% drive-thru transactions", "% lunch/dinner transactions",
                                 "% male", "Total population","% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65","# of transactions, t-1","Mean spending per order, t-1","Mean calorie, t-1",
                                 "Mean spending per order trend", "# of transactions trend", "Mean calorie trend",
                                 "Has 12-mon pre-data","Has 18-mon pre-data","Has 24-mon pre-data",
                                 "Distance")))
names(result)
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:3]), v.names = "score",
                  idvar = c("old", "new"),
                  timevar = "method", times = c("pre","ps_weight","ps_weight_trim"))

# replicate love.plot
result$label <- factor(result$new, levels=c("Distance", "Joint brand", "Ownership",
                                            "Has 12-mon pre-data","Has 18-mon pre-data","Has 24-mon pre-data",
                                            "Mean calorie, t-1", "Mean calorie trend",
                                            "# of transactions, t-1", "# of transactions trend",
                                            "Mean spending per order, t-1", "Mean spending per order trend",
                                            "% drive-thru transactions", "% lunch/dinner transactions",
                                            "Total population", "% male", "% white", "% Black", "% Asian",
                                            "% Hispanic", "Household median income", "Income per capita",
                                            "% without HS degree", "% has college degree and up",
                                            "% under 18", "% above 65"))
result$method <- factor(result$method, levels = c("pre","ps_weight","ps_weight_trim"))

ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_vline(xintercept = 23.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, overall comparisons",
       y="Standardized mean differences", x="",
       caption="Note: no transformations on any variables.") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/finalize/covariate-balance-nolog.jpeg", dpi="retina")
