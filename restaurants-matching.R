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
                           col.names=c("restid", "year", "month", "calorie", "fat", "sat_fat",
                                       "carb", "protein", "sodium", "count", "dollar"))
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

tmp2 <- restaurant[, c(2:4,6:8,24:36)]
tmp2 <- aggregate(data=tmp2,
                  .~tract_num+address+concept+ownership+year+month+yearno+monthno,
                  sum)
restaurant <- merge(tmp2, tmp1, by=c("address", "tract_num"))
rm(tmp1, tmp2, drive)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant <- restaurant[order(restaurant$state, restaurant$address, restaurant$ownership, restaurant$monthno), ]

rm(acs, calorie, time)
### add timeline for menu labeling in city/state ----
#C:\Users\wue04\NYU Langone Health\Elbel, Brian - Taco Bell labeling R01\PROPOSAL\Menu Labeling Legislation Research
restaurant$entry <- ifelse(restaurant$state=="NY"&
                             (restaurant$county=="New York"|restaurant$county=="Kings"|
                                restaurant$county=="Bronx"|restaurant$county=="Queens"|
                                restaurant$county=="Richmond"), 221,
                           ifelse(restaurant$state=="WA"&restaurant$county=="King", 229,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Albany", 243,
                           ifelse(restaurant$state=="PA"&restaurant$city=="Philadelphia", 241, 
                           ifelse(restaurant$state=="NY"&restaurant$county=="Westchester", 233,
                           ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery", 253,
                           ifelse(restaurant$state=="CA", 253,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Suffolk",251,
                           ifelse(restaurant$state=="MA", 251,
                           ifelse(restaurant$state=="ME", 254,
                           ifelse(restaurant$state=="OR", 253,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Ulster",238,
                           ifelse(restaurant$state=="VT", 270,
                           ifelse(restaurant$state=="NY"&restaurant$county=="Schenectady",249,NA))))))))))))))
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
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 3), .names = "{col}3")) %>%
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 4), .names = "{col}4")) %>%
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 5), .names = "{col}5")) %>%
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 6), .names = "{col}6")) %>%
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 7), .names = "{col}7")) %>%
  mutate(across(c(count,dollar,calorie), ~ dplyr::lag(., 8), .names = "{col}8")) 
  #mutate(calorie1=dplyr::lag(calorie,1,default = NA)) %>%
tmp <- restaurant[,c(1:7,10,40,43,46,49,52,55)]

### estimating individual slopes ----
master <- NULL
for (i in c(221,229,233,238,241,243,253,251,254,270,249)) {
    dollar <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>%
      do(tidy(lm(dollar~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>%
      rename(slope_dollar = estimate) 
    count <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>%
      do(tidy(lm(count~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>%
      rename(slope_count = estimate) 
    calorie <- restaurant %>% 
      group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>%
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
                            ifelse(restaurant$state=="NY"&restaurant$county=="Westchester", "westchester",
                            ifelse(restaurant$state=="MD"&restaurant$county=="Montgomery", "mont",
                            ifelse(restaurant$state=="CA", "ca",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Suffolk", "suffolk",
                            ifelse(restaurant$state=="MA", "ma",
                            ifelse(restaurant$state=="ME", "me",
                            ifelse(restaurant$state=="OR", "or",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Ulster","ulster",
                            ifelse(restaurant$state=="VT", "vt",
                            ifelse(restaurant$state=="NY"&restaurant$county=="Schenectady","schc","none"))))))))))))))
restaurant$concept <- ifelse(restaurant$concept=="TBC", 1, 0)
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY", 1, 0)

#split california
restaurant$policy2 <- ifelse(restaurant$state=="CA"&grepl("Imperial|Orange|Riverside|San Bernardino|San Diego",restaurant$county),"south-cal",
                             ifelse(restaurant$state=="CA"&grepl("Los Angeles|San Luis Obispo|Santa Barbara|Ventura",restaurant$county),"west-cal",
                             ifelse(restaurant$state=="CA"&grepl("Alpine|Calaveras|Fresno|Inyo|Kern|Kings|Madera|Mariposa|Merced|Mono|San Joaquin|Stanislaus|Tulare|Tuolumne",restaurant$county),"cen-cal",
                             ifelse(restaurant$state=="CA"&grepl("Alameda|Contra Costa|Monterey|San Benito|San Francisco|San Mateo|Santa Clara|Santa Cruz",restaurant$county),"s-valley",
                             ifelse(restaurant$state=="CA"&grepl("Amador|El Dorado|Marin|Napa|Nevada|Placer|Sacramento|Sierra|Solano|Sonoma|Sutter|Yolo|Yuba",restaurant$county),"north-cal",
                             ifelse(restaurant$state=="CA"&grepl("Butte|Colusa|Del Norte|Glenn|Humboldt|Lake|Lassen|Mendocino|Modoc|Plumas|Shasta|Siskiyou|Tehama|Trinity",restaurant$county),"jefferson",restaurant$policy))))))

#export unmatched data
#write.csv(restaurant, "data/calorie-aims/unmatched-restaurants.csv", row.names = FALSE)

### finalize diff matching+weighting, no caliper ----
# ps+iptw, mahal+entrp,mahal+sbw, entrp only
formula <- treat~concept+ownership+calorie3+slope_calorie+
  count3+slope_count+dollar3+slope_dollar+drive+meal+
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
for (i in c("ca","king","ma","mont","mult","nassau","nj","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(match_to = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) #%>%
    #dplyr::select(address:treat)
  master_all <- rbind(master_all, tmp)
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed.csv", row.names = FALSE)
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
#ignore 2 months leading to ML
#match on months t-8 to t-3
names(restaurant)
formula <- treat~concept+ownership+calorie3+slope_calorie+
  count3+slope_count+dollar3+slope_dollar+drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24
set.seed(5)
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL
for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}
master.original <- master
matched.original <- matched
restaurant2 <- restaurant
length(unique(paste0(matched.original$address[matched.original$treat==0],matched.original$match_place[matched.original$treat==0]))) #1518
length(unique(paste0(matched.original$address[matched.original$treat==1],matched.original$match_place[matched.original$treat==1]))) #506
length(unique(paste0(master.original$address[master.original$treat==0],master.original$match_place[master.original$treat==0]))) #1942
length(unique(paste0(master.original$address[master.original$treat==1],master.original$match_place[master.original$treat==1]))) #506

# trim small and large weights
while(max(matched$weights)>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0])))) {
  tmp <- matched[matched$weights>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))),]
  restaurant2 <- anti_join(restaurant2,tmp,by="address")
  matched <- NULL
  master <- NULL
  for (i in c(1:10)) {
    tryCatch({#catch groups that do not have comparison restaurants
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      #matching
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      #add distance to unmatched data
      subset$distance <- subset.match$distance
      #add ps balance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
      # combine clusters of restaurants
      master <- rbind(master, subset)
      matched <- rbind(matched, match)
    }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
  }
}
rm(i,bal, time,subset, subset.match,tmp,match)

table(matched$match_place[matched$treat==1])
length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))) #1518
length(unique(paste0(matched$address[matched$treat==1],matched$match_place[matched$treat==1]))) #506
length(unique(paste0(master$address[master$treat==0],master$match_place[master$treat==0]))) #1904
length(unique(paste0(master$address[master$treat==1],master$match_place[master$treat==1]))) #506

summary(matched$weights[matched$treat==0])
summary(matched.original$weights[matched.original$treat==0])
par(mfrow=c(2,1))
hist(matched.original$weights[matched.original$treat==0], breaks = 500,xlab = "Weight",main="Histogram of comparison units weights")
hist(matched$weights[matched$treat==0], breaks = 500,xlab = "Weight",main="After trimming")
par(mfrow=c(1,1))

#export results in matched2, combine with all months of restaurant data
names(matched)
length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
table(matched$match_place)
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x)
  master_all <- rbind(master_all, tmp)
}
write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed.csv", row.names = FALSE)
rm(master_all, i,tmp)

result <- cbind(col_w_smd(mat=subset(master.original,select = c(3:4,18:19,23,25:35,38:40,56:58,61:64)),
                          treat = master.original$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched.original, select = c(3:4,18:19,23,25:35,38:40,56:58,61:64)),
                          weights = matched.original$weights, treat = matched.original$treat, s.weights = matched.original$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,18:19,23,25:35,38:40,56:58,61:64)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- cbind(result,
                data.frame(old=row.names(result),
                           new=c("Ownership", "Joint brand",
                                 "% drive-thru transactions", "% lunch/dinner transactions",
                                 "% male", "Total population","% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65","# of transactions, t-3","Mean spending per order, t-3","Mean calorie, t-3",
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
                                            "Mean calorie, t-3", "Mean calorie trend",
                                            "# of transactions, t-3", "# of transactions trend",
                                            "Mean spending per order, t-3", "Mean spending per order trend",
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
  geom_vline(xintercept = 25.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, using trend", y="Standardized mean differences", x="",
       caption="Note: no transformations on any variables.") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/finalize/covariate-balance-after-trim.jpeg", dpi="retina")

### ps matching + iptw weighting, trim extrem weights, use absolute values instead of slope ----
#ignore 2 months leading to ML
#match on months t-8 to t-3
names(restaurant)
formula <- treat~concept+ownership+calorie3+calorie4+calorie5+calorie6+calorie7+calorie8+
  count3+count4+count5+count6+count7+count8+dollar3+dollar4+dollar5+dollar6+dollar7+dollar8+
  drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24
set.seed(5)
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","ca","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL
for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}
master.original <- master
matched.original <- matched
restaurant2 <- restaurant
length(unique(paste0(matched.original$address[matched.original$treat==0],matched.original$match_place[matched.original$treat==0]))) #1518
length(unique(paste0(matched.original$address[matched.original$treat==1],matched.original$match_place[matched.original$treat==1]))) #506
length(unique(paste0(master.original$address[master.original$treat==0],master.original$match_place[master.original$treat==0]))) #1942
length(unique(paste0(master.original$address[master.original$treat==1],master.original$match_place[master.original$treat==1]))) #506

# trim small and large weights
while(max(matched$weights)>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0])))) {
  tmp <- matched[matched$weights>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))),]
  restaurant2 <- anti_join(restaurant2,tmp,by="address")
  matched <- NULL
  master <- NULL
  for (i in c(1:10)) {
    tryCatch({#catch groups that do not have comparison restaurants
      subset <- subset(restaurant2, (treat==1&policy==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      #matching
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      #add distance to unmatched data
      subset$distance <- subset.match$distance
      #add ps balance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
      # combine clusters of restaurants
      master <- rbind(master, subset)
      matched <- rbind(matched, match)
    }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
  }
}
rm(i,bal, time,subset, subset.match,tmp,match)

table(matched$match_place[matched$treat==1])
length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))) #1518
length(unique(paste0(matched$address[matched$treat==1],matched$match_place[matched$treat==1]))) #506
length(unique(paste0(master$address[master$treat==0],master$match_place[master$treat==0]))) #1933
length(unique(paste0(master$address[master$treat==1],master$match_place[master$treat==1]))) #506

summary(matched$weights[matched$treat==0])
summary(matched.original$weights[matched.original$treat==0])
par(mfrow=c(2,1))
hist(matched.original$weights[matched.original$treat==0], breaks = 500,xlab = "Weight",main="Histogram of comparison units weights")
hist(matched$weights[matched$treat==0], breaks = 500,xlab = "Weight",main="After trimming")
par(mfrow=c(1,1))

#export results in matched2, combine with all months of restaurant data
names(matched)
length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
table(matched$match_place)
master_all <- NULL
for (i in c("ca","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x)
  master_all <- rbind(master_all, tmp)
}
write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-abs-values.csv", row.names = FALSE)
rm(master_all, i,tmp)

names(matched)
result <- cbind(col_w_smd(mat=subset(master.original,select = c(3:4,18:19,23,25:35,38:55,61:64)),
                          treat = master.original$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,32),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched.original, select = c(3:4,18:19,23,25:35,38:55,61:64)),
                          weights = matched.original$weights, treat = matched.original$treat, s.weights = matched.original$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,32),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,18:19,23,25:35,38:55,61:64)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,32),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- cbind(result,
                data.frame(old=row.names(result),
                           new=c("Ownership", "Joint brand",
                                 "% drive-thru transactions", "% lunch/dinner transactions",
                                 "% male", "Total population","% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65",
                                 "# of transactions, t-3", "Mean spending, t-3", "Calories, t-3",
                                 "# of transactions, t-4", "Mean spending, t-4", "Calories, t-4",
                                 "# of transactions, t-5", "Mean spending, t-5", "Calories, t-5",
                                 "# of transactions, t-6", "Mean spending, t-6", "Calories, t-6",
                                 "# of transactions, t-7", "Mean spending, t-7", "Calories, t-7",
                                 "# of transactions, t-8", "Mean spending, t-8", "Calories, t-8",
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
                                            "# of transactions, t-3", "# of transactions, t-4","# of transactions, t-5","# of transactions, t-6","# of transactions, t-7","# of transactions, t-8",
                                            "Mean spending, t-3","Mean spending, t-4","Mean spending, t-5","Mean spending, t-6","Mean spending, t-7","Mean spending, t-8",
                                            "Calories, t-3","Calories, t-4","Calories, t-5","Calories, t-6","Calories, t-7","Calories, t-8",
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
  geom_vline(xintercept = 37.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, using absolute values", y="Standardized mean differences", x="",
       caption="Note: no transformations on any variables.") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/finalize/covariate-balance-after-trim-abs-value.jpeg", dpi="retina")




### ps matching + iptw weighting, trim extrem weights, split california ----
names(restaurant)
formula <- treat~concept+ownership+calorie3+slope_calorie+
  count3+slope_count+dollar3+slope_dollar+drive+meal+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65+open12+open18+open24
set.seed(5)
time <- data.frame(c("king","philly","albany","mont","suffolk","ma","cen-cal","jefferson","north-cal","s-valley","south-cal","west-cal","or","vt","schc"),
                   c(229,241,243,253,251,251,253,253,253,253,253,253,253,270,249))
colnames(time)[1:2] <- c("location","time")
master <- NULL
matched <- NULL
for (i in c(1:15)) {
  tryCatch({#catch groups that do not have comparison restaurants
    subset <- subset(restaurant, (treat==1&policy2==time[i,1])|treat==0)
    subset$entry <- time[i,2]
    tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
      filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
      filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
      filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
      filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
      dplyr::select(address,open8,open12,open18,open24) %>% distinct()
    subset <- merge(subset,tmp,by="address")
    subset <- subset(subset, open8==1&monthno==time[i,2]) 
    #matching
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    # combine clusters of restaurants
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}
master.original <- master
matched.original <- matched
restaurant2 <- restaurant
length(unique(paste0(matched.original$address[matched.original$treat==0],matched.original$match_place[matched.original$treat==0]))) #1518
length(unique(paste0(matched.original$address[matched.original$treat==1],matched.original$match_place[matched.original$treat==1]))) #506
length(unique(paste0(master.original$address[master.original$treat==0],master.original$match_place[master.original$treat==0]))) #1942
length(unique(paste0(master.original$address[master.original$treat==1],master.original$match_place[master.original$treat==1]))) #506

# trim small and large weights
while(max(matched$weights)>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0])))) {
  tmp <- matched[matched$weights>=0.05*length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))),]
  restaurant2 <- anti_join(restaurant2,tmp,by="address")
  matched <- NULL
  master <- NULL
  for (i in c(1:15)) {
    tryCatch({#catch groups that do not have comparison restaurants
      subset <- subset(restaurant2, (treat==1&policy2==time[i,1])|treat==0)
      subset$entry <- time[i,2]
      tmp <- subset %>% group_by(address) %>% mutate(relative = monthno - entry) %>%
        filter(relative<0&relative>=-24) %>% mutate(n=n()) %>% mutate(open24 = ifelse(n==24, 1,0)) %>%
        filter(relative<0&relative>=-18) %>% mutate(n=n()) %>% mutate(open18 = ifelse(n==18, 1,0)) %>%
        filter(relative<0&relative>=-12) %>% mutate(n=n()) %>% mutate(open12 = ifelse(n==12, 1,0)) %>%
        filter(relative<0&relative>=-8) %>% mutate(n=n()) %>% mutate(open8 = ifelse(n==8, 1,0)) %>%
        dplyr::select(address,open8,open12,open18,open24) %>% distinct()
      subset <- merge(subset,tmp,by="address")
      subset <- subset(subset, open8==1&monthno==time[i,2]) 
      #matching
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      #add distance to unmatched data
      subset$distance <- subset.match$distance
      #add ps balance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
      # combine clusters of restaurants
      master <- rbind(master, subset)
      matched <- rbind(matched, match)
    }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
  }
}
rm(i,bal, time,subset, subset.match,tmp,match)

table(matched$match_place[matched$treat==1])
length(unique(paste0(matched$address[matched$treat==0],matched$match_place[matched$treat==0]))) #1518
length(unique(paste0(matched$address[matched$treat==1],matched$match_place[matched$treat==1]))) #506
length(unique(paste0(master$address[master$treat==0],master$match_place[master$treat==0]))) #1904
length(unique(paste0(master$address[master$treat==1],master$match_place[master$treat==1]))) #506

summary(matched$weights[matched$treat==0])
summary(matched.original$weights[matched.original$treat==0])
par(mfrow=c(2,1))
hist(matched.original$weights[matched.original$treat==0], breaks = 500,xlab = "Weight",main="Histogram of comparison units weights")
hist(matched$weights[matched$treat==0], breaks = 500,xlab = "Weight",main="After trimming")
par(mfrow=c(1,1))

#export results in matched2, combine with all months of restaurant data
names(matched)
length(unique(matched$address)) #some comparison restaurants were matched to multiple treated restaurants
table(matched$match_place)
master_all <- NULL
for (i in c("cen-cal","jefferson","north-cal","s-valley","south-cal","west-cal","king","ma","mont","or","suffolk")) {
  tmp <- matched %>%
    filter(match_place==i) %>%
    dplyr::select(address, monthno, tract_num, ownership, concept, distance, s.weights, weights, match_place) %>%
    rename(entry = monthno) %>%
    left_join(restaurant, by=c("address", "tract_num", "ownership", "concept")) %>%
    arrange(address, tract_num, monthno) %>%
    dplyr::select(c(address:above65,treat,policy)) %>%
    rename(entry = entry.x)
  master_all <- rbind(master_all, tmp)
}
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-splitCA.csv", row.names = FALSE)
rm(master_all, i,tmp)

result <- cbind(col_w_smd(mat=subset(master.original,select = c(3:4,18:19,23,25:35,38:40,56:58,62:65)),
                          treat = master.original$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched.original, select = c(3:4,18:19,23,25:35,38:40,56:58,62:65)),
                          weights = matched.original$weights, treat = matched.original$treat, s.weights = matched.original$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,18:19,23,25:35,38:40,56:58,62:65)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,20),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- cbind(result,
                data.frame(old=row.names(result),
                           new=c("Ownership", "Joint brand",
                                 "% drive-thru transactions", "% lunch/dinner transactions",
                                 "% male", "Total population","% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65","# of transactions, t-3","Mean spending per order, t-3","Mean calorie, t-3",
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
                                            "Mean calorie, t-3", "Mean calorie trend",
                                            "# of transactions, t-3", "# of transactions trend",
                                            "Mean spending per order, t-3", "Mean spending per order trend",
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
  geom_vline(xintercept = 25.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, split California", y="Standardized mean differences", x="",
       caption="Note: no transformations on any variables.") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/finalize/covariate-balance-after-trim-splitCA.jpeg", dpi="retina")

