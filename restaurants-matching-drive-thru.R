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
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","year","month","occasion", "calorie", "fat","sat_fat",
                                     "carb", "protein","sodium", "count", "dollar"))
sapply(sample07q1, class)
sample07q1[, c(5:10,12)] <- sample07q1[, c(5:10,12)]/2

calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","year","month","occasion","calorie","fat","sat_fat",
                                       "carb","protein","sodium","count","dollar"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

drive <- calorie #for merging later
calorie <- calorie %>% dplyr::select(restid,month,calorie,count,dollar)
calorie <- aggregate(data=calorie,.~restid+month,sum)
colnames(calorie)[3:5] <- c("calorie_all","count_all","dollar_all")

drive <- drive %>% filter(occasion==2) %>% dplyr::select(-year,-occasion)
drive <- aggregate(data=drive,.~restid+month,sum)
calorie <- merge(calorie,drive,by=c("restid","month"))
rm(drive,sample07q1,sample,i,j)

### add time information: month, year; keep yearno and monthno ----
time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
names(time)
time <- time[, c(7, 17, 38)]
colnames(time) <- c("month", "yearno", "monthno")
sapply(time, class)
time$yearno <- as.integer(substr(time$yearno, 2, 5))
time$monthno <- as.integer(substr(time$monthno, 6, 7))
time <- time[!duplicated(time) & time$yearno>=2006, ]

calorie <- merge(calorie, time, by="month")
colnames(calorie)[c(1,14:15)] <- c("monthno", "year", "month")
#calorie <- aggregate(data=calorie, .~year+month+restid+monthno, sum) 
calorie <- calorie[order(calorie$year, calorie$month), ]

### adding drive-thru and meal time breakdown ----
drive07 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_2007_Q1.csv",
                  stringsAsFactors=FALSE)
drive07[,c(5:10,12)] <- drive07[,c(5:10,12)]/2
meal07 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart/mean-calorie_restid_daypart_2007_Q1.csv",
                 stringsAsFactors=FALSE) 
meal07[,c(5:10,12)] <- meal07[,c(5:10,12)]/2

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
drive <- drive %>% filter(DW_OCCASION==2) %>% dplyr::select(1,3,11) %>% rename(drive=count) %>% group_by(DW_RESTID, DW_MONTH) %>% summarise(drive = sum(drive))
  
meal <- rbind(meal07, meal)
meal <- meal %>% filter(DW_DAYPART==3|DW_DAYPART==5) %>% dplyr::select(1,3,11) %>% rename(meal=count) %>%  group_by(DW_RESTID, DW_MONTH) %>% summarise(meal = sum(meal))
rm(drive07, sample, meal07, i, j)
drive <- merge(drive, meal, by=c("DW_RESTID", "DW_MONTH"))
colnames(drive)[1:2] <- c("restid", "monthno")
rm(meal)

### merge restaurant and calorie information, fix missing values, consolidate restaurants by address ----
restaurant <- merge(restaurant, calorie, by="restid")
restaurant <- restaurant[order(restaurant$address, restaurant$state, restaurant$monthno), ]
sapply(restaurant, function(x) sum(is.na(x)))
sapply(restaurant, function(x) sum(is.nan(x)))

# merge drive-thru and mealtime breakdown
restaurant <- merge(restaurant, drive, by=c("restid", "monthno"))
restaurant <- restaurant[, -c(7:8,25:28)]

#fix concept and ownership variables, recode to binary
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY",1,0)
restaurant$concept <- ifelse(restaurant$concept=="TBC",1,0)

# aggregate on address, instead of restid
tmp1 <- restaurant[, c(3:22)]
tmp1 <- tmp1[!duplicated(tmp1), ]
length(unique(paste0(restaurant$address, restaurant$tract_num)))
tmp1 <- tmp1[order(tmp1$state, tmp1$address), ]

tmp2 <- restaurant[, c(2:4,6:7,23:37)]
tmp2 <- aggregate(data=tmp2,.~tract_num+address+concept+ownership+year+month+monthno,sum)
restaurant <- merge(tmp2, tmp1, by=c("address","tract_num","concept","ownership"))
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
restaurant[, c(8,10)] <- restaurant[, c(8,10)]/restaurant$count_all
restaurant[,c(11:16,18)] <- restaurant[,c(11:16,18)]/restaurant$count

# create log vars
# replace 0 values with a small value
#restaurant <- restaurant %>% 
#  mutate(across(c(calorie,count,median_income,capital_income,total), ~ ifelse(.==0, log(.+1), log(.)), .names = "{col}"))

# take lagged measurements for dynamic vars
restaurant <- restaurant %>%
  group_by(address, tract_num, concept, ownership) %>% arrange(monthno) %>%
  mutate(across(c(count,dollar,calorie,count_all,dollar_all,calorie_all), ~ dplyr::lag(., 3), .names = "{col}3")) 

### estimating individual slopes ----
master <- NULL
for (i in c(221,229,233,238,241,243,253,251,254,270,249)) {
    dollar <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(dollar~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_dollar = estimate) 
    count <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(count~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_count = estimate) 
    calorie <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(calorie~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_calorie = estimate)
    dollar_all <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(dollar_all~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_dollar_all = estimate) 
    count_all <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(count_all~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_count_all = estimate) 
    calorie_all <- restaurant %>% group_by(address, tract_num, ownership, concept) %>%
      filter(monthno>=i-8 & monthno<=i-3) %>% do(tidy(lm(calorie_all~monthno, data = .))) %>%
      filter(!is.na(estimate)&term=="monthno") %>%
      dplyr::select(address:concept, estimate) %>% rename(slope_calorie_all = estimate)
    dollar <- merge(dollar, count, by=c("address","tract_num","ownership","concept"))
    dollar <- merge(dollar, calorie, by=c("address","tract_num","ownership","concept"))
    dollar <- merge(dollar,dollar_all,by=c("address","tract_num","ownership","concept"))
    dollar <- merge(dollar,count_all,by=c("address","tract_num","ownership","concept"))
    dollar <- merge(dollar,calorie_all,by=c("address","tract_num","ownership","concept"))
    dollar$monthno <- i
    master <- rbind(master, dollar)
}
rm(i, dollar, count, calorie,dollar_all,count_all,calorie_all)

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
#export unmatched data
#write.csv(restaurant, "data/calorie-aims/unmatched-restaurants-drive-thru.csv", row.names = FALSE)

### ps matching + iptw weighting, trim extrem weights ----
#ignore 2 months leading to ML
#match on months t-8 to t-3
names(restaurant)
formula <- treat~concept+ownership+drive+meal+
  calorie3+slope_calorie+count3+slope_count+dollar3+slope_dollar+
  calorie_all3+slope_calorie_all+count_all3+slope_count_all+dollar_all3+slope_dollar_all+
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
    print(paste0("matching is done for ", time[i,1]))
    
    #add distance to unmatched data
    subset$distance <- subset.match$distance
    #add ps balance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    print(paste0("matching is done for ", time[i,1]))
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

# trim large weights
master <- NULL
matched <- NULL
for (i in c(1:10)) {
  tryCatch({#catch groups that do not have comparison restaurants
    restaurant2 <- restaurant
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
    subset <- subset[complete.cases(subset), ]
    subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
    match <- match.data(subset.match, distance="distance", weights = "s.weights") 
    subset$distance <- subset.match$distance
    bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
    match$weights <- bal$weights
    match$match_place <- time[i,1]
    summary(match$weights[match$treat==0])
    max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))
    while(max(match$weights[match$treat==0])>=0.05*length(unique(match$address[match$treat==0]))) {
      tmp <- match[match$weights>=0.05*length(unique(match$address[match$treat==0])),]
      restaurant2 <- anti_join(restaurant2,tmp,by="address")
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
      subset <- subset[complete.cases(subset), ]
      subset.match <- matchit(data=subset,formula = formula,distance="logit",method="nearest",replace=FALSE,ratio=3)
      match <- match.data(subset.match, distance="distance", weights = "s.weights") 
      subset$distance <- subset.match$distance
      bal <- weightit(data=match, formula = formula, method = "ps",estimand = "ATT", s.weights = "s.weights")
      match$weights <- bal$weights
      match$match_place <- time[i,1]
    }
    master <- rbind(master, subset)
    matched <- rbind(matched, match)
    rm(subset,subset.match,tmp,match)
  }, error=function(e){cat(paste0("ERROR : ",time[i,1]),conditionMessage(e), "\n")})
}
rm(restaurant2,time,tmp,i,subset,subset.match,bal,match)

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
#write.csv(master_all, "data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", row.names = FALSE)
rm(master_all, i,tmp)

result <- cbind(col_w_smd(mat=subset(master.original,select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          treat = master.original$treat,
                          std = TRUE, bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched.original, select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          weights = matched.original$weights, treat = matched.original$treat, s.weights = matched.original$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)),
                col_w_smd(mat=subset(matched, select = c(3:4,15:16,20,22:32,35:46,49:52)),
                          weights = matched$weights, treat = matched$treat, s.weights = matched$s.weights,
                          std = TRUE,bin.vars = c(rep(TRUE,2),rep(FALSE,26),rep(TRUE,3),FALSE)))
colnames(result)[1:3] <- c("pre", "ps_weight","ps_weight_trim")
result <- data.frame(cbind(result,label=c("Ownership", "Joint brand","% drive-through transactions", "% lunch/dinner transactions",
                             "% male", "Total population","% white", "% Black", "% Asian",
                             "% Hispanic", "Household median income", "Income per capita",
                             "% without HS degree", "% has college degree and up","% under 18","% above 65",
                             "# of drive-through transactions, t-3","Drive-through mean spending, t-3","Drive-through mean calorie, t-3",
                             "# of transactions, t-3","Mean spending, t-3","Mean calorie, t-3",
                             "Drive-through mean spending trend", "# of drive-through transactions trend", "Drive-through mean calorie trend",
                             "Mean spending trend", "# of transactions trend", "Mean calorie trend",
                             "Has 12-month baseline data","Has 18-month baseline data","Has 24-month baseline data","Distance")))
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:3]), v.names = "score",
                  idvar = "label",
                  timevar = "method", times = c("pre","ps_weight","ps_weight_trim"))
result$score <- as.numeric(result$score)

result$label <- factor(result$label,
                       levels=c("Distance","Ownership", "Joint brand","Has 12-month baseline data","Has 18-month baseline data","Has 24-month baseline data",
                                "# of drive-through transactions, t-3","Drive-through mean spending, t-3","Drive-through mean calorie, t-3",
                                "# of transactions, t-3","Mean spending, t-3","Mean calorie, t-3",
                                "Drive-through mean spending trend", "# of drive-through transactions trend", "Drive-through mean calorie trend",
                                "Mean spending trend", "# of transactions trend", "Mean calorie trend",
                                "% drive-through transactions", "% lunch/dinner transactions",
                                "Total population","% male", "% white", "% Black", "% Asian",
                                "% Hispanic", "Household median income", "Income per capita",
                                "% without HS degree", "% has college degree and up","% under 18","% above 65"))
result$method <- factor(result$method, levels = c("pre","ps_weight","ps_weight_trim"))

ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.5, linetype="dashed") +
  #geom_vline(xintercept = 25.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  labs(title="Covariate balance, match drive-thru data", y="Standardized mean differences", x="", caption="") +
  scale_color_manual(name="Sample", labels=c("Unmatched","PS+IPTW", "PS+IPTW, trimmed"),
                     values =c("orange", "aquamarine3","violet")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/match-drive-thru/covariate-balance-after-trim.jpeg", dpi="retina")

#for paper
ggplot(data = result%>%filter(method!="ps_weight"),
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.25, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = -0.25, color = "red", size = 0.5, linetype="dashed") +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  scale_y_continuous(limits = c(-1,3),breaks=c(-1,-0.25,0,0.25,1,2,3)) + 
  labs(y="Standardized mean differences", x="") +
  scale_color_manual(name="Sample", labels=c("Unmatched","Matched"), values =c("orange", "aquamarine3")) +
  theme_bw() + theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("manuscript/figures/covariate-balance.jpeg", dpi="retina")







