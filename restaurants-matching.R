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

tmp1 <- restaurant[, c(2:4, 9:23)]
tmp1 <- tmp1[!duplicated(tmp1), ]
length(unique(paste0(restaurant$address, restaurant$tract_num)))
tmp1 <- tmp1[order(tmp1$state, tmp1$address), ]

tmp2 <- restaurant[, c(2:3, 5:8, 28:39)]
tmp2 <- aggregate(data=tmp2,
                  .~tract_num+address+concept+drive_thru+drive_thru_type+ownership+year+month+yearno+monthno,
                  sum)
restaurant <- merge(tmp2, tmp1, by=c("address", "tract_num"))
rm(tmp1, tmp2)

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
                                 ((restaurant$year>=2010)|((restaurant$year==2009)&
                                 restaurant$month>=11)), 1,
                        ifelse(restaurant$state=="VT"&(restaurant$year>=2013|
                                (restaurant$year==2012&restaurant$month>=6)), 1, 0))))))))))))))))
rm(acs, calorie, time)
table(restaurant$state[restaurant$ml==1])

### prepare data for rolling entry matching ----
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

# take lagged measurements for dynamic vars
restaurant <- restaurant[order(restaurant$state, restaurant$address, restaurant$monthno), ]
restaurant <- restaurant %>%
  group_by(address, tract_num, concept, ownership) %>%
  mutate(calorie1 = lag(calorie, 1)) %>%
  mutate(calorie2 = lag(calorie, 2)) %>%
  mutate(calorie3 = lag(calorie, 3)) %>%
  mutate(count1 = lag(count, 1)) %>%
  mutate(count2 = lag(count, 2)) %>%
  mutate(count3 = lag(count, 3)) %>%
  mutate(dollar1 = lag(dollar, 1)) %>%
  mutate(dollar2 = lag(dollar, 2)) %>%
  mutate(dollar3 = lag(dollar, 3)) 

restaurant$concept <- as.factor(restaurant$concept)
restaurant$ownership <- as.factor(restaurant$ownership)
restaurant_subset <- subset(restaurant, #select=-c(24:27),
                            subset=(!is.na(calorie1)&!is.na(calorie2)&!is.na(calorie3)&
                                      !is.na(count1)&!is.na(count2)&!is.na(count3)&
                                      !is.na(dollar2)&!is.na(dollar2)&!is.na(dollar3)))
restaurant_subset$concept <- ifelse(restaurant_subset$concept=="TBC", 0, 1)
restaurant_subset$ownership <- ifelse(restaurant_subset$ownership=="COMPANY", 1, 0)

# check NA values
colnames(restaurant_subset)[colSums(is.na(restaurant_subset)) > 0]

# pre-matching summary stats
# by city/state, N restaurants and N restid-month
tmp <- restaurant[(restaurant$treat==1)&(restaurant$monthno==restaurant$entry), c(1,3:4,9:10,41)]
tmp$n <- 1
table(tmp$county[tmp$state!="CA"], tmp$state[tmp$state!="CA"])

temp <- aggregate(data=tmp[tmp$state=="NY",], n~state+county+entry, FUN=length)
rm(temp, tmp)

### rolling entry matching ----
#reduce input data for matching
reduced_data <- reduce_data(data=restaurant_subset,
                            treat="treat", tm="monthno", entry="entry",
                            id="restid", lookback=1)

#select variables for matching
#calculate propsensity scores for each observation
names(reduced_data)
formula <- treat~concept+drive_thru+ownership+male+
  total+white+black+asian+hisp+median_income+capital_income+hsbelow+collegeup+
  under18+above65+calorie+calorie1+calorie2+count+count1+count2+dollar+dollar1+dollar2

scored_data <- score_data(reduced_data = reduced_data, model_type = "logistic",
                          match_on = "pscore", treat="treat",
                          tm="monthno", entry="entry", id="restid",
                          fm=as.formula(formula))

# apply rolling entry mathcing algorithm
matched_data <- rollmatch(scored_data = scored_data, data=restaurant_subset,
                          treat="treat",
                          tm="monthno", entry="entry", id="restid",
                          vars=all.vars(as.formula(formula)),
                          lookback = 1, alpha = 0.2,
                          standard_deviation = "average", num_matches = 5,
                          replacement = TRUE)
names(matched_data)
sapply(matched_data, class)
summary(matched_data)

matched <- matched_data$matched_data
matched <- matched[order(matched$treat_id, matched$match_rank),
                   c(3,1,10,9,12,2,4:5,8,6:7,11,13)]
matched <- merge(matched, restaurant[, c(1, 3, 4, 9, 10)], by.x="treat_id", by.y="restid")
table(matched$state)
table(matched$county[matched$state=="NY"]) #restid in nyc has no matches

hist(matched$control_matches, breaks=100,
     main="", xlab="Number of times a comparison restaurant was used")

length(matched$treat_id)
length(matched$control_id)

### manual propsensity score matching ----
#specify matching formula
formula <- treat~concept+drive_thru+ownership+calorie1+calorie2+calorie3+
  count1+count2+count3+dollar1+dollar2+dollar3+
  total+male+white+black+asian+hisp+median_income+capital_income+
  hsbelow+collegeup+under18+above65

master <- NULL
master_matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
  subset <- subset(restaurant_subset, entry==i & monthno==i)
  subset.match <- matchit(data=subset, formula = formula, 
                          distance="logit", method="nearest", 
                          caliper=0.2, #calclosest=TRUE, subclass=20,
                          replace=FALSE, ratio=1)
  subset.matched <- match.data(subset.match, distance="distance") # create a dataset with matched results
  subset <- cbind(subset, subset.match$distance)
  
  # combine clusters of restaurants
  master <- rbind(master, subset)
  master_matched <- rbind(master_matched, subset.matched)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
colnames(master)[47] <- "distance"
rm(i, subset, subset.match, subset.matched)

# calculate standardized mean differences manually
names(master)
result <- as.data.frame(col_w_smd(mat=subset(master,
                             select = c(3:4,6,22,24:34,38:47)),
                  treat = master$treat, weights = NULL,
                  std = TRUE,
                  bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
result2 <- as.data.frame(col_w_smd(mat=subset(master_matched, select = c(3:4,6,22,24:34,38:47)),
                   treat = master_matched$treat, weights = master_matched$weights,
                   std = TRUE,
                   bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))

colnames(result)[1] <- "pre"
colnames(result2)[1] <- "post"
result <- cbind(result, result2, 
                data.frame(old=c("concept", "drive_thru", "ownership", "male", "total",
                                 "white", "black", "asian", "hisp","median_income","capital_income",
                                 "hsbelow","collegeup","under18","above65",
                                 "calorie1","calorie2","calorie3","count1","count2","count3",
                                 "dollar1","dollar2","dollar3","distance"),
                           new=c("Joint brand","Has drive through", "Ownership",
                                 "% male","Total population", "% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65", 
                                 "Mean calorie, t-1", "Mean calorie, t-2", "Mean calorie, t-3",
                                 "# of transactions, t-1", "# of transactions, t-2", "# of transactions, t-3",
                                 "Mean spending per order, t-1", "Mean spending per order, t-2",
                                 "Mean spending per order, t-3",
                                 "Distance")))
names(result)
result <- reshape(result, direction = "long",
               varying = list(names(result)[1:2]), v.names = "score",
               idvar = c("old", "new"),
               timevar = "method", times = c("pre", "post"))
rm(result2)

# replicate love.plot
result$label <- factor(result$new, levels=c("Distance", "Joint brand", "Has drive through",
                                      "Ownership",
                                      "Mean calorie, t-1", 
                                      "Mean calorie, t-2", "Mean calorie, t-3", "# of transactions, t-1",
                                      "# of transactions, t-2", "# of transactions, t-3",
                                      "Mean spending per order, t-1", "Mean spending per order, t-2",
                                      "Mean spending per order, t-3",
                                      "Total population", "% male", "% white", "% Black", "% Asian",
                                      "% Hispanic", "Household median income", "Income per capita",
                                      "% without HS degree", "% has college degree and up",
                                      "% under 18", "% above 65"))

#visualization
ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_hline(yintercept = -0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_vline(xintercept = 24.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  #scale_y_continuous(limits = c(-1, 3)) +
  labs(title="Covariate balance, Mahalanobis distance",
       y="Standardized mean differences", x="",
       caption="Note: matching ratio 1:1, without replacement, using Mahalanobis distance") +
  scale_color_discrete(name="Sample", labels=c("Matched", "Unmatched")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/covariate-balance-mahalanobis.jpeg", dpi="retina")

#summary stats
#before matching, all restid-month rows
length(unique(restaurant$restid))
length(unique(restaurant$restid[restaurant$treat==1]))
length(unique(restaurant$restid[restaurant$treat==0]))

#after matching, with matched units
table(master_matched$treat)
length(unique(master_matched$restid))
length(unique(master_matched$restid[master_matched$treat==1]))
length(unique(master_matched$restid[master_matched$treat==0]))
tmp <- merge(master_matched, restaurant, by="restid")
rm(tmp)

### CBPS, covariate balancing ----
master.cbps <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    subset.match <- CBPS(data=subset, formula = formula, replace=FALSE, ratio=1,
                         iterations = 1000, standardize = TRUE, method = "over",
                         twostep = TRUE)
    subset <- cbind(subset, subset.match$fitted.values, subset.match$weights) #add pscore and weights
    colnames(subset)[48:49] <- c("distance", "weights")
    # combine clusters of restaurants
    master.cbps <- rbind(master.cbps, subset) #master data before matching and balancing, with pscore and weights
  }, error=function(e){cat(paste0("ERROR: ", i), conditionMessage(e), "\n")})
}
rm(i, subset, subset.match)

# calculate standardized mean differences manually
names(master.cbps)
result.cbps <- as.data.frame(col_w_smd(mat=subset(master.cbps,
                                             select = c(5:6,8,11,13:23,39:48)),
                                  treat = master.cbps$treat, weights = NULL,
                                  std = TRUE,
                                  bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
result2.cbps <- as.data.frame(col_w_smd(mat=subset(master.cbps,
                                                   select = c(5:6,8,11,13:23,39:48)),
                                   treat = master.cbps$treat, weights = master.cbps$weights,
                                   std = TRUE,
                                   bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
colnames(result.cbps)[1] <- "pre"
colnames(result2.cbps)[1] <- "post"
result.cbps <- cbind(result.cbps, result2.cbps, 
                data.frame(old=c("concept", "drive_thru", "ownership", "male", "total",
                                 "white", "black", "asian", "hisp","median_income","capital_income",
                                 "hsbelow","collegeup","under18","above65",
                                 "calorie1","calorie2","calorie3","count1","count2","count3",
                                 "dollar1","dollar2","dollar3","distance"),
                           new=c("Joint brand","Has drive through", "Ownership",
                                 "% male","Total population", "% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65", 
                                 "Mean calorie, t-1", "Mean calorie, t-2", "Mean calorie, t-3",
                                 "# of transactions, t-1", "# of transactions, t-2", "# of transactions, t-3",
                                 "Mean spending per order, t-1", "Mean spending per order, t-2",
                                 "Mean spending per order, t-3",
                                 "Distance")))
names(result.cbps)
result.cbps <- reshape(result.cbps, 
                  direction = "long",
                  varying = list(names(result.cbps)[1:2]),
                  v.names = "score",
                  idvar = c("old", "new"),
                  timevar = "method",
                  times = c("pre", "post"))
rm(result2.cbps)

# replicate love.plot
result.cbps$label <- factor(result.cbps$new, levels=c("Distance", "Joint brand", "Has drive through",
                                            "Ownership",
                                            "Mean calorie, t-1", 
                                            "Mean calorie, t-2", "Mean calorie, t-3", "# of transactions, t-1",
                                            "# of transactions, t-2", "# of transactions, t-3",
                                            "Mean spending per order, t-1", "Mean spending per order, t-2",
                                            "Mean spending per order, t-3",
                                            "Total population", "% male", "% white", "% Black", "% Asian",
                                            "% Hispanic", "Household median income", "Income per capita",
                                            "% without HS degree", "% has college degree and up",
                                            "% under 18", "% above 65"))

#visualization
ggplot(data = result.cbps,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "red", size = 0.1, linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.1, linetype="dashed") +
  geom_vline(xintercept = 24.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  #scale_y_continuous(limits = c(-1, 3)) +
  labs(title="Covariate balance, CBPS", y="Standardized mean differences", x="",
       caption="Note: matching ratio 1:1, without replacement, find nearest match") +
  scale_color_discrete(name="Sample", labels=c("Matched", "Unmatched")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/covariate-balance-cbps.jpeg", dpi="retina")

#summary stats
#after matching
length(unique(master.cbps$restid))
length(unique(master.cbps$restid[master.cbps$treat==1]))
length(unique(master.cbps$restid[master.cbps$treat==0]))
tmp <- merge(master_matched, restaurant, by="restid")
rm(tmp)






















### testing manually calculate smd ----
#calculate manually
(mean(subset$under18[subset$treat==1]) - mean(subset$under18[subset$treat==0]))/
  sqrt(((sd(subset$under18[subset$treat==1]))^2+(sd(subset$under18[subset$treat==0]))^2)/2)
(mean(subset$ownership[subset$treat==1]) - mean(subset$ownership[subset$treat==0]))/
  sqrt((mean(subset$ownership[subset$treat==1])*(1-mean(subset$ownership[subset$treat==1]))+mean(subset$ownership[subset$treat==0])*(1-mean(subset$ownership[subset$treat==0])))/2)

col_w_smd(mat=subset(subset, select = c(8,22)),
          treat = subset$treat, weights = NULL,
          std = TRUE)

### propensity score matching, optimal matching ----
master <- NULL
master_matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    subset.match <- matchit(data=subset,
                            formula = formula, caliper=0.2,
                            ratio=1, distance="logit", method="optimal", replace=FALSE)
    subset.matched <- match.data(subset.match, distance="distance") # create a dataset with matched results
    subset <- cbind(subset, subset.match$distance)
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    master_matched <- rbind(master_matched, subset.matched)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
colnames(master)[48] <- "distance"
rm(i, subset, subset.match, subset.matched)

# calculate standardized mean differences manually
names(master)
result <- as.data.frame(col_w_smd(mat=subset(master,
                                             select = c(5:6,8,11,13:23,39:48)),
                                  treat = master$treat, weights = NULL,
                                  std = TRUE,
                                  bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))

result2 <- as.data.frame(col_w_smd(mat=subset(master_matched, select = c(5:6,8,11,13:23,39:48)),
                                   treat = master_matched$treat, weights = master_matched$weights,
                                   std = TRUE,
                                   bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))

colnames(result)[1] <- "pre"
colnames(result2)[1] <- "post"
result <- cbind(result, result2, 
                data.frame(old=c("concept", "drive_thru", "ownership", "male", "total",
                                 "white", "black", "asian", "hisp","median_income","capital_income",
                                 "hsbelow","collegeup","under18","above65",
                                 "calorie1","calorie2","calorie3","count1","count2","count3",
                                 "dollar1","dollar2","dollar3","distance"),
                           new=c("Joint brand","Has drive through", "Ownership",
                                 "% male","Total population", "% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65", 
                                 "Mean calorie, t-1", "Mean calorie, t-2", "Mean calorie, t-3",
                                 "# of transactions, t-1", "# of transactions, t-2", "# of transactions, t-3",
                                 "Mean spending per order, t-1", "Mean spending per order, t-2",
                                 "Mean spending per order, t-3",
                                 "Distance")))
names(result)
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:2]), v.names = "score",
                  idvar = c("old", "new"),
                  timevar = "method", times = c("pre", "post"))
rm(result2)

# replicate love.plot
result$label <- factor(result$new, levels=c("Distance", "Joint brand", "Has drive through",
                                            "Ownership",
                                            "Mean calorie, t-1", 
                                            "Mean calorie, t-2", "Mean calorie, t-3", "# of transactions, t-1",
                                            "# of transactions, t-2", "# of transactions, t-3",
                                            "Mean spending per order, t-1", "Mean spending per order, t-2",
                                            "Mean spending per order, t-3",
                                            "Total population", "% male", "% white", "% Black", "% Asian",
                                            "% Hispanic", "Household median income", "Income per capita",
                                            "% without HS degree", "% has college degree and up",
                                            "% under 18", "% above 65"))

#visualization
ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point() +
  geom_hline(yintercept = 0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_hline(yintercept = -0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_vline(xintercept = 24.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  #scale_y_continuous(limits = c(-1, 3)) +
  labs(title="Covariate balance, PS matching, optimal",
       y="Standardized mean differences", x="",
       caption="Note: matching ratio 1:1, without replacement, find optimal match") +
  scale_color_discrete(name="Sample", labels=c("Matched", "Unmatched")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/covariate-balance-ps-optimal.jpeg", dpi="retina")

#summary stats
#before matching
length(unique(restaurant$restid))
length(unique(restaurant$restid[restaurant$treat==1]))
length(unique(restaurant$restid[restaurant$treat==0]))

#after matching
table(master_matched$treat)
length(unique(master_matched$restid))
length(unique(master_matched$restid[master_matched$treat==1]))
length(unique(master_matched$restid[master_matched$treat==0]))
tmp <- merge(master_matched, restaurant, by="restid")
rm(tmp)


### sanity checks, why some restaurants didnt have any match ----
tmp <- restaurant %>%
  dplyr::filter(state=="ME") %>%
  dplyr::select(address, state, year, month, open, close)
table(tmp$month, tmp$year)

#after reducing to obs at time of labeling implementation
check <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i & treat==1)
    check <- rbind(check, subset)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
rm(subset, i)

table(check$state)
table(check$city[check$state=="NY"])


### combine ps nearest, subclass and optimal ----
master <- NULL
master_matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="nearest", 
                            caliper=0.2, #calclosest=TRUE, subclass=20,
                            replace=FALSE, ratio=1)
    subset.matched <- match.data(subset.match, distance="distance") # create a dataset with matched results
    subset <- cbind(subset, subset.match$distance)
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    master_matched <- rbind(master_matched, subset.matched)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
colnames(master)[48] <- "distance"

# calculate standardized mean differences manually
names(master)
result.pre <- as.data.frame(col_w_smd(mat=subset(master,
                                             select = c(3:4,6,22,24:34,38:47)),
                                  treat = master$treat, weights = NULL,
                                  std = TRUE,
                                  bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
result.nearest <- as.data.frame(col_w_smd(mat=subset(master_matched, select = c(3:4,6,22,24:34,38:47)),
                                   treat = master_matched$treat, weights = master_matched$weights,
                                   std = TRUE,
                                   bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
master <- NULL
master_matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="nearest", 
                            caliper=0.2, subclass=20,
                            replace=FALSE, ratio=1)
    subset.matched <- match.data(subset.match, distance="distance") # create a dataset with matched results
    subset <- cbind(subset, subset.match$distance)
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    master_matched <- rbind(master_matched, subset.matched)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
colnames(master)[48] <- "distance"
result.subclass <- as.data.frame(col_w_smd(mat=subset(master_matched, select = c(3:4,6,22,24:34,38:47)),
                                          treat = master_matched$treat, weights = master_matched$weights,
                                          std = TRUE,
                                          bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))

master <- NULL
master_matched <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tryCatch({ #catch groups that do not have comparison restaurants
    subset <- subset(restaurant_subset, entry==i & monthno==i)
    subset.match <- matchit(data=subset, formula = formula, 
                            distance="logit", method="optimal", 
                            caliper=0.2,
                            replace=FALSE, ratio=1)
    subset.matched <- match.data(subset.match, distance="distance") # create a dataset with matched results
    subset <- cbind(subset, subset.match$distance)
    
    # combine clusters of restaurants
    master <- rbind(master, subset)
    master_matched <- rbind(master_matched, subset.matched)
  }, error=function(e){cat(paste0("ERROR : month ", i),conditionMessage(e), "\n")})
}
colnames(master)[48] <- "distance"
result.op <- as.data.frame(col_w_smd(mat=subset(master_matched, select = c(3:4,6,22,24:34,38:47)),
                                           treat = master_matched$treat, weights = master_matched$weights,
                                           std = TRUE,
                                           bin.vars = c(rep(TRUE, 3), rep(FALSE, 22))))
result <- cbind(result.pre, result.nearest, result.subclass, result.op,
                data.frame(old=c("concept", "drive_thru", "ownership", "male", "total",
                                 "white", "black", "asian", "hisp","median_income","capital_income",
                                 "hsbelow","collegeup","under18","above65",
                                 "calorie1","calorie2","calorie3","count1","count2","count3",
                                 "dollar1","dollar2","dollar3","distance"),
                           new=c("Joint brand","Has drive through", "Ownership",
                                 "% male","Total population", "% white", "% Black", "% Asian",
                                 "% Hispanic", "Household median income", "Income per capita",
                                 "% without HS degree", "% has college degree and up",
                                 "% under 18", "% above 65", 
                                 "Mean calorie, t-1", "Mean calorie, t-2", "Mean calorie, t-3",
                                 "# of transactions, t-1", "# of transactions, t-2", "# of transactions, t-3",
                                 "Mean spending per order, t-1", "Mean spending per order, t-2",
                                 "Mean spending per order, t-3",
                                 "Distance")))
rm(i, subset, subset.match, subset.matched)

names(result)
colnames(result)[1:4] <- c("pre", "nearest", "subclass", "optimal")
result <- reshape(result, direction = "long",
                  varying = list(names(result)[1:4]), v.names = "score",
                  idvar = c("old", "new"),
                  timevar = "method", times = c("pre", "nearest", "subclass", "optimal"))
rm(result.nearest, result.subclass, result.op, result.pre)

# replicate love.plot
result$label <- factor(result$new, levels=c("Distance", "Joint brand", "Has drive through",
                                            "Ownership",
                                            "Mean calorie, t-1", 
                                            "Mean calorie, t-2", "Mean calorie, t-3", "# of transactions, t-1",
                                            "# of transactions, t-2", "# of transactions, t-3",
                                            "Mean spending per order, t-1", "Mean spending per order, t-2",
                                            "Mean spending per order, t-3",
                                            "Total population", "% male", "% white", "% Black", "% Asian",
                                            "% Hispanic", "Household median income", "Income per capita",
                                            "% without HS degree", "% has college degree and up",
                                            "% under 18", "% above 65"))
result$method <- factor(result$method, levels=c("pre", "nearest", "subclass", "optimal"))

#visualization
ggplot(data = result,
       mapping = aes(x = fct_rev(label), y = score, group= method, color=method)) +
  geom_point(pch=1) +
  geom_hline(yintercept = 0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_hline(yintercept = -0.25, color = "red", size = 0.1, linetype="dashed") +
  geom_vline(xintercept = 24.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.1) +
  coord_flip() +
  #scale_y_continuous(limits = c(-1, 3)) +
  labs(title="Covariate balance, propensity score matching",
       y="Standardized mean differences", x="",
       caption="Note: matching ratio 1:1, without replacement") +
  scale_color_manual(name="Sample", labels=c("Unmatched", "Nearest", "Subclass", "Optimal"),
                       values =c("orange", "blueviolet", "hot pink", "aquamarine3")) +
  theme_bw() +
  theme(legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/ps-matching/covariate-balance-overall.jpeg", dpi="retina")
