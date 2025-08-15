### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)
dev.off()

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(plm)
library(lmerTest)
library(stargazer)
library(table1)
library(tableone)
library(broom)
library(car)
library(usmap)
library(maps)
library(car) #testing joint significance
library(zoo)

### clean order-type data ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid", "yearno", "monthno","occasion", "calorie", "fat",
                                     "sat_fat", "carb", "protein", "sodium", "count", "dollar"))
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
                           col.names=c("restid", "yearno", "monthno","occasion", "calorie", "fat", "sat_fat",
                                       "carb", "protein", "sodium", "count", "dollar"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,calorie,by="restid")

# aggregate on address, instead of restid
restaurant$restid <- NULL
restaurant$yearno <- NULL
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY",1,0)
restaurant$concept <- ifelse(restaurant$concept=="TBC",1,0)
restaurant <- aggregate(data=restaurant, .~address+concept+ownership+monthno+occasion, sum)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant[,c(6:11,13)] <- restaurant[,c(6:11,13)]/restaurant$count

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(13:14,16:22)] #delete transaction data related to overall calorie info
colnames(matched)[13] <- "calorie_overall"
occasion <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
rm(calorie,restaurant,matched)

### preparing data, order-type ----
occasion$tract_num <- substr(occasion$tract_num, 2, 12)
occasion <- occasion %>%
  filter(complete.cases(occasion)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:dollar) %>%
  arrange(id, occasion, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat, match_place, occasion) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(occasion$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
occasion$relative <- occasion$monthno - occasion$entry +1
occasion$relative2 <- occasion$monthno - occasion$entry #month 0 is first month of ML
occasion$post <- ifelse(occasion$relative2<0, 0, 1)

# month as relative and factor
# set month 1 as ref group
occasion$relative.factor <- factor(occasion$relative)
occasion <- within(occasion, relative.factor<-relevel(relative.factor, ref="1"))
summary(occasion$relative)
occasion$relative2.factor <- factor(occasion$relative2)
occasion <- within(occasion, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
occasion <- occasion %>%
  group_by(id, treat, match_place, post,occasion) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
occasion$open_before <- ifelse(occasion$post==0, occasion$open_before, occasion$open_month-occasion$open_before)
occasion$open_after <- ifelse(occasion$post==1, occasion$open_after, occasion$open_month-occasion$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- occasion %>%
  group_by(id, treat, match_place,occasion) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24,occasion) %>%
  distinct()
occasion <- merge(occasion, tmp, by=c("id", "treat", "match_place","occasion"), all = TRUE)
tmp <- occasion %>%
  group_by(id, treat, match_place,occasion) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24,occasion) %>%
  distinct()
occasion <- merge(occasion, tmp, by=c("id", "treat", "match_place","occasion"), all = TRUE)
occasion$open6 <- ifelse(occasion$before6==1&occasion$after6==1,1,0)
occasion$open12 <- ifelse(occasion$before12==1&occasion$after12==1,1,0)
occasion$open18 <- ifelse(occasion$before18==1&occasion$after18==1,1,0)
occasion$open24 <- ifelse(occasion$before24==1&occasion$after24==1,1,0)
rm(tmp)

occasion <- within(occasion, relative2.factor<-relevel(relative2.factor, ref="-3"))
occasion$id_match <- paste0(occasion$id, occasion$match_place)

### main analysis, calorie=treat*month, order type ----
tidy_mod.factor_all <- NULL
for (i in 1:3) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = occasion%>%filter((relative2<=-3|(relative2>=2&relative2<=55))&occasion==i), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
  colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
  tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat <- 0
  tidy_mod.factor$group <- 0
  dim(tidy_mod.factor)
  tidy_mod.factor[201:202, 1] <- "0" #add 2 rows for month 0
  tidy_mod.factor[201:202, c(2,4)] <- 0 #add coef.month and coef.treat
  tidy_mod.factor[201:202, 5] <- c(0, 1) #add treat=0 and treat=1
  tidy_mod.factor[, 1] <- c(seq(-49,-4,1),seq(3,56,1),seq(-49,-4, 1),seq(3,56, 1),-3,-3) #change month numbers
  tidy_mod.factor$group[101:200] <- 1 #change group to treat=1
  tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat[102:202] <- tidy_mod.factor$coef.month[102:202]
  tidy_mod.factor$coef.month[102:202] <- tidy_mod.factor$coef.month[1:101]
  tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                    tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
  tidy_mod.factor$diff <- NA
  tidy_mod.factor$diff[1:101] <- tidy_mod.factor$calorie[102:202] - tidy_mod.factor$calorie[1:101]
  tidy_mod.factor$p.diff <- NA
  tidy_mod.factor$p.diff[1:101] <- tidy_mod.factor$p[102:202]
  tidy_mod.factor$occasion <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
  tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]
}

ggplot(data=tidy_mod.factor_all,aes(x=month,y=diff,group=factor(occasion),color=factor(occasion))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-150, ymax=100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-75, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-150,100),breaks=seq(-150,100,25)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, by order type", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Order type",labels=c("Eat-in","Drive-through","Takeout"),
                     values=c("hotpink","olivedrab3","#13B0E4")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/main-effect-orderType.jpeg", dpi="retina")

### detect trend using diff-in-diff, order type ----
tmp1 <- tidy_mod.factor_all %>% group_by(occasion) %>%
  filter(month>=-30&month<0) %>% dplyr::select(month, diff, occasion) %>%
  mutate(month = -month) %>% arrange(occasion,month) %>%
  mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_all %>% group_by(occasion) %>%
  filter(month>=1&month<=30) %>% dplyr::select(month, diff, occasion) %>%
  arrange(occasion,month) %>%
  rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by=c("month","occasion")) %>%
  group_by(occasion,month) %>%
  arrange(occasion,month) %>%
  mutate(mean = post_mean - pre_mean) %>% select(-diff)
rm(tmp1,tmp2)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(occasion))) + 
  #geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color="hotpink", group=1)) +
  #ggplot2::annotate(geom="label", x=6, y=-30, label="   p<0.05", size=3) + 
  #geom_point(aes(x=5.35,y=-30),color="hotpink",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by otder type", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Ordery type", labels=c("Eat-in","Drive-through","Takeout"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/diff-in-diff-orderType.jpeg", dpi="retina")


### clean meal-time data ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart/mean-calorie_restid_daypart_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid", "yearno", "monthno","meal", "calorie", "fat",
                                     "sat_fat", "carb", "protein", "sodium", "count", "dollar"))
sample07q1[, c(5:10,12)] <- sample07q1[, c(5:10,12)]/2 
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart/mean-calorie_restid_daypart_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid", "yearno", "monthno","meal", "calorie", "fat", "sat_fat",
                                       "carb", "protein", "sodium", "count", "dollar"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,calorie,by="restid")

# aggregate on address, instead of restid
restaurant$restid <- NULL
restaurant$yearno <- NULL
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY",1,0)
restaurant$concept <- ifelse(restaurant$concept=="TBC",1,0)
restaurant <- aggregate(data=restaurant, .~address+concept+ownership+monthno+meal, sum)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant[,c(6:11,13)] <- restaurant[,c(6:11,13)]/restaurant$count

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(13:14,16:22)] #delete transaction data related to overall calorie info
colnames(matched)[13] <- "calorie_overall"
meal <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
rm(calorie,restaurant,matched)
### preparing data, meal-time ----
meal$tract_num <- substr(meal$tract_num, 2, 12)
meal <- meal %>% filter(complete.cases(meal)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:dollar) %>% rename(meal_pct = meal.x, meal = meal.y) %>%
  arrange(id, meal, monthno) %>% group_by(id, treat, match_place, meal) %>%
  mutate(rank=row_number(id)) %>% mutate(open_month=max(rank))
summary(meal$rank) #sanity check, the max should be 106
meal$relative <- meal$monthno - meal$entry +1
meal$relative2 <- meal$monthno - meal$entry #month 0 is first month of ML
meal$post <- ifelse(meal$relative2<0, 0, 1)
meal$relative.factor <- factor(meal$relative)
meal <- within(meal, relative.factor<-relevel(relative.factor, ref="1"))
summary(meal$relative)
meal$relative2.factor <- factor(meal$relative2)
meal <- within(meal, relative2.factor<-relevel(relative2.factor, ref="0"))
meal <- meal %>%
  group_by(id, treat, match_place, post,meal) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
meal$open_before <- ifelse(meal$post==0, meal$open_before, meal$open_month-meal$open_before)
meal$open_after <- ifelse(meal$post==1, meal$open_after, meal$open_month-meal$open_after)
tmp <- meal %>%
  group_by(id, treat, match_place,meal) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24,meal) %>%
  distinct()
meal <- merge(meal, tmp, by=c("id", "treat", "match_place","meal"), all = TRUE)
tmp <- meal %>%
  group_by(id, treat, match_place,meal) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24,meal) %>%
  distinct()
meal <- merge(meal, tmp, by=c("id", "treat", "match_place","meal"), all = TRUE)
meal$open6 <- ifelse(meal$before6==1&meal$after6==1,1,0)
meal$open12 <- ifelse(meal$before12==1&meal$after12==1,1,0)
meal$open18 <- ifelse(meal$before18==1&meal$after18==1,1,0)
meal$open24 <- ifelse(meal$before24==1&meal$after24==1,1,0)
rm(tmp)
meal <- within(meal, relative2.factor<-relevel(relative2.factor, ref="-3"))
meal$id_match <- paste0(meal$id, meal$match_place)

### main analysis, calorie=treat*month, order type ----
tidy_mod.factor_all <- NULL
for (i in 1:6) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = meal%>%filter((relative2<=-3|(relative2>=2&relative2<=55))&meal==i), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
  colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
  tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat <- 0
  tidy_mod.factor$group <- 0
  dim(tidy_mod.factor)
  tidy_mod.factor[201:202, 1] <- "0" #add 2 rows for month 0
  tidy_mod.factor[201:202, c(2,4)] <- 0 #add coef.month and coef.treat
  tidy_mod.factor[201:202, 5] <- c(0, 1) #add treat=0 and treat=1
  tidy_mod.factor[, 1] <- c(seq(-49,-4,1),seq(3,56,1),seq(-49,-4, 1),seq(3,56, 1),-3,-3) #change month numbers
  tidy_mod.factor$group[101:200] <- 1 #change group to treat=1
  tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat[102:202] <- tidy_mod.factor$coef.month[102:202]
  tidy_mod.factor$coef.month[102:202] <- tidy_mod.factor$coef.month[1:101]
  tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                    tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
  tidy_mod.factor$diff <- NA
  tidy_mod.factor$diff[1:101] <- tidy_mod.factor$calorie[102:202] - tidy_mod.factor$calorie[1:101]
  tidy_mod.factor$p.diff <- NA
  tidy_mod.factor$p.diff[1:101] <- tidy_mod.factor$p[102:202]
  tidy_mod.factor$meal <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
  tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]
}

ggplot(data=tidy_mod.factor_all%>%filter(meal==2|meal==3|meal==5),aes(x=month,y=diff,color=factor(meal))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-100, ymax=100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,25)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, main meals", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Meal time",values=c("hotpink","olivedrab3","#13B0E4"),
                     labels=c("Breakfast (04:00-10:59)","Lunch (11:00-13:59)","Dinner (17:00-20:59)")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/main-effect-mealtime-mainMeal.jpeg", dpi="retina")

ggplot(data=tidy_mod.factor_all%>%filter(meal==1|meal==4|meal==6),aes(x=month,y=diff,color=factor(meal))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-100, ymax=100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,25)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, other meals", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Meal time",values=c("purple","orange","grey"),
                     labels=c("Late night (00:00-03:59)","Afternoon (14:00-16:59)","Evening (21:00-23:59)")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/main-effect-mealtime-otherMeal.jpeg", dpi="retina")

### detect trend using diff-in-diff, order type ----
tmp1 <- tidy_mod.factor_all %>% group_by(meal) %>%
  filter(month>=-30&month<0) %>% dplyr::select(month, diff, meal) %>%
  mutate(month = -month) %>% arrange(meal,month) %>%
  mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_all %>% group_by(meal) %>%
  filter(month>=1&month<=30) %>% dplyr::select(month, diff, meal) %>%
  arrange(meal,month) %>%
  rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by=c("month","meal")) %>%
  group_by(meal,month) %>%
  arrange(meal,month) %>%
  mutate(mean = post_mean - pre_mean) %>% select(-diff)
rm(tmp1,tmp2)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(meal))) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-220,40),breaks=seq(-220,40,20)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by meal time", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Ordery type",values = c("hotpink","olivedrab3","#13B0E4","purple","orange","grey"),
                     labels=c("Late night (00:00-03:59)","Breakfast (04:00-10:59)","Lunch (11:00-13:59)",
                              "Afternoon (14:00-16:59)","Dinner (17:00-20:59)","Evening (21:00-23:59)")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/diff-in-diff-mealtime.jpeg", dpi="retina")




