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

### preparing data ----
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

### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = occasion%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor)
#write.csv(trend, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")

# clean data
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

# add year and month factor as covariate
summary(tidy_mod.factor$calorie) #[-81,75]
summary(tidy_mod.factor$diff) #[-62,22]
ggplot(data=tidy_mod.factor%>%filter(month<=-3|month>=3),aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) + #
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -275, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&(month<=-3|month>=3)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.05&(month<=-3|month>=3)),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -400, ymax = 100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-18,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,100),breaks=seq(-400,100,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/cal=treat+month_monthFE_restaurantFE-24mon.jpeg", dpi="retina")

# hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
for (s in c(0.001,0.01,0.05)) {
  for (i in 2:30) {
    if (linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]>=s) {
      print(paste0("month ",i+1," not significant at ", s))
    }
  }
  
}
sum(abs(tidy_mod.factor$diff[42:46]))

### cal=group*month(factor), month as factor, detect trend using diff-in-diff ----
trend <- NULL
tmp <- data.frame(3:30)
colnames(tmp)[1] <- "month"
occasion <- within(occasion, relative2.factor<-relevel(relative2.factor, ref="-3"))
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = occasion%>%filter((relative2<=-3|(relative2>=2&relative2<=55))), #&match_place=="ca"
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor)
#write.csv(trend, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")

# clean data
tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat <- 0
tidy_mod.factor$group <- 0
dim(tidy_mod.factor)
#tidy_mod.factor <- tidy_mod.factor[-c(115:129),]
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
#for pre-period, use only up to 6 months of data
tmp1 <- tidy_mod.factor[tidy_mod.factor$month>=-30&tidy_mod.factor$month<0&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp1 <- tmp1[order(tmp1$month, decreasing = TRUE), ]
tmp1$month <- -tmp1$month
tmp1$pre_mean <- sum(tmp1$diff[tmp1$month<=8])/6
tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=1&tidy_mod.factor$month<=30&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp2 <- tmp2[order(tmp2$month), ]
colnames(tmp2)[2] <- "post_mean"
tmp <- merge(tmp1, tmp2, by="month", all=TRUE)
tmp$mean <- tmp$post_mean - tmp$pre_mean
tmp$loc <- "all"
tmp$diff <- NULL
#hypothesis testing
tmp$p <- 0
for (i in 2:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
}
trend <- rbind(trend, tmp)
rm(tmp1,tmp2,tmp)
trend <- trend %>%
  arrange(month) %>%
  mutate(mean3 = zoo::rollmean(mean,k=3,fill=mean))

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, group=1, color="hotpink")) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color="hotpink", group=1)) +
  ggplot2::annotate(geom="label", x=6, y=-30, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-30),color="hotpink",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-70,30),breaks=seq(-70,30,10)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Others"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), legend.position = "none",
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/mean-diff-in-diff-by-location.jpeg", dpi="retina")

# collapse impact by 6-month chunks
trend$time <- ifelse(trend$month<=6,6,
                     ifelse(trend$month>6&trend$month<=12,12,
                     ifelse(trend$month>12&trend$month<=18,18,
                     ifelse(trend$month>18&trend$month<=24,24,30))))
trend.collapse <- aggregate(data=trend, mean~time+loc,sum)
trend.collapse$mean <- ifelse(trend.collapse$time==6, trend.collapse$mean/4, trend.collapse$mean/6)
ggplot(data=trend.collapse, aes(x=time, y=mean, group=loc, color=loc)) + 
  geom_line() + geom_point() +
  #geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,0),breaks=seq(-50,0,10)) +
  scale_x_continuous(breaks=seq(6,30,6)) +
  labs(title="Mean impact by 6-month periods, matching abs value", x="Month", y="Calories", 
       caption="Impact is mean of each 6-month period sum. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Others"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/mean-diff-6mon.jpeg", dpi="retina")

### by-location analysis, separate each ML location ----
trend <- NULL
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = occasion%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
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
tidy_mod.factor <- tidy_mod.factor[!is.na(tidy_mod.factor$diff), c(1,7:8)]
#for pre-period, use only up to 6 months of data
tmp1 <- tidy_mod.factor[tidy_mod.factor$month>=-30&tidy_mod.factor$month<0&!is.na(tidy_mod.factor$diff), 1:2]
tmp1 <- tmp1[order(tmp1$month, decreasing = TRUE), ]
tmp1$month <- -tmp1$month
tmp1$pre_mean <- sum(tmp1$diff[tmp1$month<=8])/6
tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=1&tidy_mod.factor$month<=30&!is.na(tidy_mod.factor$diff), 1:2]
tmp2 <- tmp2[order(tmp2$month), ]
colnames(tmp2)[2] <- "post_mean"
tmp <- merge(tmp1, tmp2, by="month", all=TRUE)
tmp$mean <- tmp$post_mean - tmp$pre_mean
tmp$loc <- "all"
#tmp$diff <- NULL
#hypothesis testing
tmp$p <- 0
for (i in 2:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
}
trend <- rbind(trend,tmp)

for (i in c("cen-cal","north-cal","s-valley","south-cal","west-cal","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = occasion%>%filter(((relative2<=-3&relative2>=-47)|(relative2>=2&relative2<=55))&match_place==i), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
  colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
  tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat <- 0
  tidy_mod.factor$group <- 0
  dim(tidy_mod.factor)
  tidy_mod.factor[197:198, 1] <- "0" #add 2 rows for month 0
  tidy_mod.factor[197:198, c(2,4)] <- 0 #add coef.month and coef.treat
  tidy_mod.factor[197:198, 5] <- c(0, 1) #add treat=0 and treat=1
  tidy_mod.factor[, 1] <- c(seq(-47,-4,1),seq(3,56,1),seq(-47,-4, 1),seq(3,56, 1),-3,-3) #change month numbers
  tidy_mod.factor$group[99:196] <- 1 #change group to treat=1
  tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
  tidy_mod.factor$coef.treat[100:198] <- tidy_mod.factor$coef.month[100:198]
  tidy_mod.factor$coef.month[100:198] <- tidy_mod.factor$coef.month[1:99]
  tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                    tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
  tidy_mod.factor$diff <- NA
  tidy_mod.factor$diff[1:99] <- tidy_mod.factor$calorie[100:198] - tidy_mod.factor$calorie[1:99]
  tidy_mod.factor$p.diff <- NA
  tidy_mod.factor$p.diff[1:99] <- tidy_mod.factor$p[100:198]
  tidy_mod.factor <- tidy_mod.factor[!is.na(tidy_mod.factor$diff), c(1,7:8)]
  tmp1 <- tidy_mod.factor[tidy_mod.factor$month>=-30&tidy_mod.factor$month<0&!is.na(tidy_mod.factor$diff), 1:2]
  tmp1 <- tmp1[order(tmp1$month, decreasing = TRUE), ]
  tmp1$month <- -tmp1$month
  tmp1$pre_mean <- sum(tmp1$diff[tmp1$month<=8])/6
  tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=1&tidy_mod.factor$month<=30&!is.na(tidy_mod.factor$diff), 1:2]
  tmp2 <- tmp2[order(tmp2$month), ]
  colnames(tmp2)[2] <- "post_mean"
  tmp <- merge(tmp1, tmp2, by="month", all=TRUE)
  tmp$mean <- tmp$post_mean - tmp$pre_mean
  tmp$loc <- i
  #tmp$diff <- NULL
  tmp$p <- 0
  for (i in 2:29) {
    tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
  }
  trend <- rbind(trend,tmp)
}
rm(i,tmp,tmp1,tmp2)

summary(trend$diff) #[-102,39]
ggplot(data=trend,aes(x=month, y=diff,group=loc, color=loc)) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = 12, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = 14, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-120, ymax=70, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-75, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-120,70),breaks=seq(-120,70,10)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, by location, split CA", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \nKing county, Montgomery county and the hypothetical state of Jefferson do not have separate analyses for each having N=3.") + 
  scale_color_manual(name="Location",
                     labels=c("All (N=506)","Central California (N=84)","North California (N=68)",
                              "Silicon Valley (N=47)","South California (N=125","West California (N=85)",
                              "MA (N=25)", "OR (N=46)", "Suffolk county, NY (N=16)"),
                     values=c("hotpink","olivedrab3","red","orange","grey","purple","#13B0E4","black","blue")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) # +theme_bw()
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/cal=treat+month_monthFE_restaurantFE-bylocation-splitCA.jpeg", dpi="retina")

#diff-in-diff figure
ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, group=loc, color=loc)) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=loc, group=loc)) +
  ggplot2::annotate(geom="label", x=6, y=-75, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-75),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-125,75),breaks=seq(-125,75,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by location", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location",
                     labels=c("All (N=506)","Central California (N=84)","North California (N=68)",
                              "Silicon Valley (N=47)","South California (N=125","West California (N=85)",
                              "MA (N=25)", "OR (N=46)", "Suffolk county, NY (N=16)"),
                     values=c("hotpink","olivedrab3","red","orange","grey","purple","#13B0E4","black","blue")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/mean-diff-in-diff-by-location-splitCA.jpeg", dpi="retina")

# number of restaurants open for each month post labeling
time <- data.frame(c(1:30),c(rep(0,90))) %>% setNames(c("month","num"))
for (i in c(0:29)) {
  time[i+1,2] <- length(unique(paste0(occasion$id[occasion$relative2==i],occasion$match_place[occasion$relative2==i])))
  time[i+31,2] <- length(unique(occasion$id[occasion$relative2==i&occasion$treat==1]))
  time[i+61,2] <- length(unique(paste0(occasion$id[occasion$relative2==i&occasion$treat==0],occasion$match_place[occasion$relative2==i&occasion$treat==0])))
}
time$group <- "all"
time$group[31:60] <- "treat"
time$group[61:90] <- "comp"
time$pct <- 0
time$pct[1:30] <- time$num[1:30]/time$num[1]
time$pct[31:60] <- time$num[31:60]/time$num[31]
time$pct[61:90] <- time$num[61:90]/time$num[61]

ggplot(data=time, aes(x=month, y=pct, group=group,color=group)) + 
  geom_line() + geom_point() +
  geom_vline(xintercept = 12, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = 13, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=13, y=1.07, label="N comparison \nrestaurants decreased \nby 57, 50 of which \nare occasion to \n CA restaurants.", size=3) + 
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(0.5,1.2),breaks=seq(0.5,1.2,0.1),labels = scales::percent) +
  scale_x_continuous(breaks=seq(1,30,1)) +
  labs(title="Number of restaurants post labeling", x="Month", y="", 
       caption="These comp restaurants have a median weight of 0.1, much higher than the median of 0.04 with the full sample. \n38 of those unique restaurants are located in TX.") + 
  scale_color_discrete(name="Group",labels=c("All","Comparison","Treatment")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reoccasion/num-restaurants-over-time.jpeg", dpi="retina")

