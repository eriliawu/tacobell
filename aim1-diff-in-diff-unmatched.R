### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects
### unmatched data

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
#install.packages(c("lme4", "lmerTest")) #random effects and tesing significance
#install.packages("plm")
library(lme4)
library(plm)
library(lmerTest)
#install.packages("stargazer")
library(stargazer)
#install.packages("table1")
library(table1)
library(tableone)
library(broom)
#install.packages("car")
library(car)
### import data ----
unmatched <- read.csv("data/calorie-aims/unmatched-restaurants.csv", stringsAsFactors = FALSE,
                      colClasses = c(rep(NA,7),rep("NULL",2),NA,rep("NULL",9),rep(NA,3),rep("NULL",13),rep(NA,2),rep("NULL",21),NA))
unmatched$tract_num <- substr(unmatched$tract_num, 2, 12)

### unmatched data, expand comparison units, to match each treated unit ----
#omit treated restaurants without calorie data at the time of labeling
table(unmatched$policy[(unmatched$treat==1&unmatched$monthno==unmatched$entry)])
unmatched <- unmatched %>%
  filter((policy!="nyc"&policy!="ulster"&policy!="westchester")&!is.na(calorie))

time <- unmatched %>% select(12,14) %>% filter(!duplicated(policy)) %>% filter(!is.na(entry))
comp <- unmatched[unmatched$treat==0, ]
master <- NULL
for (i in 1:10) {
  treat <- unmatched[unmatched$policy==time[i,2], ]
  treat <- rbind(treat,comp)
  treat$match_to <- time[i,2]
  treat$entry <- time[i,1]
  master <- rbind(master, treat)
}
length(unique(unmatched$id[unmatched$treat==0])) #5644
unmatched <- master
rm(master,treat,comp,i,time)
table(unmatched$entry)
table(unmatched$match_to)
### preapre data ----
unmatched <- unmatched %>%
  filter(!is.na(calorie)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept, match_to)) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(unmatched$rank) #sanity check, the max should be 106

# set up post ML indicator
unmatched$relative <- unmatched$monthno - unmatched$entry +1
unmatched$relative2 <- unmatched$monthno - unmatched$entry #month 0 is first month of ML
unmatched$post <- ifelse(unmatched$relative2<0,0,1)

# month as relative and factor
# set month 1 as ref group
unmatched$relative.factor <- factor(unmatched$relative)
unmatched <- within(unmatched, relative.factor<-relevel(relative.factor, ref="1"))

unmatched$relative2.factor <- factor(unmatched$relative2)
unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="3"))

# calculate open_month both before and after ML
unmatched <- unmatched %>%
  group_by(id, treat, match_to, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
unmatched$open_before <- ifelse(unmatched$post==0, unmatched$open_before, unmatched$open_month-unmatched$open_before)
unmatched$open_after <- ifelse(unmatched$post==1, unmatched$open_after, unmatched$open_month-unmatched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, after6,after12,after18,after24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, before6,before12,before18,before24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
unmatched$open6 <- ifelse(unmatched$before6==1&unmatched$after6==1,1,0)
unmatched$open12 <- ifelse(unmatched$before12==1&unmatched$after12==1,1,0)
unmatched$open18 <- ifelse(unmatched$before18==1&unmatched$after18==1,1,0)
unmatched$open24 <- ifelse(unmatched$before24==1&unmatched$after24==1,1,0)
rm(tmp)

unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="-3"))
unmatched$id_match <- paste0(unmatched$id, unmatched$match_place)

### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = unmatched%>%filter(((relative2<=-3&relative2>=-49)|(relative2>=2&relative2<=55))), 
                  index = c("id"), model = "within")
tidy_mod.factor <- tidy(mod.factor)

# clean data
tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat <- 0
tidy_mod.factor$group <- 0
dim(tidy_mod.factor)
tidy_mod.factor <- tidy_mod.factor[-c(1:17,118:134), ]
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
  geom_hline(yintercept = -200, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -250, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&(month<=-3|month>=3)),aes(x=month, y=diff*1-200), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.05&(month<=-3|month>=3)),aes(x=month, y=diff*1-200), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin =-3, xmax=3, ymi =-400, ymax=100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-130, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-18, y=-200, label="   P<0.05", size=3) + 
  geom_point(aes(x=-21,y=-200),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,100),breaks=seq(-400,100,50),
                     sec.axis = sec_axis(~(.+200)/1, name="Difference")) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, unmatched data", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff-unmatched/cal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

# hypothesis testing
presum <- "treat:relative2.factor-4+treat:relative2.factor-5+treat:relative2.factor-6+treat:relative2.factor-7+treat:relative2.factor-8+treat:relative2.factor-9+treat:relative2.factor-10+treat:relative2.factor-11+treat:relative2.factor-12"
for (s in c(0.001,0.01,0.05)) {
  for (i in 2:30) {
    if (linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]>=s) {
      print(paste0("month ",i+1," not significant at ", s))
    }
  }
}

### cal=group*month(factor), month as factor, detect trend using diff-in-diff ----
trend <- NULL
tmp <- data.frame(3:30)
colnames(tmp)[1] <- "month"
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = unmatched%>%filter(((relative2<=-3&relative2>=-49)|(relative2>=2&relative2<=55))), 
                  index = c("id"), model = "within")
tidy_mod.factor <- tidy(mod.factor)

# clean data
tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat <- 0
tidy_mod.factor$group <- 0
dim(tidy_mod.factor)
tidy_mod.factor <- tidy_mod.factor[-c(1:17,118:134), ]
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
tmp1$pre_mean <- sum(tmp1$diff[tmp1$month<=12])/10
tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=0&tidy_mod.factor$month<=30&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp2 <- tmp2[order(tmp2$month), ]
colnames(tmp2)[2] <- "post_mean"
tmp <- merge(tmp1, tmp2, by="month", all=TRUE)
tmp$mean <- tmp$post_mean - tmp$pre_mean
tmp$diff <- NULL
rm(tmp1,tmp2,tmp)
trend <- trend %>%
  arrange(month) %>%
  mutate(mean3 = zoo::rollmean(mean,k=3,fill=NA)) %>%
  dplyr::select(month,mean,mean3)
mean <- trend[,1:2]
mean$group <- 1
mean3 <- trend[,c(1,3)]
colnames(mean3)[2] <- "mean"
mean3$group <- 3
trend <- rbind(mean,mean3)

ggplot(data=trend, aes(x=month, y=mean, group=as.character(group),color=as.character(group))) + 
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,25),breaks=seq(-100,25,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, 3-month rolling average", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -12. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Mean", labels=c("No transformation","3-month rolling average"),
                     values = c("hotpink","olivedrab3")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff-unmatched/mean-diff-in-diff-by-rolling-avg.jpeg", dpi="retina")

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
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-reunununmatched/mean-diff-6mon.jpeg", dpi="retina")


### manuscript, appendix figure 2, unmatched data, use drive-thru only----
unmatched <- read.csv("data/calorie-aims/unmatched-restaurants-drive-thru.csv", stringsAsFactors = FALSE,
                      colClasses = c(rep(NA,7),rep("NULL",4),rep(NA,8),rep("NULL",13),rep(NA,2),rep("NULL",12),NA))
unmatched$tract_num <- substr(unmatched$tract_num, 2, 12)
table(unmatched$policy[(unmatched$treat==1&unmatched$monthno==unmatched$entry)])
unmatched <- unmatched %>%
  filter((policy!="nyc"&policy!="ulster"&policy!="westchester")&!is.na(calorie))

time <- unmatched %>% select(entry,policy) %>% filter(!duplicated(policy)) %>% filter(!is.na(entry))
comp <- unmatched[unmatched$treat==0, ]
master <- NULL
for (i in 1:10) {
  treat <- unmatched[unmatched$policy==time[i,2], ]
  treat <- rbind(treat,comp)
  treat$match_to <- time[i,2]
  treat$entry <- time[i,1]
  master <- rbind(master, treat)
}

unmatched <- master
rm(master,treat,comp,i,time)
table(unmatched$entry)
table(unmatched$match_to)
unmatched <- unmatched %>%
  filter(!is.na(calorie)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept, match_to)) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(unmatched$rank) #sanity check, the max should be 106

# set up post ML indicator
unmatched$relative <- unmatched$monthno - unmatched$entry +1
unmatched$relative2 <- unmatched$monthno - unmatched$entry #month 0 is first month of ML
unmatched$post <- ifelse(unmatched$relative2<0,0,1)

# month as relative and factor
# set month 1 as ref group
unmatched$relative.factor <- factor(unmatched$relative)
unmatched <- within(unmatched, relative.factor<-relevel(relative.factor, ref="1"))

unmatched$relative2.factor <- factor(unmatched$relative2)
unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="3"))

# calculate open_month both before and after ML
unmatched <- unmatched %>%
  group_by(id, treat, match_to, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
unmatched$open_before <- ifelse(unmatched$post==0, unmatched$open_before, unmatched$open_month-unmatched$open_before)
unmatched$open_after <- ifelse(unmatched$post==1, unmatched$open_after, unmatched$open_month-unmatched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, after6,after12,after18,after24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
tmp <- unmatched %>%
  group_by(id, treat, match_to) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_to, before6,before12,before18,before24) %>%
  distinct()
unmatched <- merge(unmatched, tmp, by=c("id", "treat", "match_to"), all = TRUE)
unmatched$open6 <- ifelse(unmatched$before6==1&unmatched$after6==1,1,0)
unmatched$open12 <- ifelse(unmatched$before12==1&unmatched$after12==1,1,0)
unmatched$open18 <- ifelse(unmatched$before18==1&unmatched$after18==1,1,0)
unmatched$open24 <- ifelse(unmatched$before24==1&unmatched$after24==1,1,0)
rm(tmp)

unmatched <- within(unmatched, relative2.factor<-relevel(relative2.factor, ref="-3"))
unmatched$id_match <- paste0(unmatched$id, unmatched$match_place)

### appendix figure 2, main effect and diff in diff, unmatched data ----
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = unmatched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56])) 
tmp1 <- tidy_mod.factor %>% 
  filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month,diff,low,high) %>%
  mutate(month = -month) %>% arrange(month) %>%
  mutate(pre_mean = sum(diff[1:6])/6, pre_low=sum(low[1:6])/6, pre_high = sum(high[1:6])/6)
tmp2 <- tidy_mod.factor %>% 
  filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month,diff,low,high) %>%
  arrange(month) %>% rename(post_mean = diff, post_low=low, post_high=high)
trend <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
  mutate(mean = post_mean - pre_mean, low=post_low-pre_low, high=post_high-pre_high) %>%
  dplyr::select(month,mean,low,high)
rm(tmp1,tmp2)
tidy_mod.factor <- tidy_mod.factor[,c(1,6,8)]

# hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
for (i in 4:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
}
trend <- cbind(trend, tmp) %>% dplyr::select(month,mean,p) %>% mutate(group=1)
trend <- merge(trend,tidy_mod.factor,by=c("group","month"), all=TRUE)
trend <- trend %>% add_row(group=2) #manually add group=2 as the diff in diff line for the orange color

# add year and month factor as covariate
summary(trend$calorie) #[-100,38]
summary(trend$mean) #[-73,30]
ggplot(data=trend,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -200, color="grey", linetype="dashed", size=0.5) +
  geom_point() + geom_line() +
  geom_line(data=trend%>%filter(!is.na(mean)),aes(x=month, y=mean*1-200), color="orange") + #add diff between 2 groups
  geom_point(data=trend%>%filter(!is.na(mean)&p<0.05),aes(x=month, y=mean*1-200), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -400, ymax = 200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-100, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=16.5, y=-200, label="   P<0.05", size=3) + 
  geom_point(aes(x=15,y=-200),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,200),breaks=seq(-400,200,50),
                     sec.axis = sec_axis(~(.+200)/1, name="Difference-in-different estimate")) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="", x="Month", y="Calorie difference estimate", caption="") + 
  scale_color_manual(name="Menu labeling",labels=c("No","Yes","Difference"),values=c("#F8766D","#00BFC4","orange")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig2-diff-in-diff-unmatched.jpeg", dpi="retina")


