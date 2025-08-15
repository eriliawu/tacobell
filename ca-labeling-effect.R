### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell/data")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)

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
library(zoo)

### matched data, preparing data ----
# read in transaction data, by restid-month-occasion
sales <- NULL
for (i in 2007:2013) {
  for (j in 1:4) {
    tryCatch(
      if(i==2013 & j==4) {stop("file doesn't exist")} else
      {
        if (i==2007&j==1) {
          tmp <- read.csv(paste0("from-bigpurple/mean-calorie-w-mod/exclude-beverages/by-restaurant-occasion/mean-calorie-excl-bev_restid_",
                                 i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                          colClasses = c(rep(NA,4),rep("NULL",7),NA,"NULL"))
          tmp[,4] <- tmp[,4]/2
          sales <- rbind(tmp,sales)
          print(paste0(i,"Q",j," is done"))
        } else {
          tmp <- read.csv(paste0("from-bigpurple/mean-calorie-w-mod/exclude-beverages/by-restaurant-occasion/mean-calorie-excl-bev_restid_",
                                 i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                          colClasses = c(rep(NA,4),rep("NULL",7),NA,"NULL"))
          sales <- rbind(tmp,sales)
          print(paste0(i,"Q",j," is done"))
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(tmp,i,j)
colnames(sales)[1:3] <- c("restid","monthno","occasion")
sales <- sales %>% mutate(treat=ifelse(occasion==2,0,1)) %>% 
  group_by(restid,monthno,treat) %>% 
  summarise(cal=sum(cal),count=sum(count))

restaurant <- read.csv("restaurants/analytic_restaurants.csv") %>% filter(state=="CA") %>% 
  dplyr::select(restid,address,tract_num,concept,ownership)
sales <- merge(sales,restaurant,by="restid")
rm(restaurant)

matched <- sales %>% dplyr::select(-restid) %>% 
  mutate(concept=ifelse(concept=="TBC",1,0)) %>% 
  mutate(ownership=ifelse(ownership=="COMPANY",1,0)) %>% 
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>% 
  dplyr::select(monthno,treat,cal,count,id) %>% 
  group_by(monthno,treat,id) %>% summarise(cal=sum(cal),count=sum(count)) %>% 
  mutate(mean_cal = cal/count) %>% 
  mutate(relative2=monthno-253) %>% 
  mutate(post=ifelse(relative2<0,0,1))
  
matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))

time <- read.csv("from-bigpurple/time_day_dim.csv") %>% 
  mutate(year=as.integer(substr(YEARNO,2,5))) %>% 
  mutate(month=as.integer(substr(MONTHBGNDT,6,7))) %>% 
  dplyr::select(DW_MONTH,year,month) %>% rename(monthno=DW_MONTH) %>% 
  filter(monthno>=204) %>% distinct()
matched <- merge(matched,time,by="monthno")
rm(time)

### overall diff in diff, and visualize ----
mod.factor <- plm(formula = mean_cal~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id", model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)&month!="treat") %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month+mod.factor$coefficients[1],NA)) %>%
  mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]+mod.factor$coefficients[1])) 
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
summary(trend$calorie) #[-51,128]
summary(trend$mean) #[-26,15]
ggplot(data=trend,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_point() + geom_line() +
  geom_line(data=trend%>%filter(!is.na(mean)),aes(x=month, y=mean*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=trend%>%filter(!is.na(mean)&p<0.05),aes(x=month, y=mean*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -400, ymax = 200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-230, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=16.5, y=-250, label="   P<0.05", size=3) + 
  geom_point(aes(x=14.5,y=-250),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,200),breaks=seq(-400,200,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference-in-different estimate")) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling in CA", x="Month", y="Calorie difference estimate", caption="") + 
  scale_color_manual(name="Menu labeling",labels=c("No","Yes","Difference-in-difference"),values=c("#F8766D","#00BFC4","orange")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
ggsave("../tables/overall-ca-keep-treat.jpeg", dpi="retina")

### clean data by food category ----
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        if (i==2007&j==1) {
          tmp <- read.csv(paste0("from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-category-occasion_",
                                 i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                          colClasses = c(rep(NA,5),rep("NULL",7),NA,"NULL"))
          tmp[,5] <- tmp[,5]/2
          sales <- rbind(tmp,sales)
          print(paste0(i,"Q",j," is done"))
        } else {
          tmp <- read.csv(paste0("from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-category-occasion_",
                                 i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                          colClasses = c(rep(NA,5),rep("NULL",7),NA,"NULL"))
          sales <- rbind(tmp,sales)
          print(paste0(i,"Q",j," is done"))
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(tmp,i,j)
colnames(sales)[1:4] <- c("monthno","restid","category","occasion")
sales <- sales %>% filter(occasion %in% c(1:3)) %>% 
  filter(!is.na(category)) %>% 
  mutate(treat=ifelse(occasion==2,0,1)) %>% 
  group_by(restid,monthno,treat,category) %>% 
  summarise(cal=sum(cal),count=sum(count))

restaurant <- read.csv("restaurants/analytic_restaurants.csv") %>% filter(state=="CA") %>% 
  dplyr::select(restid,address,tract_num,concept,ownership)
sales <- merge(sales,restaurant,by="restid")
rm(restaurant)

matched <- sales %>% dplyr::select(-restid) %>% 
  mutate(concept=ifelse(concept=="TBC",1,0)) %>% 
  mutate(ownership=ifelse(ownership=="COMPANY",1,0)) %>% 
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>% 
  dplyr::select(monthno,treat,cal,count,id,category) %>% 
  group_by(monthno,treat,id,category) %>% summarise(cal=sum(cal),count=sum(count)) %>% 
  mutate(mean_cal = cal/count) %>% 
  mutate(relative2=monthno-253) %>% 
  mutate(post=ifelse(relative2<0,0,1))

matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))

time <- read.csv("from-bigpurple/time_day_dim.csv") %>% 
  mutate(year=as.integer(substr(YEARNO,2,5))) %>% 
  mutate(month=as.integer(substr(MONTHBGNDT,6,7))) %>% 
  dplyr::select(DW_MONTH,year,month) %>% rename(monthno=DW_MONTH) %>% 
  filter(monthno>=204) %>% distinct()
matched <- merge(matched,time,by="monthno").

rm(time)

### diff in diff by category, and visualize ----
trend <- NULL
for (i in 1:9) {
  mod.factor <- plm(formula = mean_cal~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
                    index = "id", model = "within")
  tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE) %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|calorie", month)&month!="treat") %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month+mod.factor$coefficients[1],NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]+mod.factor$coefficients[1]))
  #diff in diff
  tmp1 <- tidy_mod.factor %>% 
    filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month,diff,low,high) %>%
    mutate(month = -month) %>% arrange(month) %>%
    mutate(pre_mean = sum(diff[1:6])/6, pre_low=sum(low[1:6])/6, pre_high = sum(high[1:6])/6)
  tmp2 <- tidy_mod.factor %>% 
    filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month,diff,low,high) %>%
    arrange(month) %>% rename(post_mean = diff, post_low=low, post_high=high)
  trend_tmp <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
    mutate(mean = post_mean - pre_mean, low=post_low-pre_low, high=post_high-pre_high) %>%
    dplyr::select(month,mean,low,high)
  rm(tmp1,tmp2)
  tidy_mod.factor <- tidy_mod.factor[,c(1,6,8)]
  
  ## hypothesis testing
  presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  trend_tmp <- cbind(trend_tmp, tmp) %>% dplyr::select(month,mean,p) %>% mutate(group=1)
  trend_tmp <- merge(trend_tmp,tidy_mod.factor,by=c("group","month"), all=TRUE)
  trend_tmp$category <- i
  trend <- rbind(trend,trend_tmp)
}
rm(i,j,mod.factor,tidy_mod.factor,tmp,trend_tmp,presum)

summary(trend$mean[trend$category!=1]) #[-26,15]
ggplot(data=trend %>% filter(!is.na(mean)&category%in%c(2:4,6,8)),aes(x=month, y=mean,color=as.character(category))) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -200, color="grey", linetype="dashed", size=0.5) +
  geom_point() + geom_line() +
  ggplot2::annotate(geom="label", x=16.5, y=-250, label="   P<0.05", size=3) + 
  geom_point(aes(x=15,y=-250),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,50),breaks=seq(-50,50,10)) +
  scale_x_continuous(breaks=seq(3,30,1)) + #select which months to display
  labs(title="Diff in diff, by food category", x="Month", y="Calorie difference estimate", caption="") + 
  scale_color_manual(name="Category",
                     labels=c("Burrito","Dessert","Other entree","Side","Taco"),
                     values=c("#F8766D","grey","orange","olivedrab","#00BFC4")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("../tables/food-category-ca.jpeg", dpi="retina")


