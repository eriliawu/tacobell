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
#install.packages("usmap") #show state abbr in ggplot map
library(usmap)
library(maps)
library(car) #testing joint significance
#install.packages("zoo") #fill in NAs with the first non-NA value in a column
library(zoo)
 

### matched data, preparing data ----
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru.csv", stringsAsFactors = FALSE)
matched$tract_num <- substr(matched$tract_num, 2, 12)
matched$holiday <- ifelse(matched$month==12, 1, 0)
matched <- matched %>%
  filter(complete.cases(matched)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat, match_place) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(matched$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
matched$relative <- matched$monthno - matched$entry +1
matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
matched$post <- ifelse(matched$relative2<0, 0, 1)

# summary stats for restarants continuously open for 106 months
length(unique(matched$id[matched$open_month==106&matched$treat==1])) #67
length(unique(matched$id[matched$open_month==106&matched$treat==0])) #399

# month as relative and factor
# set month 1 as ref group
matched$relative.factor <- factor(matched$relative)
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="1"))
summary(matched$relative)

matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
matched <- matched %>%
  group_by(id, treat, match_place, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
matched$open_before <- ifelse(matched$post==0, matched$open_before, matched$open_month-matched$open_before)
matched$open_after <- ifelse(matched$post==1, matched$open_after, matched$open_month-matched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
matched$open6 <- ifelse(matched$before6==1&matched$after6==1,1,0)
matched$open12 <- ifelse(matched$before12==1&matched$after12==1,1,0)
matched$open18 <- ifelse(matched$before18==1&matched$after18==1,1,0)
matched$open24 <- ifelse(matched$before24==1&matched$after24==1,1,0)
rm(tmp)

matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))
matched$id_match <- paste0(matched$id, matched$match_place)

### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE)
# clean data
tidy_mod.factor <- tidy_mod.factor %>%
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

# add year and month factor as covariate
summary(tidy_mod.factor$calorie) #[-95,133]
summary(tidy_mod.factor$diff) #[-95,14]
ggplot(data=tidy_mod.factor,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)&p<0.05),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -400, ymax = 200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-16.5,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,200),breaks=seq(-400,200,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased at drive-thru", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/cal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

# hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
for (s in c(0.001,0.01,0.05)) {
  for (i in 2:30) {
    if (linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]>=s) {
      print(paste0("month ",i+1," not significant at ", s))
    }
  }
  
}

### cal=group*month(factor), month as factor, detect trend using diff-in-diff ----
tidy_mod.factor <- tidy_mod.factor %>%
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

#hypothesis testing
tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
for (i in 4:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
}
trend <- cbind(trend, tmp)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, group=1, color="hotpink")) + 
  geom_line(data=trend, aes(x=month, y=low, group=1, color="hotpink",linetype="dashed")) + 
  geom_line(data=trend, aes(x=month, y=high, group=1, color="hotpink",linetype="dashed")) + 
  #geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color="hotpink", group=1)) +
  ggplot2::annotate(geom="label", x=6, y=-30, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-30),color="hotpink",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,30),breaks=seq(-100,30,10)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Others"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/mean-diff-in-diff-by-location.jpeg", dpi="retina")

### analysis by restaurant open time after labeling ----
#restaurants open for at least 12, 15, 18, 21, 24 months after labeling
tidy_mod.factor_all <- NULL
for (i in c(12,15,18,21,24,27,30)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&open_after>=i), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor %>%
    dplyr::select(term,estimate,p.value) %>%
    rename(month=term,coef.month=estimate,p=p.value) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0) %>%
    add_row(month="-3",coef.month=0,group=1) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))
  tidy_mod.factor$time <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
}
tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]

summary(tidy_mod.factor_all$diff) #[-100,16]
ggplot(data=tidy_mod.factor_all,aes(x=month, y=diff,color=as.character(time))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-100, ymax=25, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,25),breaks=seq(-100,25,25)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + 
  labs(title="Effect of menu labeling on calories purchased, by open time after ML", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Open after labeling", labels=c("12 months","15 months","18 months","21 months","24 months","27 months","30 months"),
                       values=c("hotpink","olivedrab3","red","orange","grey","purple","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/cal=treat+month_monthFE_restaurantFE-by-open-time.jpeg", dpi="retina")

### by-location analysis, separate each ML location ----
tidy_mod.factor_all <- NULL
trend <- NULL
for (p in c("ca","ma","mont","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor %>%
    dplyr::select(term,estimate,p.value) %>%
    rename(month=term,coef.month=estimate,p=p.value) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0) %>%
    add_row(month="-3",coef.month=0,group=1) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))
  tidy_mod.factor$loc <- p
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
  tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]
  tmp1 <- tidy_mod.factor %>% 
    filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month, diff) %>%
    mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
  tmp2 <- tidy_mod.factor %>% 
    filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month, diff) %>%
    arrange(month) %>% rename(post_mean = diff)
  tmp1 <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
    mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (i in 4:29) {
    tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
  }
  tmp1 <- cbind(tmp1, tmp)
  tmp1$loc <- p
  trend <- rbind(trend,tmp1)
}
rm(tidy_mod.factor,tidy_mod.factor_all,trend,mod.factor)

#main effect by location
summary(tidy_mod.factor_all$diff) #[-112,45]
ggplot(data=tidy_mod.factor_all,aes(x=month, y=diff,color=loc)) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-125, ymax=50, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-80, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + 
  labs(title="Effect of menu labeling on calories purchased, by location", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("CA","MA","Montgomery county, MD","OR","Suffolk county, NY"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","grey")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/cal=treat+month_monthFE_restaurantFE-by-loc.jpeg", dpi="retina")

#diff in diff
summary(trend$mean) #[-105,33]
ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=loc)) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=loc)) +
  ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=13),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by location", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("CA","MA","Montgomery county, MD","OR","Suffolk county, NY"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","grey")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/mean-diff-in-diff-by-location.jpeg", dpi="retina")



### king county, beverage calorie, before and after ML ----
king <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015&j==4) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_",i,"_Q",j,".csv"),
                           col.names = c("restid","month","occasion","cal","total_fat","carb",
                                         "protein","sat_fat","sugar","fiber","sodium","count"))
        sample$year <- i
        sample$quarter <- j
        king <- rbind(sample,king)
      }, error=function(e){cat(paste0("ERROR : ",i,"Q",j),conditionMessage(e), "\n")}
    )
  }
}
king <- king %>% mutate(cal = ifelse(year==2007&quarter==1,cal/2,cal)) %>% 
  dplyr::select(-c(year,quarter))
rm(sample,i,j)

#merge with restaurant data, identify king county restaurants
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE,
                       colClasses = c(rep(NA,3),rep("NULL",6),NA,rep("NULL",3),NA,rep("NULL",8),rep(NA,2),rep("NULL",5),NA))
king <-merge(restaurant,king,by="restid")
king <- king %>% filter(state=="WA"&(county=="King"|city=="Seattle")) %>%
  dplyr::select(-restid) %>%
  group_by(address, tract_num, ownership, concept, occasion, month) %>%
  summarise_at(vars(cal:count),sum) %>% #aggregate at restaurant address level
  mutate_at(vars(cal:sodium), ~./count) %>% #calculate mean per order
  filter(month>=224 & month<=247) %>% #trim month to a year before and after ML in king
  mutate(monthno = ifelse(month<=235,month-236,month-235))

# visualize, calorie,carb,sugar,protein,total_fat
cal <- ggplot(data=king %>% filter(address=="320 Rainier Avenue South, Renton, WA 98055"),aes(x=monthno, y=cal,color=as.factor(occasion))) + 
  geom_point(size=1) + geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_vline(xintercept = 1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=1600, label="Menu labeling \n implementation", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(800,1800),breaks=seq(800,1800,200)) +
  scale_x_continuous(breaks=c(seq(-12,-1,1),seq(1,12,1))) + #select which months to display
  labs(title="320 Rainier Avenue South, Renton, WA 98055", x="Month", y="") + 
  scale_color_discrete(name="Order type", labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        legend.position="top")
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/cal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

#carbs
carb <- ggplot(data=king %>% filter(address=="320 Rainier Avenue South, Renton, WA 98055"),aes(x=monthno, y=carb,color=as.factor(occasion))) + 
  geom_point(size=1) + geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_vline(xintercept = 1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=198, label="Menu labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(100,220),breaks=seq(100,220,30)) +
  scale_x_continuous(breaks=c(seq(-12,-1,1),seq(1,12,1))) + #select which months to display
  labs(title="", x="", y="") + 
  scale_color_discrete(name="Order type", labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        legend.position='none')

sugar <- ggplot(data=king %>% filter(address=="320 Rainier Avenue South, Renton, WA 98055"),aes(x=monthno, y=sugar,color=as.factor(occasion))) + 
  geom_point(size=1) + geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_vline(xintercept = 1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=88, label="Menu labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(20,100),breaks=seq(20,100,10)) +
  scale_x_continuous(breaks=c(seq(-12,-1,1),seq(1,12,1))) + #select which months to display
  labs(title="", x="", y="") + 
  scale_color_discrete(name="Order type", labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        legend.position='none')

total_fat <- ggplot(data=king %>% filter(address=="320 Rainier Avenue South, Renton, WA 98055"),aes(x=monthno, y=total_fat,color=as.factor(occasion))) + 
  geom_point(size=1) + geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_vline(xintercept = 1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=70, label="Menu labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(30,80),breaks=seq(30,80,10)) +
  scale_x_continuous(breaks=c(seq(-12,-1,1),seq(1,12,1))) + #select which months to display
  labs(title="", x="", y="") + 
  scale_color_discrete(name="Order type", labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        legend.position='none')

protein <- ggplot(data=king %>% filter(address=="320 Rainier Avenue South, Renton, WA 98055"),aes(x=monthno, y=protein,color=as.factor(occasion))) + 
  geom_point(size=1) + geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_vline(xintercept = 1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=60, label="Menu labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(30,70),breaks=seq(30,70,10)) +
  scale_x_continuous(breaks=c(seq(-12,-1,1),seq(1,12,1))) + #select which months to display
  labs(title="", x="", y="") + 
  scale_color_discrete(name="Order type", labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        legend.position='none')

ggarrange(cal,
          ggarrange(carb,sugar,total_fat,protein,ncol = 2,nrow = 2,labels=c("Carbohydrates","Sugar","Total fat","Protein")),
          nrow = 2,
          labels = "Calories") %>%
  ggexport(filename = "tables/drive-thru-in-store-comparison/compare-king-county-restaurants/320-Rainier-Avenue-South.jpeg",
           width = 1440,height = 960,res=150)