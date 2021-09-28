### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

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
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
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

### fig 1, diff in diff ----
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
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
summary(trend$calorie) #[-95,132]
summary(trend$mean) #[-89,10]
ggplot(data=tmp,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -250, color="grey", linetype="dashed", size=0.5) +
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
#ggsave("manuscript/figures/fig1-diff-in-diff.jpeg", dpi="retina")
rm(mod.factor,tidy_mod.factor,trend,i,presum)
### fig 2, diff in diff, by location ----
trend <- NULL
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
for (p in c("ca","ma","or","suffolk")) {
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

#diff in diff, fig 2
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
  labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
  scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon","Suffolk County, New York"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("manuscript/figures/fig2-diff-in-diff-by-loc.jpeg", dpi="retina")

#remove suffolk county
ggplot() + 
  geom_line(data=trend%>%filter(loc!="suffolk"), aes(x=month, y=mean, color=loc)) + 
  geom_point(data=trend%>%filter(p<0.05&loc!="suffolk"), aes(x=month, y=mean, color=loc)) +
  ggplot2::annotate(geom="label", x=6, y=13, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=13),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-125,50),breaks=seq(-125,50,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
  scale_color_manual(name="Location", labels=c("California","Massachusetts","Oregon"),
                     values=c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("manuscript/figures/fig2-diff-in-diff-by-loc-wo-suffolk.jpeg", dpi="retina")
rm(tmp1,tmp2,tmp,mod.factor,i,p,tidy_mod.factor,trend,presum)

### table 3, diff in diff, overall and by location ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=5,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("total","ca","ma","or","suffolk"))
rownames(table3) <- c("total","ca","ma","or","suffolk")
#overall
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
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
  mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  mutate(high = ifelse(group==0,high,high+high[1:56]))
# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table3[1,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
table3[1,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
#by location
for (p in c("ca","ma","or","suffolk")) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&match_place==p), 
                    index = "id_match", weights = weights, model = "within")
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
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,1] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,2] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,3] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,4] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  table3[p,5] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  table3[p,6] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
#fitting everything in the right format with blank rows and columns
table3 <- table3 %>%
  add_row(diff_treat_3_12="",.before=2) %>% add_row(diff_treat_3_12="",.before=2) %>%
  add_column(col="",.before=4) %>%
  mutate_all(~replace(., is.na(.), ""))
#write.csv(table3, "manuscript/tables/table3.csv")
rm(treat,comp,mod.factor,table3,p,presum,tidy_mod.factor)

### table 2, calories and nutrients in follow-up periodm treat=1 only ----
#read in by occasion data, for panels order type and geography
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","occasion","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(4:11)] <- sample07q1[, c(4:11)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","occasion","calorie","fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
#merge with restaurants and aggregate by restaurant-month
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,occasion) %>% summarise_all(sum)
matched_use <- matched %>% filter(treat==1&relative2>=2&relative2<=11) %>% dplyr::select(address,tract_num,concept,ownership,monthno,match_place)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(calorie:sodium, ~ ./count))

table2 <- data.frame(matrix(data=NA,nrow = 21,ncol = 9)) %>%
  setNames(c("cal","fat","carb","protein","sat_fat","sugar","fiber","sodium","pct_sales"))
rownames(table2) <- c("drive-thru","eat-in","takeout",
                      "ca","ma","or","mont","suffolk","king",
                      "taco","burrito","salad","other","dessert","bev",
                      "latenight","breakfast","lunch","afternoon","dinner","evening")

#aggregate panel 1, by order type
tmp <- calorie %>% dplyr::select(occasion:sodium) %>% group_by(occasion) %>% summarise_all(list(mean=mean,sd=sd)) %>%
  mutate(cal=paste0(sprintf('%.0f',calorie_mean)," (",sprintf('%.0f',calorie_sd),")")) %>%
  mutate(fat=paste0(sprintf('%.2f',fat_mean)," (",sprintf('%.2f',fat_sd),")")) %>%
  mutate(carb=paste0(sprintf('%.0f',carb_mean)," (",sprintf('%.2f',carb_sd),")")) %>%
  mutate(protein=paste0(sprintf('%.2f',protein_mean)," (",sprintf('%.2f',protein_sd),")")) %>%
  mutate(sat_fat=paste0(sprintf('%.2f',sat_fat_mean)," (",sprintf('%.2f',sat_fat_sd),")")) %>%
  mutate(sugar=paste0(sprintf('%.2f',sugar_mean)," (",sprintf('%.2f',sugar_sd),")")) %>%
  mutate(fiber=paste0(sprintf('%.2f',fiber_mean)," (",sprintf('%.2f',fiber_sd),")")) %>%
  mutate(sodium=paste0(sprintf('%.0f',sodium_mean)," (",sprintf('%.0f',sodium_sd),")")) %>%
  dplyr::select(-c(2:17))
tmp <- tmp[c(2,1,3),]
table2[1:3,1:8] <- tmp[,2:9]
tmp <- calorie %>% group_by(occasion) %>% summarise(count=sum(count)) %>% ungroup() %>%
  mutate(pct=sprintf('%.2f',count/sum(count)*100))
tmp <- tmp[c(2,1,3),]
table2[1:3,9] <- tmp[,3]

#panel 2, by location
tmp <- calorie %>% filter(occasion==2) %>% dplyr::select(calorie:match_place) %>% group_by(match_place) %>%
  summarise_all(list(mean=mean,sd=sd)) %>%
  mutate(cal=paste0(sprintf('%.0f',calorie_mean)," (",sprintf('%.0f',calorie_sd),")")) %>%
  mutate(fat=paste0(sprintf('%.2f',fat_mean)," (",sprintf('%.2f',fat_sd),")")) %>%
  mutate(carb=paste0(sprintf('%.0f',carb_mean)," (",sprintf('%.2f',carb_sd),")")) %>%
  mutate(protein=paste0(sprintf('%.2f',protein_mean)," (",sprintf('%.2f',protein_sd),")")) %>%
  mutate(sat_fat=paste0(sprintf('%.2f',sat_fat_mean)," (",sprintf('%.2f',sat_fat_sd),")")) %>%
  mutate(sugar=paste0(sprintf('%.2f',sugar_mean)," (",sprintf('%.2f',sugar_sd),")")) %>%
  mutate(fiber=paste0(sprintf('%.2f',fiber_mean)," (",sprintf('%.2f',fiber_sd),")")) %>%
  mutate(sodium=paste0(sprintf('%.0f',sodium_mean)," (",sprintf('%.0f',sodium_sd),")")) %>%
  dplyr::select(-c(2:19))
tmp <- tmp[c(1,3,5,4,6,2),]
table2[4:9,1:8] <- tmp[,2:9]
tmp <- calorie %>% filter(occasion==2) %>% group_by(match_place) %>% 
  summarise(count=sum(count)) %>% ungroup() %>%
  mutate(pct=sprintf('%.2f',count/sum(count)*100))
tmp <- tmp[c(1,3,5,4,6,2),]
table2[4:9,9] <- tmp[,3]

#panel 3, read by-category data
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("monthno","restid","category","occasion","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(5:12)] <- sample07q1[, c(5:12)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("monthno","restid","category","occasion","calorie", "fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1) %>% filter(occasion==2&(category %in% c(8,2,5,4,3,1))) %>% dplyr::select(-occasion)
rm(sample, sample07q1, i, j)
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,category) %>% summarise_all(sum)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(calorie:sodium, ~ ./count)) 
tmp <- calorie %>% dplyr::select(category:sodium) %>% group_by(category) %>% summarise_all(list(mean=mean,sd=sd)) %>%
  mutate(cal=paste0(ifelse(calorie_mean>=100,sprintf('%.0f',calorie_mean),sprintf('%.2f',calorie_mean)),
                    " (",ifelse(calorie_sd>=100,sprintf('%.0f',calorie_sd),sprintf('%.2f',calorie_sd)),")")) %>%
  mutate(fat=paste0(ifelse(fat_mean>=100,sprintf('%.0f',fat_mean),sprintf('%.2f',fat_mean)),
                    " (",ifelse(fat_sd>=100,sprintf('%.0f',fat_sd),sprintf('%.2f',fat_sd)),")")) %>%
  mutate(carb=paste0(ifelse(carb_mean>=100,sprintf('%.0f',carb_mean),sprintf('%.2f',carb_mean)),
                    " (",ifelse(carb_sd>=100,sprintf('%.0f',carb_sd),sprintf('%.2f',carb_sd)),")")) %>%
  mutate(protein=paste0(ifelse(protein_mean>=100,sprintf('%.0f',protein_mean),sprintf('%.2f',protein_mean)),
                    " (",ifelse(protein_sd>=100,sprintf('%.0f',protein_sd),sprintf('%.2f',protein_sd)),")")) %>%
  mutate(sat_fat=paste0(ifelse(sat_fat_mean>=100,sprintf('%.0f',sat_fat_mean),sprintf('%.2f',sat_fat_mean)),
                    " (",ifelse(sat_fat_sd>=100,sprintf('%.0f',sat_fat_sd),sprintf('%.2f',sat_fat_sd)),")")) %>%
  mutate(sugar=paste0(ifelse(sugar_mean>=100,sprintf('%.0f',sugar_mean),sprintf('%.2f',sugar_mean)),
                    " (",ifelse(sugar_sd>=100,sprintf('%.0f',sugar_sd),sprintf('%.2f',sugar_sd)),")")) %>%
  mutate(fiber=paste0(ifelse(fiber_mean>=100,sprintf('%.0f',fiber_mean),sprintf('%.2f',fiber_mean)),
                    " (",ifelse(fiber_sd>=100,sprintf('%.0f',fiber_sd),sprintf('%.2f',fiber_sd)),")")) %>%
  mutate(sodium=paste0(ifelse(sodium_mean>=100,sprintf('%.0f',sodium_mean),sprintf('%.2f',sodium_mean)),
                    " (",ifelse(sodium_sd>=100,sprintf('%.0f',sodium_sd),sprintf('%.2f',sodium_sd)),")")) %>%
  dplyr::select(-c(2:17))
tmp <- tmp[c(6,2,5,4,3,1),]
table2[10:15,1:8] <- tmp[,2:9]
tmp <- calorie %>%  group_by(category) %>% summarise(count=sum(count)) %>% ungroup() %>%
  mutate(pct=sprintf('%.2f',count/sum(count)*100))
tmp <- tmp[c(6,2,5,4,3,1),]
table2[10:15,9] <- tmp[,3]

# panel 4, read by daypart data
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart-occasion/mean-calorie_restid_daypart_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","occasion","daypart","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(5:12)] <- sample07q1[, c(5:12)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart-occasion/mean-calorie_restid_daypart_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","occasion","daypart","calorie", "fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1) %>% filter(occasion==2) %>% dplyr::select(-occasion)
rm(sample, sample07q1, i, j)
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,daypart) %>% summarise_all(sum)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(calorie:sodium, ~ ./count))
tmp <- calorie %>% dplyr::select(daypart:sodium) %>% group_by(daypart) %>% summarise_all(list(mean=mean,sd=sd)) %>%
  mutate(cal=paste0(ifelse(calorie_mean>=100,sprintf('%.0f',calorie_mean),sprintf('%.2f',calorie_mean)),
                    " (",ifelse(calorie_sd>=100,sprintf('%.0f',calorie_sd),sprintf('%.2f',calorie_sd)),")")) %>%
  mutate(fat=paste0(ifelse(fat_mean>=100,sprintf('%.0f',fat_mean),sprintf('%.2f',fat_mean)),
                    " (",ifelse(fat_sd>=100,sprintf('%.0f',fat_sd),sprintf('%.2f',fat_sd)),")")) %>%
  mutate(carb=paste0(ifelse(carb_mean>=100,sprintf('%.0f',carb_mean),sprintf('%.2f',carb_mean)),
                     " (",ifelse(carb_sd>=100,sprintf('%.0f',carb_sd),sprintf('%.2f',carb_sd)),")")) %>%
  mutate(protein=paste0(ifelse(protein_mean>=100,sprintf('%.0f',protein_mean),sprintf('%.2f',protein_mean)),
                        " (",ifelse(protein_sd>=100,sprintf('%.0f',protein_sd),sprintf('%.2f',protein_sd)),")")) %>%
  mutate(sat_fat=paste0(ifelse(sat_fat_mean>=100,sprintf('%.0f',sat_fat_mean),sprintf('%.2f',sat_fat_mean)),
                        " (",ifelse(sat_fat_sd>=100,sprintf('%.0f',sat_fat_sd),sprintf('%.2f',sat_fat_sd)),")")) %>%
  mutate(sugar=paste0(ifelse(sugar_mean>=100,sprintf('%.0f',sugar_mean),sprintf('%.2f',sugar_mean)),
                      " (",ifelse(sugar_sd>=100,sprintf('%.0f',sugar_sd),sprintf('%.2f',sugar_sd)),")")) %>%
  mutate(fiber=paste0(ifelse(fiber_mean>=100,sprintf('%.0f',fiber_mean),sprintf('%.2f',fiber_mean)),
                      " (",ifelse(fiber_sd>=100,sprintf('%.0f',fiber_sd),sprintf('%.2f',fiber_sd)),")")) %>%
  mutate(sodium=paste0(ifelse(sodium_mean>=100,sprintf('%.0f',sodium_mean),sprintf('%.2f',sodium_mean)),
                       " (",ifelse(sodium_sd>=100,sprintf('%.0f',sodium_sd),sprintf('%.2f',sodium_sd)),")")) %>%
  dplyr::select(-c(2:17))
table2[16:21,1:8] <- tmp[,2:9]
table2$fiber <- gsub("-","",table2$fiber)
tmp <- calorie %>%  group_by(daypart) %>% summarise(count=sum(count)) %>% ungroup() %>%
  mutate(pct=sprintf('%.2f',count/sum(count)*100))
table2[16:21,9] <- tmp[,3]

table2 <- table2 %>%
  add_row(cal="",.before=1) %>% add_row(cal="",.before=5) %>% add_row(cal="",.before=5) %>%
  add_row(cal="",.before=13) %>% add_row(cal="",.before=13) %>%
  add_row(cal="",.before=21) %>% add_row(cal="",.before=21) %>%
  mutate_all(~replace(., is.na(.), ""))
#write.csv(table2, "manuscript/tables/table2.csv")
rm(tmp,table2,calorie,matched_use)

### appendix figure 3, nutrient analysis, change in nutrient per transaction ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","occasion","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(4:11)] <- sample07q1[, c(4:11)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","occasion","calorie","fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
#merge with restaurants and aggregate by restaurant-month
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,occasion) %>% summarise_all(sum)
tmp <- matched %>% dplyr::select(id:month,relative2,relative2.factor,id_match)
calorie <- merge(calorie,tmp,by=c("address","monthno","concept","ownership","tract_num")) %>%
  mutate(across(calorie:sodium, ~ ./count)) %>% filter(occasion==2) %>% dplyr::select(-occasion) %>%
  gather(key="nutrient",value="value","fat","carb","protein","sat_fat","sugar","fiber","sodium") %>%
  mutate(nutrient=factor(nutrient,levels = c("fat","carb","protein","sat_fat","sugar","fiber","sodium")))

trend <- NULL
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
for (i in c("fat","carb","protein","sat_fat","sugar","fiber","sodium")) {
  mod.factor <- plm(formula = value~treat*relative2.factor+as.factor(month),
                    data = calorie%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&nutrient==i), 
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
  tmp1 <- tidy_mod.factor %>% 
    filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month, diff) %>%
    mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
  tmp2 <- tidy_mod.factor %>% 
    filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month, diff) %>%
    arrange(month) %>% rename(post_mean = diff)
  tmp1 <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
    mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  tmp1 <- cbind(tmp1, tmp)
  tmp1$nutrient <- i
  trend <- rbind(trend,tmp1)
}
trend$nutrient <- factor(trend$nutrient,levels = c("fat","carb","protein","sat_fat","sugar","fiber","sodium"))

# figure with sodium
summary(trend$mean) #[-82,56]
ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=nutrient)) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=nutrient),size=1) +
  ggplot2::annotate(geom="label", x=6, y=-15, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-15),color="black",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-90,60),breaks=seq(-90,60,10)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
  scale_color_manual(name="Nutrient", labels=c("Total fat (g)","Carbohydrate (g)","Protein (g)","Saturated fat (g)","Sugar (g)","Fiber (g)","Sodium (mg)"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","red","grey","black")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("manuscript/figures/appendix-fig3-diff-in-diff-nutrients.jpeg", dpi="retina")

# figure without sodium
ggplot() + 
  geom_line(data=trend%>%filter(nutrient!="sodium"), aes(x=month, y=mean, color=nutrient)) + 
  geom_point(data=trend%>%filter(p<0.05&nutrient!="sodium"), aes(x=month, y=mean, color=nutrient),size=1) +
  ggplot2::annotate(geom="label", x=6, y=2, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=2),color="black",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-10,10),breaks=seq(-10,10,2)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="", x="Month", y="Difference-in-difference estimate", caption="") + 
  scale_color_manual(name="Nutrient", labels=c("Total fat (g)","Carbohydrate (g)","Protein (g)","Saturated fat (g)","Sugar (g)","Fiber (g)"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","red","grey")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank())
#ggsave("manuscript/figures/appendix-fig3-diff-in-diff-nutrients-wo-sodium.jpeg", dpi="retina")
rm(tmp,tmp1,tmp2,mod.factor,tidy_mod.factor,i,j,presum,trend,restaurant,calorie)

### appendix table 1, number of restaurants, by open time ----
app_table1 <- data.frame(matrix(data=NA,ncol = 7,nrow = 14))
colnames(app_table1)[1:7] <- c("loc","unmatched","matched_baseline","open12","open18","open24","open30")
app_table1$loc <- c("trea_all","comp_all","ca","ca_comp","ma","ma_comp","or","or_comp",
                    "mont","mont_comp","suffolk","suffolk_comp","king","king_comp")

#get indicator for open 30 months post labeling
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=29&relative2>=0) %>% mutate(n=n()) %>% mutate(after30=ifelse(n==30,1,0)) %>%
  dplyr::select(id,treat,match_place,after30) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id","treat","match_place"),all=TRUE)
rm(tmp)

#run the restaurants-matching-drive-thru.R script up to line 325
restaurant <- restaurant %>%  mutate(id = group_indices(., address, tract_num, ownership, concept)) 
length(unique(restaurant$id[restaurant$treat==1])) #1208
length(unique(restaurant$id[restaurant$treat==0])) #5645
app_table1$unmatched[1:2] <-c(1208,5645)
for (i in c("ca","ma","or","mont","suffolk","king")) {
  print(paste0(i,": ",length(unique(restaurant$id[restaurant$treat==1&restaurant$policy==i]))))
  app_table1$unmatched[app_table1$loc==i] <- length(unique(restaurant$id[restaurant$treat==1&restaurant$policy==i]))
}
rm(i)

#open ever
length(unique(matched$id_match[matched$treat==1&matched$relative2==0])) #506
length(unique(matched$id_match[matched$treat==0&matched$relative2==0])) #1518
app_table1$matched_baseline[1:2] <-c(506,1518)
for (i in c("ca","ma","or","mont","suffolk","king")) {
  app_table1$matched_baseline[app_table1$loc==i] <- length(unique(matched$id_match[matched$treat==1&matched$match_place==i]))
  app_table1$matched_baseline[app_table1$loc==paste0(i,"_comp")] <- length(unique(matched$id_match[matched$treat==0&matched$match_place==i]))
  app_table1$open12[app_table1$loc==i] <- length(unique(matched$id_match[matched$treat==1&matched$match_place==i&matched$after12==1]))
  app_table1$open12[app_table1$loc==paste0(i,"_comp")] <- length(unique(matched$id_match[matched$treat==0&matched$match_place==i&matched$after12==1]))
  app_table1$open18[app_table1$loc==i] <- length(unique(matched$id_match[matched$treat==1&matched$match_place==i&matched$after18==1]))
  app_table1$open18[app_table1$loc==paste0(i,"_comp")] <- length(unique(matched$id_match[matched$treat==0&matched$match_place==i&matched$after18==1]))
  app_table1$open24[app_table1$loc==i] <- length(unique(matched$id_match[matched$treat==1&matched$match_place==i&matched$after24==1]))
  app_table1$open24[app_table1$loc==paste0(i,"_comp")] <- length(unique(matched$id_match[matched$treat==0&matched$match_place==i&matched$after24==1]))
  app_table1$open30[app_table1$loc==i] <- length(unique(matched$id_match[matched$treat==1&matched$match_place==i&matched$after30==1]))
  app_table1$open30[app_table1$loc==paste0(i,"_comp")] <- length(unique(matched$id_match[matched$treat==0&matched$match_place==i&matched$after30==1]))
}
app_table1[1,4:7] <- app_table1[3,4:7]+app_table1[5,4:7]+app_table1[7,4:7]+app_table1[9,4:7]+app_table1[11,4:7]+app_table1[13,4:7]
app_table1[2,4:7] <- app_table1[4,4:7]+app_table1[6,4:7]+app_table1[8,4:7]+app_table1[10,4:7]+app_table1[12,4:7]+app_table1[14,4:7]
app_table1 <- app_table1 %>% mutate(open12=paste0(open12," (",sprintf('%.2f',open12/matched_baseline*100),"%)")) %>%
  mutate(open18=paste0(open18," (",sprintf('%.2f',open18/matched_baseline*100),"%)")) %>%
  mutate(open24=paste0(open24," (",sprintf('%.2f',open24/matched_baseline*100),"%)")) %>%
  mutate(open30=paste0(open30," (",sprintf('%.2f',open30/matched_baseline*100),"%)")) %>%
  mutate_all(~replace(., is.na(.), "-")) %>%
  add_row(unmatched="",.before=3) %>% add_row(unmatched="",.before=6) %>% add_row(unmatched="",.before=9) %>%
  add_row(unmatched="",.before=12) %>% add_row(unmatched="",.before=15) %>% add_row(unmatched="",.before=18) %>%
  mutate_all(~replace(., is.na(.), ""))
#write.csv(app_table1,"manuscript/tables/app_table1.csv")  

### appendix table 3, compare top selling items, items sold in every month of the study period ----
app_table3 <- data.frame(matrix(data=NA,ncol = 3,nrow = 6)) %>% setNames(c("baseline","followup","p")) %>%
  add_column(sample=c("treat, each period","comp, each period","treat, all period","comp, all period","treat, new items","comp, new items"),.before = 1)
# compare top 100 items for treated and comparison group
# aggregate items every sold in treated restaurants, find the items that were sold in every month
# do the same for comparison restaurants
sample07q1 <- read.csv("data/from-bigpurple/top-selling-items/top-selling_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("monthno","restid","product","calorie","full","qty"))
sample07q1$qty <- sample07q1$qty/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/top-selling-items/top-selling_restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("monthno","restid","product","calorie","full","qty"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid,-product) %>%
  group_by(address,concept,ownership,monthno,tract_num,calorie,full) %>% summarise(qty=sum(qty))
match_use <- matched %>% dplyr::select(id:concept,monthno:month,relative2)
calorie <- merge(calorie,match_use,by=c("address","concept","ownership","tract_num","monthno"))
rm(restaurant,matched_use)

# panel 1
# figure out which items sold in every month during baseline and follow-up period, -8 to -3 and 3 to 12
# get top 100 selling items for both groups, in pre- and post-periods
tmp <- calorie %>% filter(relative2>=-8 & relative2<=11) %>% 
  dplyr::select(treat,relative2,full,calorie,qty) %>% group_by(treat,relative2,full,calorie) %>% summarise(qty=sum(qty)) %>%
  arrange(treat,full,relative2) %>% group_by(treat,full) %>% mutate(rank=row_number()) %>%
  mutate(rank=sum(rank)) %>% filter(rank==210) %>% mutate(post=ifelse(relative2<0,0,1)) %>%
  group_by(treat,post,full,calorie) %>% summarise(qty=sum(qty)) %>% arrange(treat,post,desc(qty)) %>%
  group_by(treat,post) %>% mutate(rank=row_number()) %>% filter(rank<=100)

for (i in c(0,1)) {
  app_table3[1,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==1&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==1&tmp$post==i])),")")
  app_table3[2,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==0&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==0&tmp$post==i])),")")
}
app_table3[1,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==1&tmp$post==0],tmp$calorie[tmp$treat==1&tmp$post==1])$p.value)
app_table3[2,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==0&tmp$post==0],tmp$calorie[tmp$treat==0&tmp$post==1])$p.value)

# panel 2
# figure out which items sold in every month during the entire study period
# get top 100 selling items for both groups, in pre- and post-periods
tmp1 <- calorie %>% 
  dplyr::select(treat,monthno,full,calorie,qty,relative2) %>% group_by(treat,monthno,full,calorie) %>%
  summarise(qty=sum(qty)) %>% arrange(treat,full,monthno) %>% group_by(treat,full) %>%
  mutate(rank=row_number()) %>% mutate(rank=max(rank)) %>% filter(rank==106) %>% dplyr::select(treat,full) %>% distinct()
#merge the list of items back to data
tmp <- calorie %>% dplyr::select(treat,full,calorie,qty,relative2) %>% right_join(y=tmp1,by=c("full","treat")) %>%
  mutate(post=ifelse(relative2<0,0,1)) %>% group_by(treat,post,full,calorie) %>% summarise(qty=sum(qty)) %>%
  arrange(treat,post,desc(qty)) %>% group_by(treat,post) %>% mutate(rank=row_number()) #num of items consistently sold during entire period is smaller than 100
for (i in c(0,1)) {
  app_table3[3,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==1&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==1&tmp$post==i])),")")
  app_table3[4,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==0&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==0&tmp$post==i])),")")
}
app_table3[3,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==1&tmp$post==0],tmp$calorie[tmp$treat==1&tmp$post==1])$p.value)
app_table3[4,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==0&tmp$post==0],tmp$calorie[tmp$treat==0&tmp$post==1])$p.value)

# panel 3
# newly introduced items in baseline and follow-up period
# get list of items ever sold in relative2 < -8, get list of items sold between -8 and -3, anti join the two lists 
# get list of items ever sold in relative2 <= -3, get list of items sold between 3 and 12, anti join the two lists
tmp <- calorie %>% dplyr::select(treat,relative2,full,calorie) %>% filter(relative2<=-3) %>% mutate(new=ifelse(relative2< -8,0,1)) %>%
  distinct(treat,new,full,calorie) %>% arrange(full,treat,new) %>% group_by(treat,full) %>%
  mutate(rank=row_number()) %>% mutate(total=max(rank)) %>% filter(new==1&rank==1&total==1) %>% dplyr::select(treat,full,calorie) %>%
  mutate(post=0)
tmp1 <- calorie %>% dplyr::select(treat,relative2,full,calorie) %>% filter(relative2<= -3 |(relative2>=2&relative2<=11)) %>% mutate(new=ifelse(relative2<= -3,0,1)) %>%
  distinct(treat,new,full,calorie) %>% arrange(full,treat,new) %>% group_by(treat,full) %>%
  mutate(rank=row_number()) %>% mutate(total=max(rank)) %>% filter(new==1&rank==1&total==1) %>% dplyr::select(treat,full,calorie) %>%
  mutate(post=1)
tmp <- rbind(tmp,tmp1) %>% ungroup()
for (i in c(0,1)) {
  app_table3[5,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==1&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==1&tmp$post==i])),")")
  app_table3[6,i+2] <- paste0(sprintf('%.0f',mean(tmp$calorie[tmp$treat==0&tmp$post==i]))," (",sprintf('%.0f',sd(tmp$calorie[tmp$treat==0&tmp$post==i])),")")
}
app_table3[5,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==1&tmp$post==0],tmp$calorie[tmp$treat==1&tmp$post==1])$p.value)
app_table3[6,4] <- sprintf('%.3f',t.test(tmp$calorie[tmp$treat==0&tmp$post==0],tmp$calorie[tmp$treat==0&tmp$post==1])$p.value)
#get sales % of these new items
all <- calorie %>% dplyr::select(full,qty) %>% group_by(full) %>% summarise(qty=sum(qty)) %>% arrange(desc(qty)) %>% mutate(pct=qty/sum(qty))
tmp <- tmp %>% dplyr::select(full) %>% distinct() %>% left_join(y=all,by="full") %>% summarise(pct=sum(pct))
app_table3<- app_table3 %>% add_row(sample="",.before=3) %>% add_row(sample="",.before=6) %>% add_row(sample="For panel3, new items introduced in both baseline and follow-up period represent approximately 5.45% of sales.") %>%
  mutate_all(~replace(., is.na(.), "")) 
write.csv(app_table3,"manuscript/tables/app_table3.csv",row.names = FALSE)
rm(app_table3,tmp,tmp1,i,match_use,calorie,all)

### appendix table X, baseline characteristics ----
appen_tablex <- data.frame(matrix(data=NA, ncol=14, nrow = 32)) %>% 
  setNames(c("all","all_comp","ca","ca_comp","ma","ma_comp","or","or_comp","mont","mont_comp","suffolk","suffolk_comp","king","king_comp")) %>%
  add_column(character=c("n","ownership","concept","open12","open18","open24","num_transaction","num_transaction_trend",
                         "drive_transaction","drive_transaction_trend","calorie","calorie_trend","drive_calorie","drive_calorie_trend",
                         "spending","spending_trend","drive_spedning","drive_spending_trend","drive%","lunch_dinner%",
                         "pop","under18","over65","male","asian","black","hisp","white","median_income","income_capita","no_hs","collegeup"),.before=1)
index <- data.frame(1:6,c("ca","ma","or","mont","suffolk","king")) %>% setNames(c("index","col"))
#run restaurants-matching-drive-thru.R, the entire script
length(unique(paste0(matched$address,matched$tract_num,matched$concept,matched$ownership,matched$match_place)))
#all treated and comparison restaruants
appen_tablex[1:6,2] <- c(sum(matched$treat==1),sum(matched$treat==1&matched$ownership==1),sum(matched$treat==1&matched$concept==1),
                         sum(matched$treat==1&matched$open12==1),sum(matched$treat==1&matched$open18==1),sum(matched$treat==1&matched$open24==1))
appen_tablex[1:6,3] <- c(sum(matched$treat==0),sum(matched$treat==0&matched$ownership==1),sum(matched$treat==0&matched$concept==1),
                         sum(matched$treat==0&matched$open12==1),sum(matched$treat==0&matched$open18==1),sum(matched$treat==0&matched$open24==1))
appen_tablex[c(19:20,22:28,31:32),2] <- c(mean(matched$drive[matched$treat==1])*100,mean(matched$meal[matched$treat==1])*100,mean(matched$under18[matched$treat==1])*100,
                                          mean(matched$above65[matched$treat==1])*100,mean(matched$male[matched$treat==1])*100,mean(matched$asian[matched$treat==1])*100,
                                          mean(matched$black[matched$treat==1])*100,mean(matched$hisp[matched$treat==1])*100,mean(matched$white[matched$treat==1])*100,
                                          mean(matched$hsbelow[matched$treat==1])*100,mean(matched$collegeup[matched$treat==1])*100)
appen_tablex[c(19:20,22:28,31:32),3] <- c(mean(matched$drive[matched$treat==0])*100,mean(matched$meal[matched$treat==0])*100,mean(matched$under18[matched$treat==0])*100,
                                          mean(matched$above65[matched$treat==0])*100,mean(matched$male[matched$treat==0])*100,mean(matched$asian[matched$treat==0])*100,
                                          mean(matched$black[matched$treat==0])*100,mean(matched$hisp[matched$treat==0])*100,mean(matched$white[matched$treat==0])*100,
                                          mean(matched$hsbelow[matched$treat==0])*100,mean(matched$collegeup[matched$treat==0])*100)
# by location
for (i in 1:6) {
    appen_tablex[1:6,2*i+2] <- c(sum(matched$treat==1&matched$match_place==index[i,2]),sum(matched$treat==1&matched$ownership==1&matched$match_place==index[i,2]),sum(matched$treat==1&matched$concept==1&matched$match_place==index[i,2]),
                            sum(matched$treat==1&matched$open12==1&matched$match_place==index[i,2]),sum(matched$treat==1&matched$open18==1&matched$match_place==index[i,2]),sum(matched$treat==1&matched$open24==1&matched$match_place==index[i,2]))
    appen_tablex[1:6,2*i+3] <- c(sum(matched$treat==0&matched$match_place==index[i,2]),sum(matched$treat==0&matched$ownership==1&matched$match_place==index[i,2]),sum(matched$treat==0&matched$concept==1&matched$match_place==index[i,2]),
                                 sum(matched$treat==0&matched$open12==1&matched$match_place==index[i,2]),sum(matched$treat==0&matched$open18==1&matched$match_place==index[i,2]),sum(matched$treat==0&matched$open24==1&matched$match_place==index[i,2]))
    appen_tablex[c(19:20,22:28,31:32),2*i+2] <- c(mean(matched$drive[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$meal[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$under18[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$above65[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$male[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$asian[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$black[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$hisp[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$white[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$hsbelow[matched$treat==1&matched$match_place==index[i,2]])*100,
                                                  mean(matched$collegeup[matched$treat==1&matched$match_place==index[i,2]])*100)
    appen_tablex[c(19:20,22:28,31:32),2*i+3] <- c(mean(matched$drive[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$meal[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$under18[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$above65[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$male[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$asian[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$black[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$hisp[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$white[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$hsbelow[matched$treat==0&matched$match_place==index[i,2]])*100,
                                                  mean(matched$collegeup[matched$treat==0&matched$match_place==index[i,2]])*100)
}
#convert to %
appen_tablex[3,2:15] <- (appen_tablex[1,2:15] - appen_tablex[3,2:15])/appen_tablex[1,2:15]*100
for (i in c(2,4:6)) {
  appen_tablex[i,2:15] <- appen_tablex[i,2:15]/appen_tablex[1,2:15]*100
}
#fix decimal points
appen_tablex[1,2:15] <- sprintf('%.0f',appen_tablex[1,2:15])
for (i in c(2:6,19:20,22:28,31:32)) {
  appen_tablex[i,2:15] <- sprintf('%.2f',appen_tablex[i,2:15])
}
#transaction related stats
appen_tablex[c(7:18,21,29:30),2] <- c(paste0(ifelse(abs(mean(matched$count_all3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$count_all3[matched$treat==1])),sprintf('%.2f',mean(matched$count_all3[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$count_all3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$count_all3[matched$treat==1])),sprintf('%.2f',sd(matched$count_all3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_count_all[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_count_all[matched$treat==1])),sprintf('%.2f',mean(matched$slope_count_all[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_count_all[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_count_all[matched$treat==1])),sprintf('%.2f',sd(matched$slope_count_all[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$count3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$count3[matched$treat==1])),sprintf('%.2f',mean(matched$count3[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$count3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$count3[matched$treat==1])),sprintf('%.2f',sd(matched$count3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_count[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_count[matched$treat==1])),sprintf('%.2f',mean(matched$slope_count[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_count[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_count[matched$treat==1])),sprintf('%.2f',sd(matched$slope_count[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$calorie_all3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$calorie_all3[matched$treat==1])),sprintf('%.2f',mean(matched$calorie_all3[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$calorie_all3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$calorie_all3[matched$treat==1])),sprintf('%.2f',sd(matched$calorie_all3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_calorie_all[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_calorie_all[matched$treat==1])),sprintf('%.2f',mean(matched$slope_calorie_all[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_calorie_all[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_calorie_all[matched$treat==1])),sprintf('%.2f',sd(matched$slope_calorie_all[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$calorie3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$calorie3[matched$treat==1])),sprintf('%.2f',mean(matched$calorie3[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$calorie3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$calorie3[matched$treat==1])),sprintf('%.2f',sd(matched$calorie3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_calorie[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_calorie[matched$treat==1])),sprintf('%.2f',mean(matched$slope_calorie[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_calorie[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_calorie[matched$treat==1])),sprintf('%.2f',sd(matched$slope_calorie[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$dollar_all3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$dollar_all3[matched$treat==1])),sprintf('%.2f',mean(matched$dollar_all3)))," (",
                                          ifelse(abs(sd(matched$dollar_all3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$dollar_all3[matched$treat==1])),sprintf('%.2f',sd(matched$dollar_all3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_dollar_all[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_dollar_all[matched$treat==1])),sprintf('%.2f',mean(matched$slope_dollar_all[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_dollar_all[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_dollar_all[matched$treat==1])),sprintf('%.2f',sd(matched$slope_dollar_all[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$dollar3[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$dollar3[matched$treat==1])),sprintf('%.2f',mean(matched$dollar3[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$dollar3[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$dollar3[matched$treat==1])),sprintf('%.2f',sd(matched$dollar3[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$slope_dollar[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$slope_dollar[matched$treat==1])),sprintf('%.2f',mean(matched$slope_dollar[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$slope_dollar[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$slope_dollar[matched$treat==1])),sprintf('%.2f',sd(matched$slope_dollar[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$total[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$total[matched$treat==1])),sprintf('%.2f',mean(matched$total[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$total[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$total[matched$treat==1])),sprintf('%.2f',sd(matched$total[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$median_income[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$median_income[matched$treat==1])),sprintf('%.2f',mean(matched$median_income[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$median_income[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$median_income[matched$treat==1])),sprintf('%.2f',sd(matched$median_income[matched$treat==1]))),")"),
                                   paste0(ifelse(abs(mean(matched$capital_income[matched$treat==1]))>=100,sprintf('%.0f',mean(matched$capital_income[matched$treat==1])),sprintf('%.2f',mean(matched$capital_income[matched$treat==1])))," (",
                                          ifelse(abs(sd(matched$capital_income[matched$treat==1]))>=100,sprintf('%.0f',sd(matched$capital_income[matched$treat==1])),sprintf('%.2f',sd(matched$capital_income[matched$treat==1]))),")"))
appen_tablex[c(7:18,21,29:30),3] <- c(paste0(ifelse(abs(mean(matched$count_all3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$count_all3[matched$treat==0])),sprintf('%.2f',mean(matched$count_all3[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$count_all3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$count_all3[matched$treat==0])),sprintf('%.2f',sd(matched$count_all3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_count_all[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_count_all[matched$treat==0])),sprintf('%.2f',mean(matched$slope_count_all[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_count_all[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_count_all[matched$treat==0])),sprintf('%.2f',sd(matched$slope_count_all[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$count3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$count3[matched$treat==0])),sprintf('%.2f',mean(matched$count3[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$count3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$count3[matched$treat==0])),sprintf('%.2f',sd(matched$count3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_count[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_count[matched$treat==0])),sprintf('%.2f',mean(matched$slope_count[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_count[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_count[matched$treat==0])),sprintf('%.2f',sd(matched$slope_count[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$calorie_all3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$calorie_all3[matched$treat==0])),sprintf('%.2f',mean(matched$calorie_all3[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$calorie_all3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$calorie_all3[matched$treat==0])),sprintf('%.2f',sd(matched$calorie_all3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_calorie_all[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_calorie_all[matched$treat==0])),sprintf('%.2f',mean(matched$slope_calorie_all[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_calorie_all[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_calorie_all[matched$treat==0])),sprintf('%.2f',sd(matched$slope_calorie_all[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$calorie3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$calorie3[matched$treat==0])),sprintf('%.2f',mean(matched$calorie3[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$calorie3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$calorie3[matched$treat==0])),sprintf('%.2f',sd(matched$calorie3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_calorie[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_calorie[matched$treat==0])),sprintf('%.2f',mean(matched$slope_calorie[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_calorie[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_calorie[matched$treat==0])),sprintf('%.2f',sd(matched$slope_calorie[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$dollar_all3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$dollar_all3[matched$treat==0])),sprintf('%.2f',mean(matched$dollar_all3)))," (",
                                             ifelse(abs(sd(matched$dollar_all3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$dollar_all3[matched$treat==0])),sprintf('%.2f',sd(matched$dollar_all3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_dollar_all[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_dollar_all[matched$treat==0])),sprintf('%.2f',mean(matched$slope_dollar_all[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_dollar_all[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_dollar_all[matched$treat==0])),sprintf('%.2f',sd(matched$slope_dollar_all[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$dollar3[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$dollar3[matched$treat==0])),sprintf('%.2f',mean(matched$dollar3[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$dollar3[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$dollar3[matched$treat==0])),sprintf('%.2f',sd(matched$dollar3[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$slope_dollar[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$slope_dollar[matched$treat==0])),sprintf('%.2f',mean(matched$slope_dollar[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$slope_dollar[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$slope_dollar[matched$treat==0])),sprintf('%.2f',sd(matched$slope_dollar[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$total[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$total[matched$treat==0])),sprintf('%.2f',mean(matched$total[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$total[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$total[matched$treat==0])),sprintf('%.2f',sd(matched$total[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$median_income[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$median_income[matched$treat==0])),sprintf('%.2f',mean(matched$median_income[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$median_income[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$median_income[matched$treat==0])),sprintf('%.2f',sd(matched$median_income[matched$treat==0]))),")"),
                                      paste0(ifelse(abs(mean(matched$capital_income[matched$treat==0]))>=100,sprintf('%.0f',mean(matched$capital_income[matched$treat==0])),sprintf('%.2f',mean(matched$capital_income[matched$treat==0])))," (",
                                             ifelse(abs(sd(matched$capital_income[matched$treat==0]))>=100,sprintf('%.0f',sd(matched$capital_income[matched$treat==0])),sprintf('%.2f',sd(matched$capital_income[matched$treat==0]))),")"))
for (i in 1:6) {
  appen_tablex[c(7:18,21,29:30),2*i+2] <- c(paste0(ifelse(abs(mean(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$count_all3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_count_all[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$count3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$count3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$count3[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$count3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$count3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$count3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_count[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$calorie_all3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_calorie_all[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$calorie3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_calorie[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$dollar_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$dollar_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$dollar_all3)))," (",
                                                   ifelse(abs(sd(matched$dollar_all3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$dollar_all3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$dollar_all3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_dollar_all[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$dollar3[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_dollar[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$total[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$total[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$total[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$total[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$total[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$total[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$median_income[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$median_income[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$median_income[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$median_income[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$median_income[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$median_income[matched$treat==1&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$capital_income[matched$treat==1&matched$match_place==index[i,2]]))),")"))
  appen_tablex[c(7:18,21,29:30),2*i+3] <- c(paste0(ifelse(abs(mean(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$count_all3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_count_all[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$count3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$count3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$count3[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$count3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$count3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$count3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_count[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$calorie_all3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_calorie_all[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$calorie3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_calorie[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$dollar_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$dollar_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$dollar_all3)))," (",
                                                   ifelse(abs(sd(matched$dollar_all3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$dollar_all3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$dollar_all3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_dollar_all[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$dollar3[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$slope_dollar[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$total[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$total[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$total[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$total[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$total[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$total[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$median_income[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$median_income[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$median_income[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$median_income[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$median_income[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$median_income[matched$treat==0&matched$match_place==index[i,2]]))),")"),
                                            paste0(ifelse(abs(mean(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',mean(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',mean(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]])))," (",
                                                   ifelse(abs(sd(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]]))>=100,sprintf('%.0f',sd(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]])),sprintf('%.2f',sd(matched$capital_income[matched$treat==0&matched$match_place==index[i,2]]))),")"))
}
appen_tablex <- appen_tablex %>%  
  add_row(character=NA,.before=21) %>% add_row(character=NA,.before=21) %>%
  add_column(space="",.before = 4) %>% add_column(space="",.before = 7) %>% add_column(space="",.before = 10) %>%
  add_column(space="",.before = 13) %>% add_column(space="",.before = 16) %>% add_column(space="",.before =19) %>%
  mutate_all(~replace(., is.na(.), "")) 
#write.csv(appen_tablex,"manuscript/tables/app_tableX.csv",row.names = FALSE)






### appendix table 2, diff-in-diff analysis in item category, time of day and nutrients ----
app_table2 <- data.frame(matrix(data=NA, nrow=19,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(type=c("taco","burrito","salad","other","desserts","bev","latenight","breakfast","lunch","afternoon","dinner","evening",
                    "fat","carb","protein","sat_fat","sugar","fiber","sodium"),.before = 1)
#panel 1: food category
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("monthno","restid","category","occasion","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(5:12)] <- sample07q1[, c(5:12)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("monthno","restid","category","occasion","calorie", "fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
calorie <- calorie %>% filter(occasion==2&(category %in% c(8,2,5,4,3,1))) %>% dplyr::select(-occasion)
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,category) %>% summarise_all(sum)
matched_use <- matched %>% dplyr::select(id:month,relative2,relative2.factor,id_match)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(calorie:sodium, ~ ./count)) %>% dplyr::select(-count)
index <- data.frame(c(8,2,5,4,3,1),c("taco","burrito","salad","other","dessert","bev")) %>%setNames(c("index","food"))
for (i in 1:6) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = calorie%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==index[i,1]), 
                    index = "id_match", weights = weights, model = "within")
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
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i,2] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i,3] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i,4] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i,5] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i,6] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i,7] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}

# panel 2: time of day
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart-occasion/mean-calorie_restid_daypart_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","occasion","daypart","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(5:12)] <- sample07q1[, c(5:12)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-daypart-occasion/mean-calorie_restid_daypart_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","occasion","daypart","calorie", "fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
calorie <- calorie %>% filter(occasion==2) %>% dplyr::select(-occasion)
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num,daypart) %>% summarise_all(sum)
matched_use <- matched %>% dplyr::select(id:month,relative2,relative2.factor,id_match)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(calorie:sodium, ~ ./count)) %>% dplyr::select(-count)
for (i in 1:6) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = calorie%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&daypart==i), 
                    index = "id_match", weights = weights, model = "within")
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
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+6,2] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+6,3] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i+6,4] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+6,5] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+6,6] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i+6,7] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}

#panel 3: nutrients
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","occasion","calorie", "fat","carb",
                                     "protein","sat_fat","sugar","fiber","sodium", "count"))
sample07q1[, c(4:11)] <- sample07q1[, c(4:11)]/2
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-occasion/mean-calorie_restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","occasion","calorie", "fat","carb",
                                       "protein","sat_fat","sugar","fiber","sodium", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
calorie <- calorie %>% filter(occasion==2) %>% dplyr::select(-occasion, -calorie) 

restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",
                       colClasses=c("integer","character",rep("NULL",7),"character",rep("NULL",3),"character",rep("NULL",15),"character")) %>%
  mutate(concept=ifelse(concept=="TBC",1,0),ownership=ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(tract_num=substr(tract_num,2,12))
calorie <- merge(calorie,restaurant,by="restid",all.x = TRUE) %>% dplyr::select(-restid) %>%
  group_by(address,concept,ownership,monthno,tract_num) %>% summarise_all(sum)
matched_use <- matched %>% dplyr::select(id:month,relative2,relative2.factor,id_match)
calorie <- merge(calorie,matched_use,by=c("address","monthno","concept","ownership","tract_num"))  %>%
  mutate(across(fat:sodium, ~ ./count)) %>% dplyr::select(-count) %>%
gather(key="nutrient",value="value","fat","carb","protein","sat_fat","sugar","fiber","sodium") 
index <- data.frame(c(1:7),c("fat","carb","protein","sat_fat","sugar","fiber","sodium")) %>%setNames(c("index","nutrient"))
for (i in 1:7) {
  mod.factor <- plm(formula = value~treat*relative2.factor+as.factor(month),
                    data = calorie%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&nutrient==index[i,2]), 
                    index = "id_match", weights = weights, model = "within")
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
    mutate(calorie = ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
    mutate(high = ifelse(group==0,high,high+high[1:56]))
  # diff in labeled group, months 3-12
  #columns 1-3
  treat <- tidy_mod.factor %>% filter(group==1&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+12,2] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&month>=-8&month<=12) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+12,3] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i+12,4] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
  #columns 4-6
  treat <- tidy_mod.factor %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+12,5] <- paste0(sprintf('%.3f',treat$beta)," (",sprintf('%.3f',treat$low),", ",sprintf('%.3f',treat$high),")")
  comp <- tidy_mod.factor %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
    mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
    summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
    mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
    filter(post==1)
  app_table2[i+12,6] <- paste0(sprintf('%.3f',comp$beta)," (",sprintf('%.3f',comp$low),", ",sprintf('%.3f',comp$high),")")
  app_table2[i+12,7] <- paste0(sprintf('%.3f',treat$beta-comp$beta)," (",sprintf('%.3f',treat$low-comp$low),", ",sprintf('%.3f',treat$high-comp$high),")")
}
app_table2 <- app_table2 %>%  add_row(diff_treat_3_12="",.before=7) %>% add_row(diff_treat_3_12="",.before=7) %>%
  add_row(diff_treat_3_12="",.before=15) %>% add_row(diff_treat_3_12="",.before=15) %>%
  add_column(col="",.before=5) %>%
  mutate_all(~replace(., is.na(.), ""))
write.csv(app_table2,"manuscript/tables/app_table2.csv",row.names = FALSE)
rm(app_table2,calorie,comp,index,matched_use,mod.factor,restaurant,treat,tidy_mod.factor,i)








### contextual information ----
#number of comparison restaurants located in TX
length(unique(paste0(matched$state[matched$treat==0],matched$county[matched$treat==0]))) #328
length(unique(matched$state[matched$treat==0])) #34
length(unique(matched$id_match[matched$state=="TX"]))/length(unique(matched$id_match[matched$treat==0])) #0.1588

### appendix figure 4AB, diff in diff by food category ----
#use the categorize-food.R script
### appendix figure 6, diff in diff by open time ----
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
tmp1 <- tidy_mod.factor_all %>% group_by(time) %>%
  filter(month>=-30&month<0) %>% dplyr::select(month,diff,time) %>%
  mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_all %>% 
  filter(month>=1&month<=30) %>% dplyr::select(month, diff,time) %>%
  arrange(month) %>% rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by=c("month","time")) %>% group_by(time,month) %>% arrange(time,month) %>%
  mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff) 
rm(tmp1,tmp2)
#hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
p <- NULL
for (i in c(12,15,18,21,24,27,30)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&open_after>=i), 
                    index = "id_match", weights = weights, model = "within")
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  p <- rbind(p,tmp)
}
trend <- cbind(trend, p)
rm(i,j,tmp,p,presum)

ggplot() + #
  geom_point(data=trend%>%filter(p<0.05),aes(x=month, y=mean,color=as.character(time)),size=1) +
  geom_line(data=trend,aes(x=month, y=mean,color=as.character(time))) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,25),breaks=seq(-100,25,25)) +
  scale_x_continuous(breaks=c(3:30)) + 
    labs(title="", x="Month", y="Calories") + 
  scale_color_manual(name="Open after labeling", labels=c("12 months","15 months","18 months","21 months","24 months","27 months","30 months"),
                     values=c("hotpink","olivedrab3","red","orange","grey","purple","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig6-diff-in-diff-open-time.jpeg", dpi="retina")
rm(mod.factor,tidy_mod.factor,tidy_mod.factor_all,trend)

### appenfix figure 5AB, diff in diff, by meal time ----
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

tmp<-matched[is.na(matched$year),]

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
matched <- matched[!is.na(matched$year), -c(13:21)] #delete transaction data related to overall calorie info
meal <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
### preparing data, meal-time
meal$tract_num <- substr(meal$tract_num, 2, 12)
meal <- meal %>% filter(complete.cases(meal)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:dollar) %>% 
  arrange(id, meal, monthno) %>% group_by(id, treat, match_place, meal) %>%
  mutate(rank=row_number(id)) %>% mutate(open_month=max(rank))
summary(meal$rank) #sanity check, the max should be 106
meal$relative2 <- meal$monthno - meal$entry #month 0 is first month of ML
meal$relative2.factor <- factor(meal$relative2)
meal <- within(meal, relative2.factor<-relevel(relative2.factor, ref="0"))
meal$id_match <- paste0(meal$id, meal$match_place)
#figure 5B
tidy_mod.factor_all <- NULL
for (i in c(1:6)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = meal%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&meal==i), 
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
  tidy_mod.factor$meal <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
  tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]
}
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
#hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
p <- NULL
for (i in c(1:6)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = meal%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&meal==i), 
                    index = "id_match", weights = weights, model = "within")
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  p <- rbind(p,tmp)
}
trend <- cbind(trend, p)
rm(i,j,tmp,p,presum)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(meal))) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=factor(meal))) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=6, y=-100, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-100),color="black",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-220,40),breaks=seq(-220,40,20)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="", x="Month", y="Calories") + 
  scale_color_manual(name="Meal time",values = c("hotpink","olivedrab3","#13B0E4","purple","orange","grey"),
                     labels=c("Late night (00:00-03:59)","Breakfast (04:00-10:59)","Lunch (11:00-13:59)",
                              "Afternoon (14:00-16:59)","Dinner (17:00-20:59)","Evening (21:00-23:59)")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig5B-diff-in-diff-mealtime.jpeg", dpi="retina")

#figure 5A
meal <- meal %>% dplyr::select(year,month,count,meal) %>% group_by(year,month,meal) %>%
  summarise(count=sum(count)) %>% group_by(year,month) %>% mutate(total=sum(count)) %>%
  mutate(pct=count/total)

ggplot(data=meal,aes(x=interaction(year,month,lex.order=TRUE),y=pct,color=factor(meal),group=factor(meal))) +
  geom_line() + 
  ggplot2::annotate(geom="text",x=1:106,y=-0.01,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.03,label=unique(meal$year),size=3) +
  coord_cartesian(ylim=c(0,0.35), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.1,0.35),breaks=seq(-0.1,0.5,0.05),labels=scales::percent) + #important to set ylim here to include text labels
  labs(title="",x="Time",y="Percent of sales") + 
  scale_color_manual(name="Meal time",values = c("hotpink","olivedrab3","#13B0E4","purple","orange","grey"),
                     labels=c("Late night (00:00-03:59)","Breakfast (04:00-10:59)","Lunch (11:00-13:59)",
                              "Afternoon (14:00-16:59)","Dinner (17:00-20:59)","Evening (21:00-23:59)")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig5A-sales-by-mealtime.jpeg", dpi="retina")
rm(calorie,meal,mod.factor,tidy_mod.factor,tidy_mod.factor_all,trend,restaurant)

### appendix figure 1, unmatched data ----
# refer to aim1-diff-in-diff-unmatched.R
### appendix figure 2, unmatched data, diff in diff ----
#refer to aim1-diff-in-diff-unmatched.R script