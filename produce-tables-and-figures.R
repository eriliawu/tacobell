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

### fig 1, cal=group*month(factor), diff-in-diff line underneath ----
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

# add year and month factor as covariate
summary(trend$calorie) #[-95,132]
summary(trend$mean) #[-89,10]
ggplot(data=trend,aes(x=month, y=calorie,color=as.character(group))) + 
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
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/fig1-diff-in-diff.jpeg", dpi="retina")

### fig3, diff in diff, by location ----
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
rm(tmp1,tmp2,tmp,mod.factor,i,p,tidy_mod.factor,trend)

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

### table 2, calories and nutrients in follow-up period ----
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
  mutate(across(calorie:sodium, ~ ./count)) %>% dplyr::select(-count)

#aggregate panel 1, by order type
table2 <- data.frame(matrix(data=NA,nrow = 21,ncol = 8)) %>% setNames(c("cal","fat","carb","protein","sat_fat","sugar","fiber","sodium"))
rownames(table2) <- c("drive-thru","eat-in","takeout","ca","ma","or","mont","suffolk","king",
                      "taco","burrito","salad","other","dessert","bev","latenight","breakfast","lunch","afternoon","dinner","evening")
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
table2[1:3,] <- tmp[,2:9]
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
  dplyr::select(-c(2:17))
tmp <- tmp[c(1,3,5,4,6,2),]
table2[4:9,] <- tmp[,2:9]

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
  mutate(across(calorie:sodium, ~ ./count)) %>% dplyr::select(-count)
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
table2[10:15,] <- tmp[,2:9]

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
  mutate(across(calorie:sodium, ~ ./count)) %>% dplyr::select(-count)
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
table2[16:21,] <- tmp[,2:9]
table2$fiber <- gsub("-","",table2$fiber)
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

summary(trend$mean) #[-82,56]
ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=nutrient)) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=nutrient),size=1) +
  ggplot2::annotate(geom="label", x=6, y=-10, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-10),color="black",size=1) +
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



















