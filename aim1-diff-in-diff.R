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
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
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

matched$quarter <- ifelse(matched$month<=3, 1,
                          ifelse(matched$month>3&matched$month<=6, 2,
                                 ifelse(matched$month>6&matched$month<=9, 3,4)))
matched$quarter_year <- paste0(matched$year, "Q", matched$quarter)

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

### working off matched data only, summary stats, pick pre- data for all restaurants, regardless of actual time ----
table1::table1(data=matched %>% filter(relative==-3) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, 3 months before implementation </b>")
table1::table1(data=matched %>% filter(relative==-1) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, the month of implementation </b>")
table1::table1(data=matched %>% filter(relative==2) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, 3 months after implementation </b>")

# visualization, mean calorie, 12 month pre- and psot-
ggplot(data=matched %>% filter(relative2>=-12&relative2<=11) %>%
         mutate(group=ifelse(treat==1&match_place=="ca", 1, ifelse(treat==0&match_place=="ca", 2,ifelse(treat==1&match_place!="ca",3,4)))),
       aes(x=relative, y=calorie, group=as.character(group), color=as.character(group))) +
  stat_summary(aes(y=calorie,group=as.character(group),color=as.character(group)),#shape=as.character(treat),
               fun.y=mean, geom="line") + #insert monthly mean 
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  geom_vline(xintercept = -1, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=0, y=1170, label="Menu labeling \n implementation", size=3) + #add label for ML
  coord_cartesian(ylim=c(1000,1500), expand = FALSE, clip = "off") +
  scale_x_continuous(breaks=seq(-12,11,1)) +
  labs(title="Calories trend, 12 months before and after menu labeling", x="Month", y="Calories",
       caption="Note: California account for 77% of all treated restaurants.") +
  scale_color_manual(name="Menu Lableing", values=c("hotpink", "skyblue", "orange", "#009E73"),
                     labels=c("Treated, CA", "Comparison, CA", "Treated, other", "Comparison, other")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-12mon-before-after-single-out-ca.jpeg", dpi="retina")

### matched data, linear models ----
cov <- "concept+ownership+total+male+white+black+asian+hisp+
  median_income+capital_income+hsbelow+collegeup+under18+above65"

mod1.matched <- lm(formula = paste0("calorie~ml+treat+as.factor(monthno)+", cov),
                   data = matched, weights = weights)
summary(mod1.matched)
# month as relative and continuous
mod2.matched <- lm(formula = paste0("calorie~treat*relative+ml*relative+as.factor(monthno)+",cov),
                   data = matched, weights = weights)
summary(mod2.matched)
# diff-in-diff, with fixed effect at restaurant level
mod3.matched <- plm(formula = calorie~treat*relative+ml*relative+as.factor(monthno),
                    data = pdata.frame(matched, index = c("id")),
                    weights = weights, model = "within")
summary(mod3.matched)
# diff-in-diff, random effect at restaurant level
mod4.matched <- lme4::lmer(formula = paste0("calorie~treat*relative+ml*relative+as.factor(monthno)+", cov,"+(1|id)"),
                           data=matched, weights = weights)
summary(mod4.matched)
#city fixed effect
mod5.matched <- plm(formula = paste0("calorie~treat*relative+ml*relative+as.factor(monthno)+",cov),
                    data = pdata.frame(matched%>%mutate(location=paste(state,"+",county)), index = c("location")),
                    weights = weights, model = "within")
summary(mod5.matched)
#state fixed effect
mod6.matched <- plm(formula = paste0("calorie~treat*relative+ml*relative+as.factor(monthno)+",cov),
                    data = pdata.frame(matched, index = c("state")),
                    weights = weights, model = "within")
summary(mod6.matched)

length(unique(matched$id[matched$relative<=12&matched$relative>=-11])) #154
length(unique(matched$id)) #
length(unique(matched$id[matched$open_month==106])) #

stargazer(mod1.matched, mod2.matched, mod6.matched,mod5.matched,mod3.matched, mod4.matched,
          type="html",
          title="The effect of menu labeling on calories purchased, 24 months pre- and post-labeling",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("treat","ml", "relative", "ml:relative"),
          order=c("^treat$","^treat:relative$","^ml$", "^relative$", "^ml:relative$"),
          covariate.labels=c("In labeling group","In labeling group*month","Has labeling", 
                             "Month (relative)", "Has labeling*month (relative)"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Model","<em>diff-in-diff</em>","<em>Month continuous</em>",
                             "<em>State FE</em>","<em>City FE</em>",
                             "<em>Restaurant FE</em>","<em>Restaurant RE</em>"),
                           c("Unique restaurants", "951", "951","951","951","951","951","951")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Has labeling = 1 for treated",
                    "restaruants when labeling is implemented. All models has calendar month as control vars."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/cal=treat+ml+month-linear-24mon.html")

### matched data, strtify by location ----
# stratify by diff treatment city
model_list <- list()
for (i in c("king","nassau","philly","albany","mont","mult","ma","ca","or","nj")) {
  tryCatch({ #catch groups that do not have comparison restaurants
    model <- lm(data = matched %>% filter(match_place==i),
                formula = paste0("calorie~treat+ml*relative+as.factor(monthno)+",cov),
                weights = weights)
    #print(length(unique(matched$id[matched$match_place==i])))
    model_list[[i]] <- model
  }, error=function(e){cat(paste0("ERROR: ", i),conditionMessage(e), "\n")})
}
rm(model, i)
stargazer(mod2.matched, model_list[["king"]],model_list[["nassau"]],model_list[["philly"]],
          model_list[["albany"]],model_list[["mont"]],model_list[["mult"]],
          model_list[["ma"]],model_list[["ca"]],model_list[["or"]],model_list[["nj"]],
          type="html",
          title="The effect of menu labeling on calories purchased, stratified by city/state",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("treat","ml", "relative", "ml:relative"),
          order=c("treat","^ml$", "^relative$", "^ml:relative$"),
          covariate.labels=c("In labeling group","Has labeling", 
                             "Month (relative)", "Has labeling*month (relative)"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("City/state","Overall", "King county", "Nassau", "Philadelphia",
                             "Albany","Montgomery", "Multnomah",
                             "MA", "CA","OR", "NJ"),
                           c("Unique restaurants","154","6","16","10","6","6","43","80","710","91","41")),
          notes = "All models has calendar month as control vars.",
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/aim1-strat-by-city.html")

# restaurants continuously open for 12 months before and after ML
tmp <- matched %>% filter(relative>=-11&relative<=12) %>%
  group_by(id, treat, match_to) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank)) %>%
  filter(open_month==24)
hist(tmp$open_month)
summary(tmp$open_month)
length(unique(tmp$id[tmp$treat==1&tmp$open_month==24])) #443
length(unique(tmp$id[tmp$treat==0&tmp$open_month==24])) #364

### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
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
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE-24mon.jpeg", dpi="retina")

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
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2<=-3|(relative2>=2&relative2<=55))), #&match_place=="ca"
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
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Others"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), legend.position = "none",
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mean-diff-in-diff-by-location.jpeg", dpi="retina")

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
       caption="Impact is mean of each 6-month period sum. \ncalorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Others"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mean-diff-6mon.jpeg", dpi="retina")

### try polynomial and/or log term for month ---- 
#y=x^2+x, y=x^3+x^2+x, y=log(x)

### map where treated and comparison restaurants are ----
names(matched)

#merge with geo coordinates
coords <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
names(coords)
coords <- coords[!duplicated(coords$address), c(2,8:9)]
tmp <- matched[!duplicated(paste0(matched$id,matched$match_place)),c(2:4,6:9,11,27:29)]
coords <- merge(tmp,coords,by="address", all.x = TRUE)
rm(tmp)

#make map for CA
hist(coords$weights[coords$treat==0],
     breaks = 500,main = "Histogram of weights assigned to restaurants",
     xlab = "Weights",ylim = c(0,50))

ggplot(data=coords%>%filter(treat==0&match_place!="ca")) + coord_fixed() +
  geom_polygon(data=map_data("state"),aes(x=long, y=lat, group=group), color="grey", fill="skyblue", size=0.1, alpha=0.5) +
  geom_point(aes(x=lon, y=lat,color=match_place,size=weights),shape=1) +
  geom_text(data=data.frame(state.center, state.abb), aes(x,y, label = state.abb, size=3),color="grey",show_guide=FALSE) + #add state abbr to map
  labs(title="Locations of comparison restaruants, others",x="",y="",
       caption="3 restaruants in Delaware, Illinois and Goergia were omitted in the matching process.") +
  scale_color_manual(name="Matched to",
                     labels=c("King county, WA","MA","Montgomery county, MD","Multnomah county, OR",
                              "Nassau county, NY","NJ","OR","Suffolk county, NY"),
                     values =c("navy","orange","violet","grey","green","purple",
                               "red","aquamarine3")) +
  scale_size_continuous(name = "Weights",breaks = seq(0,5,1)) +
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"),
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/results/map-comparison-restaurants_notCA.jpeg", dpi="retina")

#make map for other locations
ggplot(data=coords%>%filter(treat==0&match_place=="ca")) + coord_fixed() +
  geom_polygon(data=map_data("state"),aes(x=long, y=lat, group=group),
               color="grey", fill="skyblue", size=0.1, alpha=0.5) +
  geom_text(data=data.frame(state.center, state.abb), aes(x,y, label = state.abb, size=3),color="grey",show_guide=FALSE) + #add state abbr to map
  geom_point(aes(x=lon, y=lat, size=weights), shape=1,color="orange") +
  labs(title="Locations of comparison restaruants, CA",x="",y="",
       caption="3 restaruants in Delaware, Illinois and Goergia were omitted in the matching process.") +
  scale_size_continuous(name = "Weights",breaks = seq(1,10,2)) +
  scale_color_discrete(guide=FALSE) + #turn off legend for match_to
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"),
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/analytic-model/matching/results/map-comparison-restaurants_CA.jpeg", dpi="retina")
rm(coords)
### fill in missing transaction data, sensitivity check ----
# fill in all the temp closings within 24 months after labeling
monthno <- data.frame(c(204:309)) %>% setNames("monthno")
#check the 36 months immediately after labeling
# filter out the ones that didn't have 24 months consecutively after ML
# omit restaurants with gaps larger than 4 months, only fill in gaps between 1 and 3 months
tmp <- matched %>%
  group_by(id,treat,match_place) %>%
  filter(monthno>=entry&relative2<=29) %>%
  dplyr::select(id,treat,match_place,address,entry,monthno,relative2,open_after,calorie) %>%
  arrange(relative2, .by_group=TRUE) %>%
  mutate(n=n()) %>%
  mutate(gap = ifelse(relative2==0,1,relative2 - dplyr::lag(relative2,1))) %>%
  mutate(max_gap = max(gap)) %>%
  filter(gap>=2 & gap<=4) %>%
  dplyr::select(id,treat,match_place) %>% distinct() 
monthno <- merge(tmp,monthno,all=TRUE)

# num of restaurants that has gaps between 2 to 4 months within the 30 months after labeling
length(unique(paste0(tmp$id,tmp$match_place))) #80 restaurants
#isolate those restaurants and add calorie info
tmp <- merge(tmp,matched,by=c("id","treat","match_place"))
tmp <- tmp %>%
  group_by(id,treat,match_place) %>%
  filter(monthno>=entry&relative2<=35) %>%
  arrange(relative2, .by_group=TRUE) %>%
  mutate(gap = ifelse(relative2==0,1,relative2 - dplyr::lag(relative2,1))) 
tmp <- merge(tmp,monthno,all=TRUE)
tmp <- tmp %>%
  group_by(id,treat,match_place) %>%
  mutate(entry = entry[!is.na(entry)][1L]) %>% #fill in the missing entry dates
  mutate(treat = treat[!is.na(treat)][1L]) %>% 
  mutate(weights = weights[!is.na(weights)][1L]) %>% 
  mutate(relative2 = ifelse(is.na(relative2), monthno-entry, relative2)) %>%
  filter(monthno>=entry&relative2<=35) %>%
  arrange(relative2, .by_group=TRUE) %>%
  mutate(max = max(monthno[!is.na(calorie)])) %>%
  filter(!(is.na(calorie)&monthno>max)) %>% #omit open ended consecutive missing months
  dplyr::select(-max) %>%
  mutate(gap2 = zoo::na.locf(gap,na.rm = FALSE, fromLast = TRUE)) %>%
  filter(gap2<=6) %>%
  filter((is.na(dplyr::lead(gap,1))&!is.na(gap))|gap2>1) %>%
  mutate(cal_after = zoo::na.locf(calorie,na.rm = FALSE, fromLast = TRUE)) %>%
  mutate(cal_before = zoo::na.locf(calorie,na.rm = FALSE))%>%
  filter(is.na(calorie)) %>%
  group_by(id,treat,match_place,gap2) %>%
  mutate(ratio = row_number()) %>%
  mutate(calorie_imputed = cal_before*(ratio/gap2)+cal_after*((gap2-ratio)/gap2)) %>%
  ungroup() %>%
  dplyr::select(-c(ratio,gap,gap2,cal_before,cal_after,year,month))

#fill in the proper seasonal year and month
time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
time <- time[, c(7,38)] %>% setNames(c("monthno","time")) %>%
  mutate(year = as.integer(substr(time,1,4))) %>%
  mutate(month = as.integer(substr(time,6,7))) %>%
  filter(monthno>=204) %>%
  arrange(monthno) %>%
  dplyr::select(-time) %>% distinct()
tmp <- merge(tmp,time,by="monthno",all.x=TRUE) %>%
  relocate(year,month)

### assemble matched data with imputed calorie ----
imputed <- matched
imputed$calorie_imputed <- imputed$calorie
imputed <- rbind(imputed,tmp)
imputed <- imputed[order(imputed$id,imputed$match_place,imputed$monthno),]
tmp <- imputed %>% dplyr::select(id,treat,match_place,calorie,calorie_imputed,relative2.factor,month,weights)
tmp <- tmp[complete.cases(tmp),]
#rm(monthno,tmp,time)

imputed$relative2.factor <- factor(imputed$relative2)
imputed <- within(imputed, relative2.factor<-relevel(relative2.factor, ref="-3"))
mod.factor <- plm(formula = calorie_imputed~treat*relative2.factor+as.factor(month),
                  data = imputed%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
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
  labs(title="Effect of menu labeling on calories purchased, imputed calorie", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \ncalorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant \nCalories are imputed for less than 4 months of consecutive missing.") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE-imputed.jpeg", dpi="retina")

### analysis by restaurant open time after labeling ----
#restaurants open for at least 12, 15, 18, 21, 24 months after labeling
tidy_mod.factor_all <- NULL
for (i in c(12,15,18,21,24,27,30)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter((relative2<=-3|(relative2>=2&relative2<=55))&open_after>=i), 
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
  tidy_mod.factor <- tidy_mod.factor[1:101,c(1,5,7:8)]
  tidy_mod.factor$time <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
}

summary(tidy_mod.factor_all$diff) #[-60,19]
ggplot(data=tidy_mod.factor_all%>%filter(month<=-3|month>=3),aes(x=month, y=diff,group=as.character(time), color=as.character(time))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-75, ymax=25, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-75,25),breaks=seq(-75,25,5)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, by open time after ML", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant") + 
  scale_color_manual(name="Open after labeling", labels=c("12 months","15 months","18 months","21 months","24 months","27 months","30 months"),
                       values=c("hotpink","olivedrab3","red","orange","grey","purple","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE-by-open-time.jpeg", dpi="retina")

### by-location analysis, separate each ML location ----
trend <- NULL
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
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
                    data = matched%>%filter(((relative2<=-3&relative2>=-47)|(relative2>=2&relative2<=55))&match_place==i), 
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
       caption="calorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant \nKing county, Montgomery county and the hypothetical state of Jefferson do not have separate analyses for each having N=3.") + 
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
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE-bylocation-splitCA.jpeg", dpi="retina")

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
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant") + 
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
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mean-diff-in-diff-by-location-splitCA.jpeg", dpi="retina")

# number of restaurants open for each month post labeling
time <- data.frame(c(1:30),c(rep(0,90))) %>% setNames(c("month","num"))
for (i in c(0:29)) {
  time[i+1,2] <- length(unique(paste0(matched$id[matched$relative2==i],matched$match_place[matched$relative2==i])))
  time[i+31,2] <- length(unique(matched$id[matched$relative2==i&matched$treat==1]))
  time[i+61,2] <- length(unique(paste0(matched$id[matched$relative2==i&matched$treat==0],matched$match_place[matched$relative2==i&matched$treat==0])))
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
  ggplot2::annotate(geom="label", x=13, y=1.07, label="N comparison \nrestaurants decreased \nby 57, 50 of which \nare matched to \n CA restaurants.", size=3) + 
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
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/num-restaurants-over-time.jpeg", dpi="retina")

### investigate data anomolies during month 12-14, in comp restaurants ----
#identify comp restaurants that dropped off in month 13
# plot mean calorie over time by location
tmp <- matched %>% filter(treat==0 & (relative2==11|relative2==12)) %>%
  dplyr::select(id,match_place,address,entry,weights,relative2) %>%
  arrange(desc(weights),relative2) %>% group_by(id,match_place) %>%
  mutate(n=n()) %>% filter(n==1) %>% dplyr::select(id,match_place) %>%
  anti_join(x=matched, by=c("id","match_place"))

#understand calorie change between month 12-14
tmp <- matched %>%
  group_by(id,match_place) %>%
  mutate(calorie_last = dplyr::lag(calorie,2)) %>%
  mutate(pct = (calorie-calorie_last)/calorie_last) %>%
  filter(relative==14) %>% ungroup() %>%
  arrange(desc(pct)) %>% dplyr::select(address,match_place,calorie,calorie_last,pct,weights,treat)
#make histogram, by treatment status

# exclude high impact restaurants
tmp <- matched %>%
  group_by(id,match_place) %>%
  mutate(calorie_last = dplyr::lag(calorie,2)) %>%
  mutate(pct = (calorie-calorie_last)/calorie_last) %>%
  filter(relative==14 & treat==0 &!is.na(pct) & weights>=0.19825 & pct>=0.029895) %>% ungroup() 
# run the same model and make figure from here

### alternative outcomes: # of transactions, mean spending per order ----
# total # of transactions as outcome
mod.factor <- plm(formula = count~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
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
  mutate(count=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))

summary(tidy_mod.factor$count) #[-4374,1111]
summary(tidy_mod.factor$diff) #[-558,2002]
ggplot(data=tidy_mod.factor%>%filter(month<=-3|month>=3),aes(x=month,y=count,color=as.character(group))) + #
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&(month<=-3|month>=3)),aes(x=month, y=diff*1-8000), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)&(month<=-3|month>=3)),aes(x=month, y=diff*1-8000), color="orange",size=1) + 
  geom_hline(yintercept = -8000, color="grey", linetype="dashed") +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-9000, ymax=2000, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-5000, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-9000,2000),breaks=seq(-9000,2000,1000),
                     sec.axis = sec_axis(~(.+8000)/1, name="Difference")) + 
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on number of transactions per store", x="Month", y="Number of transactions", 
       caption="Orange lined represents the difference between treat and comparison group. \nnum_transactions = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/count=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

# total # of transactions as outcome
mod.factor <- plm(formula = spending~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))%>%mutate(spending=count*dollar), 
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
  mutate(dollar=ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
  filter(month<=-3|month>=3) %>%
  mutate(dollar_k=dollar/1000) %>%
  mutate(diff_k = diff/1000)

summary(tidy_mod.factor$dollar_k) #[-25,6]
summary(tidy_mod.factor$diff_k) #[-4,12.5]
ggplot(data=tidy_mod.factor,aes(x=month,y=dollar_k,color=as.character(group))) + #
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff_k*1-40), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff_k*1-40), color="orange",size=1) + 
  geom_hline(yintercept = -40, color="grey", linetype="dashed") +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-50, ymax=10, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-32, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,10),breaks=seq(-50,10,10),
                     sec.axis = sec_axis(~(.+40)/1, name="Difference")) + 
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on revenue per store", x="Month", y="Revenue (thousand $)", 
       caption="Orange lined represents the difference between treat and comparison group. \nrevenue = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/revenue=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")
 

