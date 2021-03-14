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
length(unique(matched$id[matched$open_month==105&matched$treat==1])) #67
length(unique(matched$id[matched$open_month==105&matched$treat==0])) #93

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
  filter(relative2<=23&relative2>=-24) %>%
  mutate(n=n()) %>%
  mutate(open24 = ifelse(n==48, 1,0)) %>%
  filter(relative2<=17&relative2>=-18) %>%
  mutate(n=n()) %>%
  mutate(open18 = ifelse(n==36, 1,0)) %>%
  filter(relative2<=11&relative2>=-12) %>%
  mutate(n=n()) %>%
  mutate(open12 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=5&relative2>=-6) %>%
  mutate(n=n()) %>%
  mutate(open6 = ifelse(n==12, 1,0)) %>%
  dplyr::select(id, treat, match_place, open6,open12,open18,open24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
rm(tmp)

### working off matched data only, summary stats, pick pre- data for all restaurants, regardless of actual time ----
table1::table1(data=matched %>% filter(relative==-3) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, 3 months before implementation </b>")
table1::table1(data=matched %>% filter(relative==1) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, the month of implementation </b>")
table1::table1(data=matched %>% filter(relative==3) %>% mutate(nonwhite=black+asian+hisp) %>% mutate(rev=count*dollar),
               ~calorie+count+rev+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
               caption="<b>Summary statistics, 3 months after implementation </b>")

# visualization, mean calorie, 12 month pre- and psot-
ggplot(data=matched %>% filter(relative>=-11&relative<=12) %>%
         mutate(group=ifelse(treat==1&match_place=="ca", 1, ifelse(treat==0&match_place=="ca", 2,ifelse(treat==1&match_place!="ca",3,4)))),
       aes(x=relative, y=calorie, group=as.character(group), color=as.character(group))) +
  stat_summary(aes(y=calorie,group=as.character(group),color=as.character(group)),
               fun.y=mean, geom="line") + #insert monthly mean 
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=0, y=1170, label="Just before ML", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=0, y=1420, label="Dec", size=3) + #add label for time
  ggplot2::annotate(geom="label", x=1, y=1420, label="Jan", size=3) + #add label for time
  coord_cartesian(ylim=c(1000,1500), expand = FALSE, clip = "off") +
  scale_x_continuous(breaks=seq(-11,12,1)) +
  labs(title="Calories trend, 12 months before and after menu labeling", x="Month", y="Calories",
       caption="Note: California account for 77% of all treated restaurants. Grey dashed line represents the month immediately before labeling began.") +
  scale_color_manual(name="Menu Lableing", values=c("hotpink", "skyblue", "orange", "#009E73"),
                     labels=c("CA", "Comparison restaurants for CA", "Other treated restaurants", "other comparison restaurants")) +
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
length <- data.frame(c(-48:60))
colnames(length)[1] <- "month"
length$n <- NA
for (i in (-48:60)) {
  length$n[i+49] <- length(unique(paste0(matched$id[matched$relative2==i], matched$match_place[matched$relative2==i])))
}
rm(i)

mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter(relative2<=60), 
                  index = c("id"), weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor)

# clean data
tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
#tidy_mod.factor <- tidy_mod.factor[-c(119:128), ]
tidy_mod.factor$coef.treat <- 0
tidy_mod.factor$group <- 0
dim(tidy_mod.factor)
tidy_mod.factor[217:218, 1] <- "0" #add 2 rows for month 0
tidy_mod.factor[217:218, c(2,4)] <- 0 #add coef.month and coef.treat
tidy_mod.factor[217:218, 5] <- c(0, 1) #add treat=0 and treat=1
tidy_mod.factor[, 1] <- c(seq(-48, -1, 1),seq(1,60, 1),seq(-48, -1, 1),seq(1,60, 1),0,0) #change month numbers
tidy_mod.factor$group[109:216] <- 1 #change group to treat=1
tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat[110:218] <- tidy_mod.factor$coef.month[110:218]
tidy_mod.factor$coef.month[110:218] <- tidy_mod.factor$coef.month[1:109]
tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                  tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
tidy_mod.factor$diff <- NA
tidy_mod.factor$diff[1:109] <- tidy_mod.factor$calorie[110:218] - tidy_mod.factor$calorie[1:109]
tidy_mod.factor$p.diff <- NA
tidy_mod.factor$p.diff[1:109] <- tidy_mod.factor$p[110:218]
tidy_mod.factor <- merge(tidy_mod.factor, length, by="month")

# add year and month factor as covariate
#tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor).csv")
summary(tidy_mod.factor$calorie) #[-81,75]
summary(tidy_mod.factor$diff) #[-62,22]
ggplot() + #
  #geom_bar(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=n), stat="identity",color="pink", fill="white", alpha=0, size=0.5) + #num of obs in each month
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -275, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = -1, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_bar(data=tidy_mod.factor%>%filter(!is.na(diff)&(month==-1|month==0)),aes(x=month, y=n), stat="identity",color="pink", fill="pink", alpha=0.5, size=0.5) + #num of obs in each month
  geom_point(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group)), size=1) +
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.05),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  #geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.01),aes(x=month, y=diff*1-300), color="#ff0000") + 
  #geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.001),aes(x=month, y=diff*1-300), color="#660066") + 
  ggplot2::annotate(geom="label", x=0, y=-100, label="Menu labeling \n implementation", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-17.5,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(-400,100),breaks=seq(-400,100,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=seq(-45,60,5)) +
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

### cal=group*month(factor), month as factor, detect trend using diff-in-diff ----
trend <- NULL
tmp <- data.frame(1:30)
colnames(tmp)[1] <- "month"
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
              data = matched%>%filter(relative2>=-33&relative2<=56), 
              index = c("id"), weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor)
#write.csv(trend, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")

# clean data
tidy_mod.factor <- tidy_mod.factor[, c(1:2,5)]
colnames(tidy_mod.factor) <- c("month", "coef.month", "p")
tidy_mod.factor <- tidy_mod.factor[!grepl("as.factor", tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat <- 0
tidy_mod.factor$group <- 0
dim(tidy_mod.factor)
tidy_mod.factor[179:180, 1] <- "0"
tidy_mod.factor[179:180, c(2,4)] <- 0
tidy_mod.factor[179:180, 5] <- c(0, 1)
tidy_mod.factor[, 1] <- c(seq(-33, -1, 1),seq(1,56, 1),seq(-33, -1, 1),seq(1,56, 1),0,0)
tidy_mod.factor$group[90:178] <- 1
tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat[91:180] <- tidy_mod.factor$coef.month[91:180]
tidy_mod.factor$coef.month[91:180] <- tidy_mod.factor$coef.month[1:90]
tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                  tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
tidy_mod.factor$diff <- NA
tidy_mod.factor$diff[1:90] <- tidy_mod.factor$calorie[91:180] - tidy_mod.factor$calorie[1:90]
tidy_mod.factor$p.diff <- NA
tidy_mod.factor$p.diff[1:90] <- tidy_mod.factor$p[91:180]
#for pre-period, use only up to 6 months of data
tmp1 <- tidy_mod.factor[tidy_mod.factor$month>=-30&tidy_mod.factor$month<0&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp1 <- tmp1[order(tmp1$month, decreasing = TRUE), ]
tmp1$month <- -tmp1$month
tmp1$pre_mean <- sum(tmp1$diff[tmp1$month<=6])/6
tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=0&tidy_mod.factor$month<=29&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp2 <- tmp2[order(tmp2$month), ]
tmp2$month <- tmp2$month+1
tmp2$post_mean <- cumsum(tmp2$diff)/tmp2$month
tmp <- merge(tmp1, tmp2, by="month", all=TRUE)
tmp$mean <- tmp$post_mean - tmp$pre_mean
tmp$loc <- "other_trim"
tmp <- tmp[,-c(2,4)]
trend <- rbind(trend, tmp)
rm(tmp1,tmp2,tmp)

names(trend)
table(trend$loc)
location <- factor(trend$loc,levels = c("all", "ca", "other", "all_trim", 
                                        "ca_trim", "other_trim"))
Open <- factor(trend$open,levels = c("any","6mon","12mon","18mon","24mon","all"),
               labels = c("Any (N=1,009)","At least 6 months pre-/post- (N=953)",
                          "At least 12 months pre-/post- (N=852)",
                          "At least 18 months pre-/post- (N=448)",
                          "At least 24 months pre-/post- (N=361)", "All 106 months (N=160)"))

#trend <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")
ggplot(data=trend, aes(x=month, y=mean, group=location, color=location)) + #
  geom_line() +
  geom_point() +
  #ggplot2::annotate(geom="label", x=11, y=10, label="All groups follow similar \n trends. Restaurants open for \n longer periods of time \n are different in magnitude", size=3) + 
  #ggplot2::annotate(geom="label", x=11, y=-7, label="The overall trend also \n explains the inconsistent \n coefficients in linear models", size=3) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-30,50),breaks=seq(-30,50,10)) +
  scale_x_continuous(breaks=seq(1,30,1)) +
  labs(title="Mean diff-in-diff per month, by location", x="Month", y="Calories", 
       caption="Pre-period data is capped at 6 months. \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_manual(name="Location",
                     labels=c("All", "CA", "Excl. CA", "All, trimmed", "CA, trimmed", "Excl. CA, trimemd"),
                     values = c("hotpink","olivedrab3","#13B0E4","purple","#ffd400","grey")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location_trim.jpeg", dpi="retina")

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
     breaks = 100,main = "Histogram of weights assigned to restaurants",
     xlab = "Weights",xlim = c(0,10),ylim = c(0,75))

ggplot(data=coords%>%filter(treat==0)) + coord_fixed() +
  geom_polygon(data=map_data("state"),aes(x=long, y=lat, group=group),
               color="grey", fill="lightblue", size=0.1, alpha=0.5) +
  geom_point(aes(x=lon, y=lat,color=match_place,size=weights),shape=1) +
  labs(title="Locations of comparison restaruants",x="",y="",
       caption="3 restaruants in Delaware, Illinois and Goergia were omitted in the matching process.") +
  #scale_color_gradient2(midpoint=1,low="orange", mid="red", high="purple") +
  scale_color_manual(name="Matched to",
                     labels=c("CA","King county, WA","MA","Montgomery county, MD","Multnomah county, OR",
                              "Nassau county, NY","NJ","OR","Suffolk county, NY"),
                     values =c("orange", "aquamarine3","violet","grey","green","hotpink",
                               "red","black","skyblue")) +
  scale_size_continuous(name = "Weights",breaks = seq(1,10,2)) +
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"),
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/diff-pricing/burrito-price_2015q1.jpeg", dpi="retina)

#make map for other locations
ggplot() +
  coord_fixed() +
  geom_polygon(data=map_data("state"),aes(x=long, y=lat, group=group),
               color="grey", fill="lightblue", size=0.1, alpha=0.5) +
  geom_point(data=coords%>%filter(treat==0&match_place!="ca"), 
             aes(x=lon, y=lat, color=match_place), size=2, shape=1) +
  labs(title="Locations of comparison restaruants",x="",y="",
       caption="3 restaruants in Delaware, Illinois and Goergia were omitted in the matching process.") +
  scale_color_manual(name="Matched to",
                     labels=c("CA","King county, WA","MA","Montgomery county, MD","Multnomah county, OR",
                              "Nassau county, NY","NJ","OR","Suffolk county, NY"),
                     values =c("orange", "aquamarine3","violet","grey","green","hotpink",
                               "red","black","skyblue")) +
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"),
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.caption=element_text(hjust=0, face="italic"))
#ggsave("tables/diff-pricing/burrito-price_2015q1.jpeg", dpi="retina)
