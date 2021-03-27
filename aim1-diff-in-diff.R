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
length <- data.frame(c(-48:60))
colnames(length)[1] <- "month"
length$n <- NA
for (i in (-48:60)) {
  length$n[i+49] <- length(unique(paste0(matched$id[matched$relative2==i], matched$match_place[matched$relative2==i])))
}
rm(i)

mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter(relative2<=-3|(relative2>=2&relative2<=55)), 
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
#tidy_mod.factor <- tidy_mod.factor[-c(115:129),]
tidy_mod.factor[201:202, 1] <- "0" #add 2 rows for month 0
tidy_mod.factor[201:202, c(2,4)] <- 0 #add coef.month and coef.treat
tidy_mod.factor[201:202, 5] <- c(0, 1) #add treat=0 and treat=1
tidy_mod.factor[, 1] <- c(seq(-48,-3,1),seq(2,55,1),seq(-48,-3, 1),seq(2,55, 1),-49,-49) #change month numbers
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
#tidy_mod.factor <- merge(tidy_mod.factor, length, by="month")

# add year and month factor as covariate
#tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor).csv")
summary(tidy_mod.factor$calorie) #[-81,75]
summary(tidy_mod.factor$diff) #[-62,22]
ggplot(data=tidy_mod.factor%>%filter(month<=-3|month>=3),aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) + #
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -275, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = -1, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)&(month<=-3|month>=3)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.05&(month<=-3|month>=3)),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-17.5,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,100),breaks=seq(-400,100,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=seq(-45,65,3)) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/cal=treat+month_monthFE_restaurantFE_CA.jpeg", dpi="retina")

### cal=group*month(factor), month as factor, detect trend using diff-in-diff ----
trend <- NULL
tmp <- data.frame(1:30)
colnames(tmp)[1] <- "month"
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
              data = matched%>%filter(relative2<=55&match_place=="ca"), 
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
tidy_mod.factor[207:208, 1] <- "0" #add 2 rows for month 0
tidy_mod.factor[207:208, c(2,4)] <- 0 #add coef.month and coef.treat
tidy_mod.factor[207:208, 5] <- c(0, 1) #add treat=0 and treat=1
tidy_mod.factor[, 1] <- c(seq(-48, -1, 1),seq(1,55, 1),seq(-48, -1, 1),seq(1,55, 1),0,0) #change month numbers
tidy_mod.factor$group[104:206] <- 1 #change group to treat=1
tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month), ]
tidy_mod.factor$coef.treat[105:208] <- tidy_mod.factor$coef.month[105:208]
tidy_mod.factor$coef.month[105:208] <- tidy_mod.factor$coef.month[1:104]
tidy_mod.factor$calorie <- ifelse(tidy_mod.factor$group==0, tidy_mod.factor$coef.month,
                                  tidy_mod.factor$coef.month+tidy_mod.factor$coef.treat)
tidy_mod.factor$diff <- NA
tidy_mod.factor$diff[1:104] <- tidy_mod.factor$calorie[105:208] - tidy_mod.factor$calorie[1:104]
tidy_mod.factor$p.diff <- NA
tidy_mod.factor$p.diff[1:104] <- tidy_mod.factor$p[105:208]
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
tmp$loc <- "ca"
tmp <- tmp[,-c(2,4)]
trend <- rbind(trend, tmp)
rm(tmp1,tmp2,tmp)

names(trend)
table(trend$loc)
location <- factor(trend$loc,levels = c("all", "ca", "other"))
Open <- factor(trend$open,levels = c("any","6mon","12mon","18mon","24mon","all"),
               labels = c("Any (N=1,009)","At least 6 months pre-/post- (N=953)",
                          "At least 12 months pre-/post- (N=852)",
                          "At least 18 months pre-/post- (N=448)",
                          "At least 24 months pre-/post- (N=361)", "All 106 months (N=160)"))

#trend <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")
ggplot(data=trend, aes(x=month, y=mean, group=location, color=location)) + #
  geom_line() + geom_point() +
  #ggplot2::annotate(geom="label", x=11, y=10, label="All groups follow similar \n trends. Restaurants open for \n longer periods of time \n are different in magnitude", size=3) + 
  #ggplot2::annotate(geom="label", x=11, y=-7, label="The overall trend also \n explains the inconsistent \n coefficients in linear models", size=3) + 
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,10),breaks=seq(-50,10,10)) +
  scale_x_continuous(breaks=seq(1,30,1)) +
  labs(title="Mean diff-in-diff per month, by location", x="Month", y="Calories", 
       caption="Pre-period data is capped at 6 months. \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_manual(name="Location", labels=c("All", "CA", "Excl. CA"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mean-diff-in-diff-by-location.jpeg", dpi="retina")

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