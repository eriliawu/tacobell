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
### import data, unmatched ----
unmatched <- read.csv("data/calorie-aims/unmatched-restaurants.csv",
                      stringsAsFactors = FALSE,
                      colClasses = c(rep(NA, 38), rep("NULL", 6)))

### set up covariates and transform montho ----
table(unmatched$ownership)
names(unmatched)

# covariates:
# purchase: month, holiday
# restaurant: characteristics of the restaurant itself and the community
# ML: whether a restaurant is in the labeling group
# month: relative month indicators representing months pre- and post-
# plus restaurant level random effects
unmatched$holiday <- ifelse(unmatched$month==12, 1, 0)

# craete a new restid for each address+tract_num+ownership+concept combo
unmatched <- unmatched %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday)

#recode ownership and concept to be the same as matching
unmatched$concept <- ifelse(unmatched$concept=="TBC", 0, 1)
unmatched$ownership <- ifelse(unmatched$ownership=="COMPANY", 1, 0)
unmatched <- unmatched[complete.cases(unmatched), ]

### implement model design, unmatched data ----
# simple diff-in-diff, pre-/post-
mod1.unmatched <- lm(formula = calorie~ml+
             as.factor(year)+as.factor(month)+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
             median_income+capital_income+hsbelow+collegeup+under18+above65,
           data = unmatched)
summary(mod1.unmatched)

# diff in diff, month as continunous var
mod2.unmatched <- lm(formula = calorie~ml+ml*monthno+monthno+
                       holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                       median_income+capital_income+hsbelow+collegeup+under18+above65,
                     data = unmatched)
summary(mod2.unmatched)

# diff-in-diff, with fixed effect at restaurant level
mod3.unmatched <- plm(formula = calorie~ml+ml*monthno+monthno+holiday,
            data = pdata.frame(unmatched, index = c("id")),
            model = "within")
summary(mod3.unmatched)

# diff-in-diff, random effect at restaurant level
mod4.unmatched <- lme4::lmer(formula = calorie~ml+ml*monthno+monthno+
                         holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                         median_income+capital_income+hsbelow+collegeup+under18+above65+(1|id),
                   data=unmatched)
summary(mod4.unmatched)

### unmatched data, expand comparison units, to match each treated unit ----
tmp <- unmatched[unmatched$treat==1, ]
tmp$match_to <- tmp$entry

master <- NULL
for (i in c(221,229,242,241,226,233,247,253,251,254,238,239,270)) {
  tmp1 <- unmatched[unmatched$treat==0, ]
  tmp1$match_to <- i
  master <- rbind(master, tmp1)
}
unmatched.comp <- rbind(tmp, master)
rm(tmp, tmp1, master, i)
unmatched.comp$relative <- unmatched.comp$monthno - unmatched.comp$match_to +1

# simple diff-in-diff
mod1.unmatched.comp <- lm(formula = calorie~ml+
                            as.factor(year)+as.factor(month)+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                            median_income+capital_income+hsbelow+collegeup+under18+above65,
                          data = unmatched.comp)
summary(mod1.unmatched.comp)

# month as relative and continuous var
mod2.unmatched.comp <- lm(formula = calorie~ml+ml*relative+as.factor(year)+
                            as.factor(month)+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                     median_income+capital_income+hsbelow+collegeup+under18+above65,
                   data = unmatched.comp)
summary(mod2.unmatched.comp)

# diff-in-diff, with fixed effect at restaurant level
mod3.unmatched.comp <- plm(formula = calorie~ml+ml*relative+relative+as.factor(year)+as.factor(month)+holiday,
                    data = pdata.frame(unmatched.comp, index = c("id")),
                    model = "within")
summary(mod3.unmatched.comp)

# diff-in-diff, random effect at restaurant level
mod4.unmatched.comp <- lme4::lmer(formula = calorie~ml+ml*relative+relative+
                                    as.factor(year)+as.factor(month)+holiday+concept+drive_thru+ownership+
                                    total+male+white+black+asian+hisp+median_income+
                                    capital_income+hsbelow+collegeup+under18+above65+(1|id),
                             data=unmatched.comp)
summary(mod4.unmatched.comp)

### export results to regression table ----
stargazer(mod1.unmatched, mod2.unmatched, mod3.unmatched, mod4.unmatched,
          mod1.unmatched.comp, mod2.unmatched.comp, mod3.unmatched.comp, mod4.unmatched.comp,
          mod1.matched, mod2.matched, mod3.matched, mod4.matched,
          type="html",
          title="The effect of menu labeling on calories purchased",
          #dep.var.labels="Mean calories per order",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          column.labels = c("<b> Unmatched, continuous month </b>",
                            "<b> Unmatched, relative month</b>", "<b> Matched </b>"), #bold
          column.separate = c(4,4,4),
          model.names = FALSE,
          keep = c("ml", "monthno", "ml:monthno", "relative", "ml:relative"),
          order=c("^ml$", "^monthno$", "^ml:monthno$", "^relative$", "^ml:relative$"),
          covariate.labels=c("Has labeling", "Month (continuous)", "Has labeling*month (continuous)", 
                             "Month (relative)", "Has labeling*month (relative)"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Model", "<em>Post dummy</em>", "<em>Month cont</em>", "<em>FE</em>", "<em>RE</em>",
                             "<em>Post dummy</em>", "<em>Month relative</em>", "<em>FE</em>", "<em>RE</em>",
                             "<em>Post dummy</em>", "<em>Month relative</em>", "<em>FE</em>", "<em>RE</em>")),
          notes = c("Has labeling = 1 when a treated restaurant has labeling in place; month is presented as a continuous variable in columns 1-4. In all models, we control for purchase ",
                    "characteristics (holiday season); restaurant characteristics: owned by Taco Bell, has drive through, has other brands on-site; and community characteristics at census ",
                    "tract level: population density, % male, % Black, % Asian, % Hispanic, % White, household median income, income per capita, % without high school degree, % with bachelor ",
                    "degree or higher, % under 18, % above 65. Additionally, for columns 5-12, we control for calendar year and month."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/aim1.html")

### visualize, calories per order, by ML ----
# by subgroups, estimate ML=0/1 groups separately
ggplot(data=unmatched, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=paste0(treat,ml), color=paste0(treat,ml))) +
  geom_point(size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=paste0(treat,ml),color=paste0(treat,ml)),
               fun.y=mean, geom="line") + #insert monthly mean as scatter plots
  ggplot2::annotate(geom="text", x=1:106, y=-50, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
  ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=-150, label=seq(2006, 2015, 1), size=4) + #year
  geom_vline(xintercept = 18, color="grey", linetype="dashed", size=1) + #nyc
  geom_vline(xintercept = 50, color="grey", linetype="dashed", size=1) + #california and oregon
  geom_vline(xintercept = 67, color="grey", linetype="dashed", size=1) + #vermont
  ggplot2::annotate(geom="label", x=15.5, y=1600, label="NYC", size=3) + #add label for nyc
  ggplot2::annotate(geom="label", x=43, y=1600, label="California & Oregon", size=3) + #add label for california
  ggplot2::annotate(geom="label", x=71, y=1600, label="Vermont", size=3) + #add label for vermont
  coord_cartesian(ylim=c(0, 2500), expand = FALSE, clip = "off") + 
  labs(title="Calories trend", x="", y="Monthly mean calories per order, at restaurant level",
       caption="Note: vertical dashed lines represent menu labeling inplementation in NYC (the first), California and Oregon (the biggest group) and Vermont (the last).") +
  scale_color_discrete(name="Menu Lableing", labels=c("Never labeled", "Pre-labeling", "Post-labeling")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-by-ml-detailed.jpeg", dpi="retina")

# differ by treat
ggplot(data=unmatched, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=as.character(treat), color=as.character(treat))) +
  geom_point(size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=as.character(treat),color=as.character(treat)),
               fun.y=mean, geom="line") + #insert monthly mean as scatter plots
  ggplot2::annotate(geom="text", x=1:106, y=-50, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
  ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=-150, label=seq(2006, 2015, 1), size=4) + #year
  geom_vline(xintercept = 18, color="grey", linetype="dashed", size=1) + #nyc
  geom_vline(xintercept = 50, color="grey", linetype="dashed", size=1) + #california and oregon
  geom_vline(xintercept = 67, color="grey", linetype="dashed", size=1) + #vermont
  ggplot2::annotate(geom="label", x=20.5, y=1600, label="NYC", size=3) + #add label for nyc
  ggplot2::annotate(geom="label", x=57, y=1600, label="California & Oregon", size=3) + #add label for california
  ggplot2::annotate(geom="label", x=71, y=1600, label="Vermont", size=3) + #add label for vermont
  coord_cartesian(ylim=c(0, 2500), expand = FALSE, clip = "off") + 
  labs(title="Calories trend", x="", y="Monthly mean calories per order, at restaurant level",
       caption="Note: vertical dashed lines represent menu labeling inplementation in NYC (the first), California and Oregon (the biggest group) and Vermont (the last).") +
  scale_color_discrete(name="Menu Lableing", labels=c("Never labeled", "Has labeling")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-by-ml.jpeg", dpi="retina")

# only california and oregon
ggplot(data=unmatched %>% filter (treat==0 | (treat==1&(entry==253))),
       aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=as.character(treat), color=as.character(treat))) +
  geom_point(data=unmatched %>% filter (calorie>=500 & calorie<=2000), size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=as.character(treat),color=as.character(treat)),
               fun.y=mean, geom="line") + #insert monthly mean as scatter plots
  ggplot2::annotate(geom="text", x=1:106, y=450, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
  ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=350, label=seq(2006, 2015, 1), size=4) + #year
  geom_vline(xintercept = 18, color="grey", linetype="dashed", size=1) + #nyc
  geom_vline(xintercept = 50, color="grey", linetype="dashed", size=1) + #california and oregon
  geom_vline(xintercept = 67, color="grey", linetype="dashed", size=1) + #vermont
  ggplot2::annotate(geom="label", x=20.5, y=1600, label="NYC", size=3) + #add label for nyc
  ggplot2::annotate(geom="label", x=57, y=1600, label="California & Oregon", size=3) + #add label for california
  ggplot2::annotate(geom="label", x=71, y=1600, label="Vermont", size=3) + #add label for vermont
  coord_cartesian(ylim=c(500, 2000), expand = FALSE, clip = "off") + 
  labs(title="Calories trend, CA and OR only", x="", y="Monthly mean calories per order, at restaurant level",
       caption="Note: data include comparison restaurants, and treated restaurants in California and Oregon.") +
  scale_color_discrete(name="Menu Lableing", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-by-ml-CA-OR-only.jpeg", dpi="retina")

# by diff entry date
ggplot(data=unmatched %>%
         filter ((treat==1&(entry==239|entry==247|entry==251|entry==253|entry==254))|treat==0),
       aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
           group=as.character(treat*entry), color=as.character(treat*entry))) +
  geom_point(data=unmatched %>% filter (calorie>=750 & calorie<=1750), size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=as.character(treat*entry),color=as.character(treat*entry)),
               fun.y=mean, geom="line") + #insert monthly mean as scatter plots
  ggplot2::annotate(geom="text", x=1:106, y=720, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
  ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=650, label=seq(2006, 2015, 1), size=4) + #year
  geom_vline(xintercept = 35, color="grey", linetype="dashed", size=1) + #nassau
  geom_vline(xintercept = 43, color="purple", linetype="dashed", size=1) + #Mont/Mult
  geom_vline(xintercept = 47, color="green", linetype="dashed", size=1) + #MA
  geom_vline(xintercept = 49, color="skyblue", linetype="dashed", size=1) + #CA/OR
  geom_vline(xintercept = 50, color="orange", linetype="dashed", size=1) + #NJ/ME
  ggplot2::annotate(geom="label", x=35, y=1400, label="Nassau", size=3) + #add label for nassau
  ggplot2::annotate(geom="label", x=43, y=1600, label="Montgomery&Multnomah", size=3) + #add label for mult/mont
  ggplot2::annotate(geom="label", x=47, y=1200, label="MA", size=3) + #add label for MA
  ggplot2::annotate(geom="label", x=49, y=1500, label="CA&OR", size=3) + #add label for california
  ggplot2::annotate(geom="label", x=50, y=1000, label="NJ&ME", size=3) + #add label for NJ/ME
  coord_cartesian(ylim=c(750,1750), expand = FALSE, clip = "off") + 
  labs(title="Calories trend, by city/state", x="", y="Monthly mean calories per order, at restaurant level",
       caption="Note: data include comparison restaurants, and treated restaurants in cities and states with more than 10 restaurants.") +
  scale_color_manual(name="City/state",
                       labels=c("No labeling", "Nassau", "Montgomery & Multnomah", "MA", "CA & OR", "NJ & ME"),
                       values=c("red", "grey", "purple", "green", "skyblue", "orange")) +
  #scale_color_brewer(palette = "Set3") +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-by-ml-city.jpeg", dpi="retina")

### visualization, months available ----
hist(unmatched$monthno-203, breaks = 50, xlim = c(0,106),
     main = "Distribution of data availability Overall",
     xlab = "Continuous month, t0 = December 2006")

hist(matched$relative, breaks = 50, xlim = c(-60, 80),
     main = "Distribution of data availability pre- and post-labeling",
     xlab = "Relative month, the month of labeling = t1")

# % sales from drive through, by drive-through status
ggplot(data=unmatched, aes(x=drive, color=as.character(drive_thru),fill=as.character(drive_thru))) +
  geom_histogram(bins = 200, position="identity", alpha=0.5) +
  labs(title="% sales from drive-thrugh, by drive-through status",
       x="% sales from drive-through", y="Number of restaurants",
       caption="") +
  scale_color_discrete(name="Has drive through", labels=c("No", "Yes")) +
  scale_fill_discrete(name="Has drive through", labels=c("No", "Yes")) +
  scale_x_continuous(labels=scales::percent)
#ggsave("tables/analytic-model/matching/results/pct-drive-thru-orders-by-drive-thru-status.jpeg", dpi="retina")

  
### descriptive data ----
summary <- rbind(unmatched %>%
                   mutate(data="unmatched") %>%
                   mutate(rev=dollar*count) %>%
                   mutate(nonwhite=asian+black+hisp) %>%
                   dplyr::select(data,calorie,dollar,count,drive,treat,year,month,rev,meal,nonwhite,hsbelow,under18,median_income),
                 matched %>%
                   mutate(data="matched") %>%
                   mutate(rev=dollar*count) %>%
                   mutate(nonwhite=asian+black+hisp) %>%
                   dplyr::select(data,calorie,dollar,count,drive,treat,year,month,rev,meal,nonwhite,hsbelow,under18,median_income))
summary <- summary %>%
  filter(year==2015&month==1)
            
summary$treat <- factor(summary$treat, levels=c(1, 0),
                          labels = c("Yes", "No"))
label(summary$calorie) <- "Calorie"
label(summary$dollar) <- "Mean spending per order"
label(summary$count) <- "Total # of transactions"
label(summary$drive) <- "% sales from drive-through"

table1::table1(data=summary %>% filter(data=="matched"),
       ~calorie+rev+count+dollar+drive+meal+nonwhite+median_income+hsbelow+under18|treat,
       caption="<b>Summary statistics, matched data, January 2015</b>")

### matched data, preparing data ----
matched <- read.csv("data/calorie-aims/matched-restaurants.csv", stringsAsFactors = FALSE)
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
hist(matched$open_month, breaks = 50)

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
matched$relative <- matched$monthno - matched$match_to +1
matched$relative2 <- matched$monthno - matched$match_to #month 0 is first month of ML
matched$post <- ifelse(matched$relative<0, 0, 1)

# set albany to be ref group
matched$match_place <- factor(matched$match_place)
matched <- within(matched, match_place<-relevel(match_place, ref="albany"))

# summary stats for restarants continuously open for 106 months
length(unique(matched$id[matched$open_month==106&matched$treat==1])) #59
length(unique(matched$id[matched$open_month==106&matched$treat==0])) #95

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

### matched data, models ----
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

### have relative month as factor ----
# month as relative and factor
# set month 1 as ref group
matched$relative.factor <- factor(matched$relative)
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="1"))
summary(matched$relative)

mod.factor <- lm(formula = paste0("calorie~treat+ml*relative.factor+as.factor(monthno)+",cov),
                   data = matched, weights = weights)
summary(mod.factor)
tidy_mod.factor <- tidy(mod.factor)
#write.csv(tidy_mod.factor, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/relative-month-as-factor-12mon.csv")

# import coefficients and clean table
tidy_mod.factor <- tidy_mod.factor[-c(134:252), -(3:4)]
# fill up the 2 months of month=1
tidy_mod.factor[264:265, 1] <- c("1","1")
tidy_mod.factor[264:265, 2] <- c(0,0)
tidy_mod.factor[264:265, 3] <- c(NA,NA)
tidy_mod.factor$intercept <- as.numeric(tidy_mod.factor[1,2]) #long to wide
tidy_mod.factor$treat <- as.numeric(tidy_mod.factor[2,2])
tidy_mod.factor$ml <- as.numeric(tidy_mod.factor[3,2])
tidy_mod.factor <- tidy_mod.factor[-c(1:3),]
tidy_mod.factor$group <- 0 #differentiate treat/comp groups
tidy_mod.factor$group[c(131:260,262)] <- 1
colnames(tidy_mod.factor)[c(1:3)] <- c("month", "coef.month", "p")
#convert month to numbers
tidy_mod.factor$month <- as.integer(gsub(tidy_mod.factor$month, pattern="relative.factor|ml:",replacement = ""))
summary(tidy_mod.factor$month)
#prepare for visualization
tidy_mod.factor <- tidy_mod.factor[order(tidy_mod.factor$group,tidy_mod.factor$month),]
tidy_mod.factor$coef.ml <- ifelse(tidy_mod.factor$group==1, tidy_mod.factor$coef.month, NA)
tidy_mod.factor$coef.month[132:262] <- tidy_mod.factor$coef.month[1:131]
tidy_mod.factor <- tidy_mod.factor %>%
  mutate(calorie=case_when(group==0 ~ coef.month+intercept,
                           group==1&month<=0 ~ coef.month+treat+intercept,
                           group==1&month>0 ~ coef.month+treat+ml+coef.ml+intercept))
summary(tidy_mod.factor$calorie)
tidy_mod.factor$diff <- NA
tidy_mod.factor$diff[1:131] <- tidy_mod.factor$calorie[1:131] - tidy_mod.factor$calorie[132:262]
summary(tidy_mod.factor$diff)
tidy_mod.factor$coef.ml[1:131] <- tidy_mod.factor$coef.month[1:131]

#visualize coef
#tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/relative-month-as-factor.csv")
ggplot() + #
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*2.25+1350), linetype="longdash") + #add diff between 2 groups
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  #ggplot2::annotate(geom="label", x=0, y=1700, label="Month 0", size=3) + #add label for ML
  #ggplot2::annotate(geom="label", x=-44, y=950, label="Diff=25", size=3) + #add label for ML
  #ggplot2::annotate(geom="label", x=1.5, y=1525, label="Increase=13", size=3) + #add label for ML
  geom_hline(yintercept = 0, color="grey", linetype="solid", size=0.5) + #add line for ref group
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(900,1800),breaks=seq(900,1800,100),
                     sec.axis = sec_axis(~(.-1350)/2.25, name="Difference")) +
  scale_x_continuous(breaks=seq(-49,81,5)) +
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories",
       caption="No estimates past month 71 because there was only 1 restaurant.") + #All coefficients set the reference group as month=0 (grey dashed line), the month immediately before labeling began. \n 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/coef-month-interaction-in-calories.jpeg", dpi="retina")

### check which treated restaurants continuously operated 6 months before and after ML ----
table(matched$relative[matched$treat==1&matched$relative>=-5&matched$relative<=6])
tmp <- matched %>%
  filter(relative>=-5 & relative<=6 & !is.na(calorie) & treat==1) %>%
  group_by(id,match_place) %>%
  mutate(id_number=row_number(id)) %>%
  mutate(open_number=max(id_number)) %>%
  relocate(address,match_place,relative,open_number) %>%
  filter(open_number==7) 
table(tmp$open_number) #6,7,8,9,10,11
length(unique(paste0(tmp$id[tmp$open_month!=12&tmp$treat==0],tmp$match_place[tmp$open_month!=12&tmp$treat==0])))

### check restaurants in philly
tmp <- matched %>%
  filter(address=="3000 Island Ave, Philadelphia, PA 19153") 

### cap data to time frame  ----
ggplot(matched, aes(x=relative2)) + #, color=as.factor(treat)
  geom_histogram(binwidth = 1, fill="white", color="black") + #, position = "identity"
  scale_x_continuous(breaks=seq(-50,80,5)) +
  labs(title="Number of months open pre- and post-labeling", x="Month", y="Frequency",
       caption="") +
  geom_hline(yintercept = 951*0.9, color="red", linetype="dashed", size=1) +
  geom_hline(yintercept = 951*0.75, color="red", linetype="dashed", size=1) +
  geom_hline(yintercept = 951*0.5, color="red", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=-50, y=875, label="90%", size=3) + #add label threshold
  ggplot2::annotate(geom="label", x=-50, y=735, label="75%", size=3) + #add label threshold
  ggplot2::annotate(geom="label", x=-50, y=500, label="50%", size=3)  #add label threshold

month <- NULL
for (i in -50:80) {
  if (length(unique(matched$id[matched$relative2==i]))>=951*0.75) {
    month <- c(month, i)
  }
} #90% of peak: [-15,11], 75% of peak: [-15,29], 50% of peak: [-28,56], empirically: [-33,56]
print(paste0("Month [", min(month), ", ", max(month), "]"))
rm(i, month)

### have relative month as factor, calorie=treat+month+treat*month ----
matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="0"))

mod.factor <- lm(formula = paste0("calorie~treat*relative2.factor+as.factor(monthno)+",cov),
                 data = matched%>%filter(relative2>=-28&relative2<=56), weights = weights)
summary(mod.factor)
tidy_mod.factor <- tidy(mod.factor)
#write.csv(tidy_mod.factor, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor)_restrict_time.csv")

mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(monthno),
                    data = pdata.frame(matched%>%filter(relative2>=-28&relative2<=56), index = c("id")),
                    weights = weights, model = "within")
summary(mod.factor)
tidy_mod.factor <- tidy(mod.factor)
#write.csv(tidy_mod.factor, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor)_restrict_time_restsurantFE.csv")

# add year and month factor as covariate
tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor).csv")
summary(tidy_mod.factor$calorie) #[957,1952]
summary(tidy_mod.factor$diff) #[-156,95]
ggplot() + #
  geom_point(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group)), size=1) +
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*3+1100), linetype="longdash", color="orange") + #add diff between 2 groups
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=3, y=1800, label="1st month \n of labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(500,2000),breaks=seq(500,2000,100),
                     sec.axis = sec_axis(~(.-1100)/3, name="Difference")) +
  scale_x_continuous(breaks=seq(-50,80,5)) +
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories",
       caption="Orange dashed lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑calendar_year_month") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/calorie=month+treat+month_month+year(factor).jpeg", dpi="retina")

# add year and month factor as covariate, limit months
tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor)_restrict_time.csv")
summary(tidy_mod.factor$calorie) #[1269,1671]
summary(tidy_mod.factor$diff) #[-60,-3]
ggplot() + #
  geom_point(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group)), size=1) +
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*7.5+1250), linetype="longdash", color="orange") + #add diff between 2 groups
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=3, y=1800, label="1st month \n of labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(500,2000),breaks=seq(500,2000,100),
                     sec.axis = sec_axis(~(.-1250)/7.5, name="Difference")) +
  scale_x_continuous(breaks=seq(-50,80,5)) +
  labs(title="Effect of menu labeling on calories purchased, limit time", x="Month", y="Calories",
       caption="Orange dashed lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑calendar_year_month") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/calorie=month+treat+month_month+year(factor).jpeg", dpi="retina")

# add year and month factor as covariate, limit months and restID FE
tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor)_restrict_time_restsurantFE.csv")
summary(tidy_mod.factor$calorie) #[-120,11]
summary(tidy_mod.factor$diff) #[-24,28]
ggplot() + #
  geom_point(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group)), size=1) +
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*4-300), linetype="longdash", color="orange") + #add diff between 2 groups
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=3, y=50, label="1st month \n of labeling", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(-500,100),breaks=seq(-500,100,50),
                     sec.axis = sec_axis(~(.+300)/4, name="Difference")) +
  scale_x_continuous(breaks=seq(-50,80,5)) +
  labs(title="Effect of menu labeling on calories purchased, limit time", x="Month", y="Calories",
       caption="Orange dashed lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑calendar_year_month + ∑RestaurantFE") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/calorie=month+treat+month_month+year(factor).jpeg", dpi="retina")


### cal=group*post|group*month*post, month as linear, month = 0 for 1st month of labeling ----
# mod1, cal=group+post+group*post,
# create relative2, so month = 0 for 1st month of labeling
mod1.matched <- lm(formula = paste0("calorie~treat*post+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod1.matched)
mod2.matched <- lm(formula = paste0("calorie~treat*post+as.factor(month)+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod2.matched)
#add year and month FE
mod3.matched <- lm(formula = paste0("calorie~treat*post+as.factor(monthno)+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod3.matched)
# mod4, cal=group*post*month, month as linear
mod4.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod4.matched)
#add month fixed effects
mod5.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+as.factor(month)+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod5.matched)
#add year and month FE
mod6.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat++as.factor(monthno)+", cov),
                   data = matched%>%filter(relative2>=-12&relative2<=11), weights = weights)
#summary(mod6.matched)

stargazer(mod1.matched, mod2.matched, mod3.matched, mod4.matched,mod5.matched,mod6.matched,
          type="html",
          title="The effect of menu labeling on calories purchased, 12 months pre- and post-labeling",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("Constant","treat","post","treat:post","relative2", "treat:relative2","post:relative2","treat:post:relative2"),
          order=c("^Constant$","^treat$","^post$", "^treat:post$","^relative2$","^treat:relative2$","^post:relative2$","^treat:post:relative2$"),
          covariate.labels=c("Intercept, β<sub>0</sub>","In labeling group, β<sub>1</sub>","Post labeling, β<sub>2</sub>",
                             "In labeling group*post labeling, β<sub>3</sub>",
                             "Month, β<sub>4</sub>","In labeling group*month, β<sub>5</sub>",
                             "Post labeling*month, β<sub>6</sub>",
                             "In labeling group*post labeling*month, β<sub>7</sub>"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Model","<em>diff-in-diff</em>","<em>Month FE</em>","<em>Year+month FE</em>",
                             "<em>diff-in-diff</em>","<em>Month FE</em>","<em>Year+month FE</em>"),
                           c("Unique restaurants", "951", "951","951","951","951","951","951")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Post labeling = 1 for both treated and comparison restaurants after", 
                    "labeling implementation for treated restaruants. All models include restaurant level and community demographic control vars."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/cal=treat+post+relative2-12mon.html")

### cal=group*month*post ----
mod1.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+", cov),
                   data = matched, weights = weights)
#add month fixed effects
mod2.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+as.factor(month)+", cov),
                   data = matched, weights = weights)
#add year and month FE
mod3.matched <- lm(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+as.factor(monthno)+", cov),
                   data = matched, weights = weights)
# add restaurant FE
mod4.matched <- plm(formula = calorie~treat+post+relative2+relative2*treat+
                      post*treat+post*relative2+post*relative2*treat+as.factor(monthno),
                    data = pdata.frame(matched, index = c("id")),
                    weights = weights, model = "within")
# add restaurant RE
mod5.matched <- lme4::lmer(formula = paste0("calorie~treat+post+relative2+relative2*treat+
                                    post*treat+post*relative2+post*relative2*treat+as.factor(monthno)+", cov,"+(1|id)"),
                           data=matched, weights = weights)
# add state FE
mod6.matched <- plm(formula = calorie~treat+post+relative2+relative2*treat+
                      post*treat+post*relative2+post*relative2*treat+as.factor(monthno),
                    data = pdata.frame(matched, index = c("state")),
                    weights = weights, model = "within")
stargazer(mod1.matched, mod2.matched, mod3.matched,mod6.matched,mod4.matched,mod5.matched,
          type="html",
          title="The effect of menu labeling on calories purchased",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("Constant","treat","post","treat:post","relative2", "treat:relative2","post:relative2","treat:post:relative2"),
          order=c("^Constant$","^treat$","^post$", "^treat:post$","^relative2$","^treat:relative2$","^post:relative2$","^treat:post:relative2$"),
          covariate.labels=c("Intercept, β<sub>0</sub>","In labeling group, β<sub>1</sub>","Post labeling, β<sub>2</sub>",
                             "In labeling group*post labeling, β<sub>3</sub>",
                             "Month, β<sub>4</sub>","In labeling group*month, β<sub>5</sub>",
                             "Post labeling*month, β<sub>6</sub>",
                             "In labeling group*post labeling*month, β<sub>7</sub>"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Model","<em>diff-in-diff</em>","<em>Month FE</em>","<em>Year+month FE</em>",
                             "<em>State FE</em>","<em>Restaurant FE</em>","<em>Restaurant RE</em>"),
                           c("Unique restaurants", "951", "951","951","951","951","951","951")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Post labeling = 1 for both treated and comparison restaurants after", 
                    "labeling implementation for treated restaruants. All models include restaurant level and community demographic control vars."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/cal=treat+post+relative2-additional-specs.html")

### cal=group*month*post, cap time, CA only ----
#figure out the data availability pattern in CA
summary(matched$relative2[matched$match_place=="ca"]) #[-49, 56]
length(unique(matched$id[matched$match_place=="ca"])) #710

ggplot(matched%>%filter(match_place=="ca"), aes(x=relative2)) + #, color=as.factor(treat)
  geom_histogram(binwidth = 1, fill="white", color="black") + #, position = "identity"
  scale_x_continuous(breaks=seq(-49,56,5)) +
  labs(title="Number of months open pre- and post-labeling, CA", x="Month", y="Frequency",
       caption="") +
  geom_hline(yintercept = 710*0.9, color="red", linetype="dashed", size=1) +
  geom_hline(yintercept = 710*0.75, color="red", linetype="dashed", size=1) +
  geom_hline(yintercept = 710*0.5, color="red", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=-49, y=650, label="90%", size=3) + #add label threshold
  ggplot2::annotate(geom="label", x=-49, y=550, label="75%", size=3) + #add label threshold
  ggplot2::annotate(geom="label", x=-49, y=375, label="50%", size=3)  #add label threshold

month <- NULL
for (i in -49:56) {
  if (length(unique(matched$id[matched$relative2==i&matched$match_place=="ca"]))>=710*0.5) {
    month <- c(month, i)
  }
} #90% of peak: [-15,11], 75% of peak: [-15,23], 50% of peak: [-34,56]
print(paste0("Month [", min(month), ", ", max(month), "]"))
rm(i, month)

formula <- "calorie~post*relative2*treat+factor(monthno)"
mod1.matched <- plm(formula = formula,data = matched, index = "id",weights = weights, model = "within")
mod2.matched <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-15&relative2<=11), index = c("id")),
                    weights = weights, model = "within")
mod3.matched <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-15&relative2<=29), index = c("id")),
                    weights = weights, model = "within")
mod4.matched <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-28&relative2<=56), index = c("id")),
                    weights = weights, model = "within")
mod5.matched <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-33&relative2<=56), index = c("id")),
                    weights = weights, model = "within")
mod1.matched.ca <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(match_place=="ca"), index = c("id")),
                    weights = weights, model = "within")
mod2.matched.ca <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-15&relative2<=11&match_place=="ca"), index = c("id")),
                    weights = weights, model = "within")
mod3.matched.ca <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-15&relative2<=23&match_place=="ca"), index = c("id")),
                    weights = weights, model = "within")
mod4.matched.ca <- plm(formula = formula,
                    data = pdata.frame(matched%>%filter(relative2>=-34&relative2<=56&match_place=="ca"), index = c("id")),
                    weights = weights, model = "within")

stargazer(mod1.matched, mod2.matched, mod3.matched,mod4.matched,mod5.matched,
          mod1.matched.ca, mod2.matched.ca, mod3.matched.ca,mod4.matched.ca,
          type="html",
          title="The effect of menu labeling on calories purchased, different time frames",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("post","treat:post","relative2", "treat:relative2","post:relative2","treat:post:relative2"),
          order=c("^post$", "^treat:post$","^relative2$","^treat:relative2$","^post:relative2$","^treat:post:relative2$"),
          covariate.labels=c("Post labeling, β<sub>2</sub>",
                             "In labeling group*post labeling, β<sub>3</sub>",
                             "Month, β<sub>4</sub>","In labeling group*month, β<sub>5</sub>",
                             "Post labeling*month, β<sub>6</sub>",
                             "In labeling group*post labeling*month, β<sub>7</sub>"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Time frame","All","[-15, 11]","[-15, 29]","[-28, 56]", "[-33, 56]",
                             "All","[-15, 11]", "[-15, 23]", "[-34, 56]"),
                           c("% of Month 0", "All","90%","75%","50%","Empirical obs",
                             "All","90%","75%","50%"),
                           c("Locations", "All", "All", "All", "All", "All",
                             "CA","CA","CA","CA")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Post labeling = 1 for both treated and comparison restaurants after", 
                    "labeling implementation for treated restaruants. All models include restaurant level and community demographic control vars,",
                    "as well as calendar month fixed effects, and restaurant fixed effects."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/cal=treat+post+relative2-restaurantFE-cap-time-monthFE.html")

### cal=group*month(factor), month as factor ----
#test quarter FE, instead of month FE
matched$quarter <- ifelse(matched$month<=3, 1,
                          ifelse(matched$month>3&matched$month<=6, 2,
                                 ifelse(matched$month>6&matched$month<=9, 3,4)))
matched$quarter_year <- paste0(matched$year, "Q", matched$quarter)
mod.factor <- lm(formula = paste0("calorie~treat*relative2.factor+factor(quarter_year)+",cov),
                 data = matched%>%filter(relative2>=-33&relative2<=56), weights = weights)
car::vif(mod.factor)

mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter(relative2>=-33&relative2<=56&match_place!="ca"), # &open12==1
                  index = c("id"), weights = weights, model = "within")
summary(mod.factor)
tidy_mod.factor <- tidy(mod.factor)
#write.csv(tidy_mod.factor, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_monthFE_restaurantFE_captime_open12.csv")

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

# add year and month factor as covariate
#tidy_mod.factor <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_month+year(factor).csv")
summary(tidy_mod.factor$calorie) #[-81,75]
summary(tidy_mod.factor$diff) #[-62,22]
ggplot() + #
  geom_point(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group)), size=1) +
  geom_line(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.05),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.01),aes(x=month, y=diff*1-300), color="red") + 
  geom_point(data=tidy_mod.factor%>%filter(!is.na(p.diff)&p.diff<0.001),aes(x=month, y=diff*1-300), color="purple") + 
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=0.5) +
  geom_vline(xintercept = -1, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate(geom="label", x=0, y=-100, label="Pre-label \n to label", size=3) + #add label for ML
  geom_hline(yintercept = -275, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + #
  scale_y_continuous(limits=c(-400,100),breaks=seq(-400,100,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=seq(-33,56,2)) +
  labs(title="Effect of menu labeling on calories purchased, excl. CA", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. Orange dot: p<0.05, red: p<0.01, purple: p<0.001 \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/cal=treat+month_monthFE_restaurantFE_captime_notCA.jpeg", dpi="retina")

### cal=group*month*post, limit to restaurants open for an extended time ----
#histogram of restaurants open month
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
  filter(relative2<=5&relative2>=-6) %>%
  mutate(n=n()) %>%
  mutate(open6 = ifelse(n==12, 1,0)) %>%
  dplyr::select(id, treat, match_place, open6) %>%
  distinct()
table(tmp$open6)

tmp1 <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=11&relative2>=-12) %>%
  mutate(n=n()) %>%
  mutate(open12 = ifelse(n==24, 1,0)) %>%
  dplyr::select(id, treat, match_place, open12) %>%
  distinct()
table(tmp1$open12)

tmp2 <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=17&relative2>=-18) %>%
  mutate(n=n()) %>%
  mutate(open18 = ifelse(n==36, 1,0)) %>%
  dplyr::select(id, treat, match_place, open18) %>%
  distinct()
table(tmp2$open18)

tmp3 <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=23&relative2>=-24) %>%
  mutate(n=n()) %>%
  mutate(open24 = ifelse(n==48, 1,0)) %>%
  dplyr::select(id, treat, match_place, open24) %>%
  distinct()
table(tmp3$open24)

tmp <- merge(tmp, tmp1, by=c("id", "treat", "match_place"))
tmp <- merge(tmp, tmp2, by=c("id", "treat", "match_place"))
tmp <- merge(tmp, tmp3, by=c("id", "treat", "match_place"))
rm(tmp1, tmp2, tmp3)
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)

tmp <- matched[!duplicated(matched$id, matched$treat, matched$match_place), ]
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(tmp$open_month, breaks = 106, xlim = c(1,120),
     main = "Histogram of restaurant open month", xlab = "# of total months open")
hist(tmp$open_before, breaks = 106, xlim = c(1,50),
     main = "", xlab = "# of months open before labeling")
hist(tmp$open_after, breaks = 106, xlim=c(1,80),
     main = "", xlab = "# of months open after labeling")
summary(tmp$open_month) 
summary(tmp$open_before) 
summary(tmp$open_after)

formula <- "calorie~post*relative2*treat+factor(monthno)"
mod1.matched <- plm(formula = formula,data = matched,
                    index = "id",weights = weights, model = "within")
mod2.matched <- plm(formula = formula,data = matched%>%filter(open_month==106),
                    index = "id",weights = weights, model = "within")
mod3.matched <- plm(formula = formula,data = matched%>%filter(open_month>=72),
                    index = "id",weights = weights, model = "within")
mod4.matched <- plm(formula = formula,data = matched%>%filter(open_month>=91),
                    index = "id",weights = weights, model = "within")
mod5.matched <- plm(formula = formula,data = matched%>%filter(open_month>=95),
                    index = "id",weights = weights, model = "within")
mod6.matched <- plm(formula = formula,data = matched%>%filter(open6==1),
                    index = "id",weights = weights, model = "within")
mod7.matched <- plm(formula = formula,data = matched%>%filter(open12==1),
                    index = "id",weights = weights, model = "within")
mod8.matched <- plm(formula = formula,data = matched%>%filter(open18==1),
                    index = "id",weights = weights, model = "within")
mod9.matched <- plm(formula = formula,data = matched%>%filter(open24==1),
                    index = "id",weights = weights, model = "within")
stargazer(mod1.matched, mod2.matched, mod3.matched,mod4.matched,mod5.matched,
          mod6.matched,mod7.matched,mod8.matched,mod9.matched,
          type="html",
          title="The effect of menu labeling on calories purchased, different time frames",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("post","treat:post","relative2", "treat:relative2","post:relative2","treat:post:relative2"),
          order=c("^post$", "^treat:post$","^relative2$","^treat:relative2$","^post:relative2$","^treat:post:relative2$"),
          covariate.labels=c("Post labeling, β<sub>2</sub>",
                             "In labeling group*post labeling, β<sub>3</sub>",
                             "Month, β<sub>4</sub>","In labeling group*month, β<sub>5</sub>",
                             "Post labeling*month, β<sub>6</sub>",
                             "In labeling group*post labeling*month, β<sub>7</sub>"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("# of months open","Any", "All months, 106", "Median, 72", "75%, 91", "90%, 95",
                             "Min 6 months", "Min 12 months", "Min 18 months", "Min 24 months")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Post labeling = 1 for both treated and comparison restaurants after", 
                    "labeling implementation for treated restaruants. All models include restaurant level and community demographic control vars,",
                    "as well as month fixed effects, and restaurant fixed effects."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/cal=treat+post+relative2-restaurantFE-monthFE-limitOpenPeriod.html")













### month as factor model, by location ----
trend <- NULL
tmp <- data.frame(1:30)
colnames(tmp)[1] <- "month"

mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
              data = matched%>%filter(open24==1&relative2>=-33&relative2<=56&match_place=="ca"), # &open12==1
              index = c("id"), weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor)
#write.csv(trend, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-sample-restriction_CA.csv")

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
tmp1 <- tidy_mod.factor[tidy_mod.factor$month>=-30&tidy_mod.factor$month<0&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp1 <- tmp1[order(tmp1$month, decreasing = TRUE), ]
tmp1$pre <- cumsum(tmp1$diff)
tmp1$month <- -tmp1$month
tmp2 <- tidy_mod.factor[tidy_mod.factor$month>=0&tidy_mod.factor$month<=29&!is.na(tidy_mod.factor$diff), c(1,7)]
tmp2 <- tmp2[order(tmp2$month), ]
tmp2$post <- cumsum(tmp2$diff)
tmp2$month <- tmp2$month+1
tmp <- merge(tmp1, tmp2, by="month")
tmp$open <- "open24"
trend <- rbind(trend, tmp)

table(trend$open)
names(trend)
trend <- trend %>%
  #dplyr::select(-diff.x, -diff.y) %>%
  mutate(accum = post - pre) %>% 
  mutate(mean_accum = accum/month)
Open <- factor(trend$open,
               levels = c("any","median73","3rd93","90%95","all","open6","open12","open18","open24"))

#trend <- read.csv("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-location.csv")
ggplot(data=trend, aes(x=month, y=mean_accum, group=Open, color=Open)) + #
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,10),breaks=seq(-50,10,10)) +
  scale_x_continuous(breaks=seq(1,30,1)) +
  labs(title="Mean diff-in-diff per month, by different samples, CA", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(factor) + treat*month(factor) + ∑month + ∑restaurant") + 
  scale_color_manual(name="Time open",
                     labels=c("Any month","All 106 months", "At least 90%, 95 months",
                              "At least 75%, 91 months","At least median, 72 months",
                              "At least 6 months pre- and post-labeling",
                              "At least 12 months pre- and post-labeling",
                              "At least 18 months pre- and post-labeling",
                              "At least 24 months pre- and post-labeling"),
                       values = c("hotpink","#F47851","#13B0E4","olivedrab3","#ffd400","grey",
                                  "#4D8C57","purple","black")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor/mean-diff-in-diff-by-sample-restrictions_CA.jpeg", dpi="retina")













