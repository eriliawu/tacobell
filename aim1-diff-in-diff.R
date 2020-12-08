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
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday) %>%
  arrange(id, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat, match_place) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(matched$rank) #sanity check, the max should be 106
hist(matched$open_month, breaks = 50)

# get relative month for pre-/post-labeling, month of labeling=1
matched$relative <- matched$monthno - matched$match_to +1

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
  geom_vline(xintercept = 1, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=2, y=1450, label="Menu labeling", size=3) + #add label for nyc
  coord_cartesian(ylim=c(1000,1500), expand = FALSE, clip = "off") +
  scale_x_continuous(breaks=seq(-11,12,1)) +
  labs(title="Calories trend, 12 months before and after menu labeling", x="Month", y="Calories",
       caption="Note: California account for 77% of all treated restaurants. Grey dashed line represents the month when labeling was implemented.") +
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
mod2.matched <- lm(formula = paste0("calorie~treat+ml*relative+as.factor(monthno)+",cov),
                   data = matched, weights = weights)
summary(mod2.matched)
# diff-in-diff, with fixed effect at restaurant level
mod3.matched <- plm(formula = calorie~treat+ml*relative+as.factor(monthno),
                    data = pdata.frame(matched, index = c("id")),
                    weights = weights, model = "within")
summary(mod3.matched)
# diff-in-diff, random effect at restaurant level
mod4.matched <- lme4::lmer(formula = paste0("calorie~treat+ml*relative+as.factor(monthno)+", cov,"+(1|id)"),
                           data=matched, weights = weights)
summary(mod4.matched)
#city fixed effect
mod5.matched <- plm(formula = paste0("calorie~treat+ml*relative+as.factor(monthno)+",cov),
                    data = pdata.frame(matched%>%mutate(location=paste(state,"+",county)), index = c("location")),
                    weights = weights, model = "within")
summary(mod5.matched)

length(unique(matched$id[matched$relative<=12&matched$relative>=-11])) #951
length(unique(matched$id)) #951
length(unique(matched$id[matched$open_month==106])) #154

stargazer(mod1.matched, mod2.matched, mod5.matched,mod3.matched, mod4.matched,
          type="html",
          title="The effect of menu labeling on calories purchased, continuously open restaurants only",
          dep.var.caption = "Dependent variable: mean calories per order",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("treat","ml", "relative", "ml:relative"),
          order=c("^treat$","^ml$", "^relative$", "^ml:relative$"),
          covariate.labels=c("In labeling group","Has labeling", 
                             "Month (relative)", "Has labeling*month (relative)"),
          keep.stat = c("n","adj.rsq"),
          add.lines = list(c("Model","<em>diff-in-diff</em>","<em>Month continuous</em>","<em>City FE</em>",
                             "<em>Restaurant FE</em>","<em>Restaurant RE</em>"),
                           c("Unique restaurants", "154", "154","154","154","154","154")),
          notes = c("In labeling group = 1 for all treated restaurants at all times; Has labeling = 1 for treated",
                    "restaruants when labeling is implemented. All models has calendar month as control vars."),
          notes.align = "l",
          out="tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/aim1-matched-data-continuous.html")

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
                           c("Unique restaurants","951","6","16","10","6","6","43","80","710","91","41")),
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
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="0"))
summary(matched$relative)

mod.factor <- lm(formula = paste0("calorie~treat+ml*relative.factor+as.factor(monthno)+",cov),
                   data = matched, weights = weights)
summary(mod.factor)
tidy_mod.factor <- tidy(mod.factor)
#write.csv(tidy_mod.factor, row.names = FALSE,"tables/analytic-model/aim1-diff-in-diff/regression/diff-intercept-pre-post/relative-month-as-factor.csv")

# import coefficients and clean table
tidy_mod.factor <- tidy_mod.factor[-c(134:252), -(3:4)]
# fill up hte 2 months of month=0
tidy_mod.factor[264:265, 1] <- c("0","0")
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
tidy_mod.factor <- tidy_mod.factor[-c(252:262),]
tidy_mod.factor <- tidy_mod.factor %>%
  mutate(calorie=case_when(group==0 ~ coef.month+intercept,
                           group==1&month<=0 ~ coef.month+treat+intercept,
                           group==1&month>0 ~ coef.month+treat+ml+coef.ml+intercept))
summary(tidy_mod.factor$calorie)
tidy_mod.factor$coef.ml[1:131] <- tidy_mod.factor$coef.month[1:131]

#visualize coef
ggplot(data=tidy_mod.factor,aes(x=month, y=calorie,group=as.character(group), color=as.character(group))) + #
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 0, color="grey", linetype="dashed", size=1) +
  ggplot2::annotate(geom="label", x=5, y=1700, label="Ref group", size=3) + #add label for ML
  #geom_hline(yintercept = 0, color="grey", linetype="solid", size=0.5) + #add line for ref group
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(900,1900)) + #
  scale_y_continuous(breaks=seq(900,1900,100)) +
  scale_x_continuous(breaks=seq(-49,81,5)) +
  labs(title="Effect of menu labeling on calories purchased", x="Month", y="Calories",
       caption="All coefficients set the reference group as month=0 (grey dashed line), the month immediately before labeling began. \n No estimates past month 71 as there was only one restaurant.") + #
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/coef-month-interaction-in-calories.jpeg", dpi="retina")
















