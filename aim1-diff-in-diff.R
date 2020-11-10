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
             year+month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
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

### matched data, by ps matching and iptw weighting ----
matched <- read.csv("data/calorie-aims/matched-restaurants.csv", stringsAsFactors = FALSE)
matched$tract_num <- substr(matched$tract_num, 2, 12)
matched$holiday <- ifelse(matched$month==12, 1, 0)
matched <- matched %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday)

# get relative month for pre-/post-labeling
matched$relative <- matched$monthno - matched$match_to +1

# simple diff-in-diff
mod1.matched <- lm(formula = calorie~ml+
                     year+month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                     median_income+capital_income+hsbelow+collegeup+under18+above65,
                   data = matched, weights = weights)
summary(mod1.matched)

# month as relative and continuous
mod2.matched <- lm(formula = calorie~ml+ml*relative+
                     year+month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
             median_income+capital_income+hsbelow+collegeup+under18+above65,
             data = matched, weights = weights)
summary(mod2.matched)

# diff-in-diff, with fixed effect at restaurant level
mod3.matched <- plm(formula = calorie~ml+ml*relative+year+month+holiday,
            data = pdata.frame(matched, index = c("id")), weights = weights,
            model = "within")
summary(mod3.matched)

# diff-in-diff, random effect at restaurant level
mod4.matched <- lme4::lmer(formula = calorie~ml+ml*relative+
                             year+month+holiday+concept+drive_thru+ownership+total+
                             male+white+black+asian+hisp+median_income+capital_income+
                             hsbelow+collegeup+under18+above65+(1|id),
                           data=matched, weights = weights)
summary(mod4.matched)

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
                            year+month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                            median_income+capital_income+hsbelow+collegeup+under18+above65,
                          data = unmatched.comp)
summary(mod1.unmatched.comp)

# month as relative and continuous var
mod2.unmatched.comp <- lm(formula = calorie~ml+ml*relative+year+
                     month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                     median_income+capital_income+hsbelow+collegeup+under18+above65,
                   data = unmatched.comp)
summary(mod2.unmatched.comp)

# diff-in-diff, with fixed effect at restaurant level
mod3.unmatched.comp <- plm(formula = calorie~ml+ml*relative+relative+year+month+holiday,
                    data = pdata.frame(unmatched.comp, index = c("id")),
                    model = "within")
summary(mod3.unmatched.comp)

# diff-in-diff, random effect at restaurant level
mod4.unmatched.comp <- lme4::lmer(formula = calorie~ml+ml*relative+relative+
                                    year+month+holiday+concept+drive_thru+ownership+
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
  geom_point(data=unmatched %>% filter (calorie>=500 & calorie<=2000), size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=as.character(treat*entry),color=as.character(treat*entry)),
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
  scale_color_discrete(name="Menu Lableing",
                       labels=c("No", "Nassau county", "Mont/Mult", "MA", "CA/OR", "NJ/ME")) +
  #scale_color_brewer(palette = "Set3") +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/calories-trend-by-ml-CA-OR-only.jpeg", dpi="retina")

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
                    dplyr::select(calorie, dollar, count, drive, treat, year, month) %>%
                    mutate(data="unmatched"),
                  matched %>%
                    dplyr::select(calorie, dollar, count, drive, treat, year, month) %>%
                    mutate(data="matched"))
summary <- summary %>%
  filter(year==2015&month==1)
            
summary$treat <- factor(summary$treat, levels=c(1, 0),
                          labels = c("Yes", "No"))
label(summary$calorie) <- "Calorie"
label(summary$dollar) <- "Mean spending per order"
label(summary$count) <- "Total # of transactions"
label(summary$drive) <- "% sales from drive-through"

table1::table1(data=summary %>% filter(data=="matched"),
       ~calorie+dollar+count+drive|treat,
       caption="<b>Summary statistics, matched data, January 2015</b>")














