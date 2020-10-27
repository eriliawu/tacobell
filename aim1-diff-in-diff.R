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
hist(unmatched$monthno-203, breaks = 50, xlim = c(0,106),
     main = "Distribution of data availability Overall",
     xlab = "Continuous month, t0 = December 2006")

# simple diff-in-diff
mod1.unmatched <- lm(formula = calorie~ml+ml*monthno+monthno+
             holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
             median_income+capital_income+hsbelow+collegeup+under18+above65,
           data = unmatched)
summary(mod1.unmatched)

# diff-in-diff, with fixed effect at restaurant level
mod2.unmatched <- plm(formula = calorie~ml+ml*monthno+monthno+holiday,
            data = pdata.frame(unmatched, index = c("id")),
            model = "within")
summary(mod2.unmatched)

# diff-in-diff, random effect at restaurant level
mod3.unmatched <- lme4::lmer(formula = calorie~ml+ml*monthno+monthno+
                         holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
                         median_income+capital_income+hsbelow+collegeup+under18+above65+(1|id),
                   data=unmatched)
summary(mod3.unmatched)

stargazer(mod1.unmatched, mod2.unmatched, mod3.unmatched,
          type="html",
          title="Unmatched group",
          dep.var.labels="Mean calorie per order",
          #dep.var.labels.include = FALSE,
          column.labels = c("Simple", "Fixed effect", "Random effect"),
          colnames = FALSE,
          keep = c("ml", "monthno", "ml:monthno"),
          covariate.labels=c("Menu labeling", "Month", "Menu labeling*month"),
          #omit.stat = c("rsq", "ser"),
          keep.stat = c("n","adj.rsq"),
          #notes = "All models control for whether the purchase occurred in a holiday season, <\br>
          #restaurant (owned by Taco Bell, has drive through, has other brands on-site) and community level characteristics (at census tract level population density, % male, % Black, % Asian, % Hispanic, % White, household median income, income per capita, % without high school degree, % with bachelor degree or higher, % under 18, % above 65).",
          out="tables/analytic-model/aim1-diff-in-diff/aim1-unmacthde.html")


# by subgroups, estimate ML=0/1 groups separately
ggplot(data=unmatched, aes(x=monthno, y=calorie,
                           group=paste0(treat,ml), color=paste0(treat,ml))) +
  geom_point(size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
  stat_summary(aes(y=calorie,group=paste0(treat,ml),color=paste0(treat,ml)),
               fun.y=mean, geom="point") + #insert monthly mean as scatter plots
  #geom_smooth(method='lm') + #add best fitted lines
  #ggplot2::annotate(geom="label", x=22, y=1750, label="Monthly decrease \n of 0.13 kcal", size=3) + #add pre-ML trend label
  #ggplot2::annotate(geom="label", x=82, y=1750, label="Monthly decrease \n of 1.21 kcal***", size=3) + #add post-ML trend label
  #ggplot2::annotate(geom="text", x=1:106, y=-50, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
  #ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=-150, label=unique(unmatched$year), size=4) + #year
  coord_cartesian(ylim=c(0, 2500), expand = FALSE, clip = "off") + 
  #scale_y_continuous(breaks=seq(0, 2500, 250)) +
  labs(title="Calories per order trend", x="Time", y="Calories",
       caption="") +
  scale_color_discrete(name="Menu Lableing", labels=c("Never labeled", "Pre-labeling", "Post-labeling")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
        axis.text.x = element_blank(), #turn off default x axis label
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/california-pre-post-ml.jpeg", dpi="retina")


# add adjustment period, 2 months before and after ML
interaction(year, month, lex.order = TRUE)

### matched data, by ps matching and iptw weighting ----
matched <- read.csv("data/calorie-aims/matched-restaurants.csv", stringsAsFactors = FALSE)
matched$tract_num <- substr(matched$tract_num, 2, 12)
matched$holiday <- ifelse(matched$month==12, 1, 0)
matched <- matched %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:holiday)

# get relative month for pre-/post-labeling
matched$relative <- matched$monthno - matched$match_to
hist(matched$relative, breaks = 50, xlim = c(-60, 80),
     main = "Distribution of data availability pre- and post-labeling",
     xlab = "Relative month, the month of labeling=t0")

# simple diff-in-diff
mod1.matched <- lm(formula = calorie~ml+ml*relative+year+
             month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
             median_income+capital_income+hsbelow+collegeup+under18+above65,
           data = matched, weights = weights)
summary(mod1.matched)

# diff-in-diff, with fixed effect at restaurant level
mod2.matched <- plm(formula = calorie~ml+ml*relative+year+month+holiday,
            data = pdata.frame(matched, index = c("id")),
            model = "within")
summary(mod2.matched)

# diff-in-diff, random effect at restaurant level
mod3.matched <- lme4::lmer(formula = calorie~ml+ml*relative+year+
               month+holiday+concept+drive_thru+ownership+total+male+white+black+asian+hisp+
               median_income+capital_income+hsbelow+collegeup+under18+above65+(1|id), data=matched,
               weights = weights)
summary(mod3.matched)

stargazer(mod1.unmatched, mod2.unmatched, mod3.unmatched,
  mod1.matched, mod2.matched, mod3.matched,
  type="html",
  title="The effect of menu labeling on calories purchased",
  dep.var.labels="Mean calories per order",
  #dep.var.labels.include = FALSE,
  column.labels = c("<b> Unmatched </b>", "<b> Matched </b>"), #bold
  column.separate = c(3, 2),
  model.names = FALSE,
  keep = c("ml","monthno", "relative", "ml:monthno", "ml:relative"),
  covariate.labels=c("Menu labeling", "Month","Relative month", "Menu labeling*month", "Menu labeling*relative month"),
  keep.stat = c("n","adj.rsq"),
  add.lines = list(c("Model", "<em>Diff-in-diff</em>", "<em>FE</em>", "<em>RE</em>","<em>Diff-in-diff</em>", "<em>FE</em>", "<em>RE</em>")),
  notes = c("Month is presetned as a continuous variable in unmacthed models. ",
            "In all models, we control for purchase characteristics (holiday season); ",
            "restaurant characteristics: owned by Taco Bell, has drive through, has other brands on-site; ",
            "and community characteristics: at census tract level population density, ",
            "% male, % Black, % Asian, % Hispanic, % White, household median income, ",
            "income per capita, % without high school degree, % with bachelor degree or higher, ",
            "% under 18, % above 65. Additionally, for matched models, we control ",
            "for calendar year and month."),
  notes.align = "l",
  out="tables/analytic-model/aim1-diff-in-diff/aim1.html")




