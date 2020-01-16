### using key words to predict calorie info
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
#install.packages("lfe")
library(lfe)
#install.packages("broom") #diagnostic plots for lienar regression
library(broom)
install.packages("tidyverse")
library(tidyverse)
library(stargazer)

### read menustat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
names(menu)
menu$item_name <- tolower(menu$item_name)

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

### clean data and re-construct ----
# decide what variables should be in the model
# category (taco, salad, etc), year, ingredient (chicken, beef, etc)
table(menu$category)

# recode food item categories
# taco, chalupa, gordita
# burrito, quesarito, griller, crunchwrap
# quesadilla
# nacho
# salad
# cinnamon twist, cinnabon delight
# bowl
menu$cat <- ifelse(grepl("salad", menu$item_name), "salad",
                   ifelse(grepl("taco|chalupa|gordita|tostada", menu$item_name),
                          "taco",
                          ifelse(grepl("burrito|quesarito|griller|crunchwrap|enchirito|taquito",
                                       menu$item_name), "burrito",
                                 ifelse(grepl("quesadilla|flatbread|pizza|doubledilla|meximelt",
                                              menu$item_name), "quesadilla",
                                        ifelse(grepl("nacho", menu$item_name), "nacho",
                                               ifelse(grepl("cinnamon|cinnabon|cookie|
                                                            brownie|churro|caramel apple empanada",
                                                            menu$item_name), "dessert",
                                                      ifelse(menu$category=="Beverages"|
                                                                   grepl("beverages", menu$item_name), "drink",
                                                             ifelse(grepl("bowl", menu$item_name), "bowl",
                                                                    ifelse(grepl("sauce|dressing|
                                                                                 salsa", menu$item_name),
                                                                           "sauce", "other")))))))))
table(menu$cat)

# find key ingredient
# chicken, steak, beef, cheese, rice
menu$chicken <- ifelse(grepl("chicken", menu$item_name), 1, 0)
menu$steak <- ifelse(grepl("steak", menu$item_name), 1, 0)
menu$beef <- ifelse(grepl("beef", menu$item_name), 1, 0)
menu$cheese <- ifelse(grepl("cheese|cheesy", menu$item_name), 1, 0)
menu$rice <- ifelse(grepl("rice", menu$item_name), 1, 0)
menu$bean <- ifelse(grepl("bean", menu$item_name), 1, 0)

### regression ----
mod1 <- lm(calories~cat+as.character(year),
             data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
summary(mod1)
par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))

mod2 <- lm(calories~cat+chicken+beef+steak+as.character(year),
           data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
summary(mod2)
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

mod3 <- lm(calories~ca+chicken+beef+steak+cheese+rice+beant+as.character(year),
           data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
summary(mod3)
par(mfrow = c(2, 2))
plot(mod3)
par(mfrow = c(1, 1))

# make regression table
mod1 <- felm(calories~cat|year,
             data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
mod2 <- felm(calories~cat+chicken+beef+steak|year,
             data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
mod3 <- felm(calories~cat+chicken+beef+steak+cheese+rice+bean|year,
             data=subset(menu, cat!="drink"&cat!="other"&calories<1000))
stargazer(mod1, mod2, mod3, type="html",
          title="Regression",
          dep.var.labels="Calories",
          covariate.labels=c("Burrito", "Dessert", "Nacho", "Quesadilla", "Salad", "Sauce", "Taco",
                             "Chicken", "Beef", "Steak", "Cheese", "Rice", "Bean"),
          out="tables/calorie-predicting/regression-table.html")


res1 <- augment(mod1)
ggplot(res, aes(x=.fitted, y=.resid)) +
      geom_point()
qplot(data=subset(menu, cat=="taco"&calories<1000), x=as.character(year), y=calories)

### calorie trend ----
# whether a category of food changes calories from year to year significantly
trend <- aggregate(calories~year+cat, subset(menu, cat!="other"&calories<1000), FUN=mean)
ggplot(data=trend,
       aes(x=as.character(year), y=calories,
           group=as.factor(cat), col=as.factor(cat))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean calories over time, by category",
           x="Year", y="Calories", col="Category",
           caption="Data exclusion: items in beverages and other category; items with over 1,000 calories.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/menustat-menu-item-analysis/mean-cal-by-cat.jpeg", width=20, height=10, unit="cm")

trend <- trend %>%
      group_by(cat) %>%
      arrange(year, .by_group=TRUE) %>%
      mutate(pct_change = (calories/lag(calories))-1)
trend$pct_change[is.na(trend$pct_change)] <- 0

ggplot(data=trend,
       aes(x=as.character(year), y=pct_change,
           group=as.factor(cat), col=as.factor(cat))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(-1, 1.6)) +
      labs(title="Percent of mean calories over time, by category",
           x="Year", y="Percentage", col="Category",
           caption="Data exclusion: items in beverages and other category; items with over 1,000 calories.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/menustat-menu-item-analysis/mean-cal-by-cat-pct.jpeg", width=20, height=10, unit="cm")



