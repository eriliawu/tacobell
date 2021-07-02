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
library(lme4)
library(plm)
library(lmerTest)
library(stargazer)
library(table1)
library(tableone)
library(broom)
library(car)
library(usmap)
library(maps)
library(car) #testing joint significance
library(zoo)

### read menustat data, figure out which items were consistenyl offered ----
menu <- read.csv("data/menustat/nutrition_info_all.csv",stringsAsFactors = FALSE)

#use item id to identify the same items
#clean up strings in the item names that unlikely appear in taco bell menu item names
menu <- menu %>% dplyr::select(id,year,item_name,calories) %>%
  filter(year<=2015) %>%
  arrange(id,year) %>% group_by(id) %>%
  mutate(n=n()) %>% mutate(rank=row_number()) %>% filter(n==6) %>% ungroup() %>%
  mutate(item_name = gsub(",|w/|Chips|Why Pay More Value Menu|Fresco Menu","",item_name)) %>%
  dplyr::select(item_name) %>%
  mutate(item_name = gsub("\\s+", " ", item_name)) %>%
  mutate(item_name=trimws(item_name)) %>% distinct()

### read sales data for all 35 quarters ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale/consistent-for-sale-item_2007_Q1.csv",
                       stringsAsFactors = FALSE, col.names = c("product"))
sample07q1$year <- 2007
sample07q1$quarter <- 1
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/consistent-sale/consistent-for-sale-item_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE, col.names=c("product"))
        sample$year <- i
        sample$quarter <- j
        sales <- rbind(sales, sample)
      }, error=function(e){cat("ERROR:",conditionMessage(e), "\n")}
    )
  }
}
sales <- rbind(sales, sample07q1)
rm(sample, sample07q1, i, j)

# merge with product names
product <- read.csv("data/from-bigpurple/product_detail.csv",stringsAsFactors = FALSE)
sales <- merge(sales,product,by.x = "product",by.y = "p_detail",all.x = TRUE)

# identify items consistently for sale thru all 35 quarters
sales <- sales %>% rename(DW_PRODUCT=product,product=detail_desc) %>%
  group_by(product) %>% arrange(product,year,quarter) %>%
  mutate(n=n()) %>% mutate(max_year=max(year)) %>% mutate(min_year=min(year)) %>%
  filter(max_year==2015&min_year==2007&n>=30) %>% ungroup() %>%
  dplyr::select(DW_PRODUCT) %>% distinct() 
#write.csv(sales,"data/upload-to-bigpurple/consistent-sales.csv",row.names = FALSE)