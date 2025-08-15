### assess missing data in macro nutrients in menustat

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
library(stargazer)
library(table1)
library(tableone)
library(broom)
library(car)
library(zoo)

### read data ----
total <- read.csv("data/from-bigpurple/missingness-nutrients/total-qty.csv")
for (i in c("carb","fat","protein","sodium")) {
  tmp <- read.csv(paste0("data/from-bigpurple/missingness-nutrients/",i,"-qty.csv")) 
  total <- merge(total,tmp,by=c("year","quarter"))
}
colnames(total)[4:7] <- c("carb","fat","protein","sodium")
total[,4:7] <- total[,4:7]/total$total_qty
summary(total[,4:7])