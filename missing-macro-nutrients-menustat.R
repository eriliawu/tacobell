### assess missing data in macro nutrients in menustat

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
library(stargazer)
library(table1)
library(tableone)
library(broom)
library(car)
library(zoo)

### read data ----