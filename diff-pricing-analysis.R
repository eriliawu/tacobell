### differential pricing
# same restaurant, across time
# same time, across cities/regions
# items: medium pepsi, bean burrito

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

### read restaurant data ----
restaurants <- read.csv("data/restaurants/analytic_restaurants.csv",
                        stringsAsFactors = FALSE)
names(restaurants)

### read product data ----
product <- read.csv("data/from-bigpurple/product_dim.csv",
                    sep = ";", header = FALSE, quote = "\"'",
                    stringsAsFactors = FALSE,
                    col.names = c("dw_product", "dw_productgroup", "productcd",
                                  "product", "product_statuscd",
                                  "product_statusdt", "product_lastupdt", "lastupdtuserid"))
sapply(product, class)
product$lastupdtuserid <- NULL

group <- read.csv("data/from-bigpurple/product_group_det.csv",
                  sep = ";", header = FALSE, quote = "\"'",
                  stringsAsFactors = FALSE,
                  col.names = c("dw_productgroup", "groupcd", "group",
                                "groups_tatuscd",
                                "group_statusdt", "group_lastupdt", "lastupdtuserid"))
group$lastupdtuserid <- NULL

product <- merge(product, group, by="dw_productgroup")
rm(group)

### clean house ----
# drop commas and other punctuations
product$product <- gsub(pattern = ",", replacement = "", x=product$product)

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW", product$group)&!grepl("BYB", product$group)&
                         !grepl("KFC", product$group)&!grepl("LJS", product$group)&
                         !grepl("PH", product$group)&!grepl("PIZZA HUT", product$group)&
                         product$group!="KRYSTAL"&product$group!="ICBIY (YOGURT)"&
                         product$group!="TCBY (YOGURT)", ]
product <- product[!grepl("AWR", product$product)&!grepl("AW ", product$product)&
                         !grepl("BYB", product$product)&!grepl("KFC", product$product)&
                         !grepl("LJS", product$product)&!grepl("PH", product$product)&
                         !grepl("PIZZA HUT", product$product)&!grepl("TCBY", product$product)&
                         !grepl("ICBIY", product$product)&!grepl("KRYSTAL", product$product), ]

# drop non-descriptive items
product <- product[product$group!="N/A"&product$group!="CFM MANAGER SPECIALS"&
                         product$group!="COMBOS"&product$product!=""&
                         !grepl("* NEW PRODCT ADDED BY", product$product)&
                         !grepl("COMBO", product$product)&
                         !grepl("FRANCHISE LOCAL MENU", product$product)&
                         product$product!="NEW ITEM"&!grepl("SPECIAL PROMOTION", product$product), ]
product <- product[product$product!="TB I'M ALL EARS"&product$product!="SPECIAL"&
                         product$product!="DO NOT ALTER THIS ITEM"&
                         product$product!="BORDER SWEAT SHIRT"&
                         product$product!="TB I'M THINKING YOU ME"&
                         product$product!="CFM DOWNLOAD 1"&
                         product$product!="TB HELLO FRIEND"&
                         product$product!="CANADA BATMAN CUP INDIVIDUAL"&
                         product$product!="DELETED ITEM, DO NOT USE"&
                         product$product!="CLEV INDIANS/TB BANDANNA 1.4"&
                         product$product!="CFM DOWNLOAD 2"&
                         product$product!="TB I'M THINKING YOU ME DINNER"&
                         product$product!="CANADA BATMAN CUP W/PURCHASE"&
                         product$product!="TB HELLO FRIEND", ]

# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]

length(product$dw_product[product$product=="MEDIUM PEPSI"])
pepsi <- product[product$product=="MEDIUM PEPSI"&product$product_statuscd=="A", ]
length(product$dw_product[product$product=="BEAN BURRITO"])
bean <- product[product$product=="BEAN BURRITO"&product$group=="BURRITO"&product$product_statuscd=="A", ]

sample <- rbind(pepsi, bean)
rm(pepsi, bean)
### run queries in database to find records ----
# see diff-pricing.R script on HPC

### across-city analysis, 2010 Q2
# merge sales data with restaurants
same.time <- read.csv("data/from-bigpurple/diff-pricing/sales_pepsi_burrito_2010Q2.csv",
                      stringsAsFactors = FALSE,
                      col.names = c("restid", "product", "price"))

same.time <- merge(same.time, restaurants, by="restid")
same.time <- merge(same.time, sample, by.x="product", by.y="dw_product")
names(same.time)
same.time <- same.time[order(same.time$product, same.time$state, same.time$address),
                       c(1:5, 25:26, 33)]
colnames(same.time)[c(1, 8)] <- c("dw_product", "product")

# look at distribution
table(same.time$price[same.time$dw_product==802])
table(same.time$price[same.time$dw_product==4098])
table(same.time$price[same.time$dw_product==37956]) #none returned
table(same.time$price[same.time$dw_product==3139])
table(same.time$price[same.time$dw_product==3648])
#bean burrito: 802; medium pepsi: 3139

same.time[same.time$state=="NY"&same.time$dw_product==802&
                (same.time$county=="New York"|same.time$county=="Bronx"|
                       same.time$county=="Kings"|same.time$county=="Queens"|
                       same.time$county=="Richmond"), c(3:4, 6)]

same.time[same.time$state=="NY"&same.time$dw_product==3139, c(3:4, 6)]








