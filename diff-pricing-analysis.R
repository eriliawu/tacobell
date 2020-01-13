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

### cross-time analysis ----
# see diff-pricing.R script on HPC 
price <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                        sales <- read.csv(paste0("data/from-bigpurple/diff-pricing/pepsi_burrito_sales_",
                                                 i, "_Q0", j, ".csv"),
                                          stringsAsFactors = FALSE)
                        sales$year <- i
                        sales$quarter <- j
                        price <- rbind(price, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
                        )
      }
}
rm(i, j, sales)
names(price)
colnames(price)[1:3] <- c("restid", "dw_product", "price")

# see what stores showed consistent sales of items
# create duplicate tags for each unique address
price <- price %>%
      group_by(restid, dw_product) %>%
      mutate(count=n()) %>%
      mutate(rank <- seq(1, count[1], 1))
colnames(price)[7] <- "dup"
summary(price$count)
length(unique(price$address[price$count==35&price$dw_product==802])) #646
max(price$count[price$dw_product==802&price$state=="NY"&
                      (price$county=="New York"|price$county=="Bronx"|
                             price$county=="Kings"|price$county=="Queens"|
                             price$county=="Richmond")]) #32

max(price$count[price$dw_product==3139]) #27

# merge restid with restaurant data
price <- merge(price, restaurants, by="restid")
names(price)
price <- price[, c(1:13, 29:30)]

table(price$county[price$count==35&price$dw_product==802&
                       price$year==2015&price$quarter==1&
                       price$state=="NY"])
table(price$county[price$count==32&price$dw_product==802&
                         price$year==2015&price$quarter==1&
                         price$state=="NY"&
                         (price$county=="New York"|price$county=="Bronx"|
                                price$county=="Kings"|price$county=="Queens"|
                                price$county=="Richmond")])

# price of a bean burrito over time, in Flushing
flushing <- price[price$address=="172-12 Northern Blvd., Flushing, NY 11358", ]
ggplot(data=flushing, aes(x=paste0(year, "Q", quarter),
                        y=price,
                        group=as.factor(dw_product), col=as.factor(dw_product))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Price of bean burrito and medium pepsi",
           x="Time", y="Price",
           col="Product", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      #scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))










