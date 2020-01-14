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
#install.packages("ggmap")
library(ggmap)
#install.packages("maps")
library(maps)

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

### read pricing data by quarter, merge with restaurant and product data ----
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

# merge with restaurant address, product name
price <- merge(price, sample[, c("dw_product", "product")], by="dw_product")
price <- merge(price, restaurants[, c(c(1:9, 23:24))], by="restid")
names(price)

# see what stores showed consistent sales of items
# create duplicate tags for each unique address
price <- price %>%
      group_by(address, dw_product) %>%
      mutate(count=n()) %>%
      mutate(rank <- seq(1, count[1], 1))
colnames(price)[18] <- "dup"

### cross-time analysis, the same restaurant/area ----
summary(price$count)

# why some restaurants show up more than the max 35 quarters
table(price$address[price$count>=36])
temp <- price[price$count>=36, ]
temp <- temp[order(temp$state, temp$address, temp$year, temp$quarter), ]
# sometimes restaurants changed hands during a quarter
# that restaurant would show up more than the max of 25 quarters in the data
rm(temp)

# restaurants consistently selling bean burrito
length(unique(price$address[price$count==35&price$dw_product==802])) #686

# in new york city
max(price$count[price$dw_product==802&price$state=="NY"&
                      (price$county=="New York"|price$county=="Bronx"|
                             price$county=="Kings"|price$county=="Queens"|
                             price$county=="Richmond")]) #32

# restaurants consistently selling medium pepsi
max(price$count[price$dw_product==3139]) #29

# bean burrito sales in new york city
table(price$county[price$count==35&price$dw_product==802&
                       price$year==2015&price$quarter==1&
                       price$state=="NY"]) #flushing

table(price$address[price$dw_product==802&
                         price$year==2015&price$quarter==1&
                         price$state=="NY"&
                         (price$county=="New York"|price$county=="Bronx"|
                                price$county=="Kings"|price$county=="Queens"|
                                price$county=="Richmond")])

# price of a bean burrito over time, in Flushing ----
ggplot(data=subset(price, address %in% "172-12 Northern Blvd., Flushing, NY 11358"),
       aes(x=paste0(year, "Q", quarter),
           y=price,
           group=as.factor(product), col=as.factor(product))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Price of bean burrito and medium Pepsi, Flushing, Queens",
           x="Time", y="Price",
           col="Product", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      #scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/diff-pricing/price-change-flushing.jpeg", width=20, height=10, unit="cm")

# price of bean burrito and pepsi over time in queens county ----
ggplot(data=subset(price, (state %in% "NY" & county %in% "Queens" & 
                                 dw_product %in% c(802, 4098, 37956))),
       aes(x=paste0(year, "Q", quarter),
           y=price,
           group=as.factor(address), col=as.factor(address))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Price of bean burrito, Queens",
           x="Time", y="Price",
           col="Address", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      #scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/diff-pricing/price-change-burrito-queens.jpeg", width=20, height=10, unit="cm")

ggplot(data=subset(price, (state %in% "NY" & county %in% "Queens" & 
                                 dw_product %in% c(3139, 3648))),
       aes(x=paste0(year, "Q", quarter),
           y=price,
           group=as.factor(address), col=as.factor(address))) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Price of medium Pepsi, Queens",
           x="Time", y="Price",
           col="Address", caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      #scale_y_continuous(limits=c(0, 5000)) +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/diff-pricing/price-change-pepsi-queens.jpeg", width=20, height=10, unit="cm")

### price of bean burrito and pepsi across regions, 2010Q2 ----
us <- map_data("state")
ggplot() +
      coord_fixed() +
      geom_polygon(data=us,
                   aes(x=long, y=lat, group=group),
                   color="black", fill="lightblue", size=0.1) +
      geom_point(data=subset(price, year==2015&quarter==1&(product=="BEAN BURRITO")&state!="AK"&
                                   (price==0.99|price==1.09|price==1.19|
                                          price==1.29|price==1.39|price==1.49|
                                          price==1.59|price==1.69|price==1.99)), 
                 aes(x=lon, y=lat, color=as.character(price)), size=0.5) +
      labs(title="Price of bean burrito, 2015Q1", x="", y="", col="Price",
           caption="Note: pricing information exclude Alaska and Hawaii.") +
      scale_color_brewer(palette="YlOrRd") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = 'white', colour = 'white'), 
            axis.line = element_line(colour = "white"),
            axis.ticks=element_blank(), axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/diff-pricing/burrito-price_2015q1.jpeg", width=20, height=10, unit="cm")

ggplot() +
      coord_fixed() +
      geom_polygon(data=us,
                   aes(x=long, y=lat, group=group),
                   color="black", fill="lightblue", size=0.1) +
      geom_point(data=subset(price, year==2015&quarter==1&(product=="MEDIUM PEPSI")&state!="AK"&
                                   (price==1.39|price==1.49|
                                          price==1.59|price==1.69|price==1.79|
                                          price==1.85|price==1.89|price==1.99|
                                          price==2.09)), 
                 aes(x=lon, y=lat, color=as.character(price)), size=0.5) +
      labs(title="Price of medium Pepsi, 2015Q1", x="", y="", col="Price",
           caption="Note: pricing information exclude Alaska and Hawaii.") +
      scale_color_brewer(palette="YlOrRd") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = 'white', colour = 'white'), 
            axis.line = element_line(colour = "white"),
            axis.ticks=element_blank(), axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/diff-pricing/pepsi-price_2015q1.jpeg", width=20, height=10, unit="cm")
rm(sample, us, restaurants, price, product)




