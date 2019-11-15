### match menu items from tacobell and menustat
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

#install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)
#install.packages("stringdist")
library(stringdist)

# read product data ----
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
table(product$group)

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW", product$group)&!grepl("BYB", product$group)&!grepl("KFC", product$group)&!grepl("LJS", product$group)&!grepl("PH", product$group)&!grepl("PIZZA HUT", product$group)&product$group!="KRYSTAL"&product$group!="ICBIY (YOGURT)"&product$group!="TCBY (YOGURT)", ]
product <- product[!grepl("AWR", product$product)&!grepl("AW,", product$product)&!grepl("AW ", product$product)&!grepl("BYB", product$product)&!grepl("KFC", product$product)&!grepl("LJS", product$product)&!grepl("PH", product$product)&!grepl("PIZZA HUT", product$product)&!grepl("TCBY", product$product)&!grepl("ICBIY", product$product)&!grepl("KRYSTAL", product$product), ]
length(unique(product$product))

# drop non-descriptive items
product <- product[product$group!="N/A"&product$group!="CFM MANAGER SPECIALS"&product$group!="COMBOS"&product$product!=""&!grepl("* NEW PRODCT ADDED BY", product$product)&!grepl("COMBO", product$product)&!grepl("FRANCHISE LOCAL MENU", product$product)&product$product!="NEW ITEM"&!grepl("SPECIAL PROMOTION", product$product), ]
length(unique(product$product))

# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]
length(unique(product$product))

#length(unique(product$product))
names(product)
product <- product[, c(4, 9, 1:2, 3, 5:8, 10:12)]

# extract only unique product names
product <- product[!duplicated(product$product), c(1:2)]

### read menu stat data
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
menu$item_name <- toupper(menu$item_name)
menu <- menu[!duplicated(menu$item_name), c(1, 4:5)]
length(unique(menu$item_name))
#menu <- menu[menu$year<=2016, ]

### find exact matches ----
test <- merge(product, menu, by.x="product", by.y="item_name") #64 exact matches
rm(test)

### fuzzy matching ----
colnames(menu)[2] <- "product"

start_time <- Sys.time()
join <- stringdist_join(product, menu, 
                by="product",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist.jw") %>%
      group_by(product.x) %>%
      top_n(1, -dist.jw)
end_time <- Sys.time()
end_time - start_time
rm(start_time, end_time)

names(join)
join <- join[, c(1, 4, 6, 2, 5, 3)]
colnames(join)[1:2] <- c("product.tb", "product.menustat")
join <- join[order(join$dist.jw, join$product.tb), ]

### analyze matching results ----
length(join$dist.jw[join$dist.jw==0])
jpeg("tables/product-matching/jw-distance-distribution.jpeg",
     width=600, height=400, quality=100)
hist(join$dist.jw, breaks=50,
     xlab="Distance",
     main="Distribution of distances")
dev.off()



