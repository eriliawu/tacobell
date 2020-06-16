### build table for nutrition and calorie information
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
#install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)
#install.packages("stringdist")
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)
#install.packages("tm") #text mining
#install.packages("SnowballC") #text stemming
#install.packages("wordcloud") #generate word cloud
#install.packages("RColorBrewer")
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

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
table(product$group)

# drop commas and other punctuations
product$product <- gsub(pattern = ",", replacement = "", x=product$product)
length(unique(product$product)) #9006

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW", product$group)&!grepl("BYB", product$group)&
                         !grepl("KFC", product$group)&!grepl("LJS", product$group)&
                         !grepl("PH", product$group)&!grepl("PIZZA HUT", product$group)&
                         product$group!="KRYSTAL"&product$group!="ICBIY (YOGURT)"&
                         product$group!="TCBY (YOGURT)", ]
product <- product[!grepl("AWR", product$product)&!grepl("AW ", product$product)&
                         !grepl("BYB", product$product)&!grepl("KFC", product$product)&
                         !grepl("LJS", product$product)&!grepl("PH ", product$product)&
                         !grepl("PIZZA HUT", product$product)&!grepl("TCBY", product$product)&
                         !grepl("ICBIY", product$product)&!grepl("KRYSTAL", product$product), ]
length(unique(product$product)) #4800

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
length(unique(product$product)) #3658

# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]
length(unique(product$product)) #3599

#length(unique(product$product))
names(product)
product <- product[, c(4, 9, 1:2, 3, 5:8, 10:12)]

# extract only unique product names
product <- product[!duplicated(product$product), c(1:2)]
#write.csv(product, "data/menu-matching/full_product_names.csv", row.names = FALSE)

### extract all substrings in product names to fill out abbreviations ----
# extract all substrings
strings <- as.data.frame(unlist(strsplit(product$product, split=" ")))
colnames(strings)[1] <- "original"
class(strings$original)
strings$original <- as.character(strings$original)

# measure substring length
strings$length <- nchar(strings$original)
strings <- strings[order(strings$original, strings$length),]

# frequency, how often does a substring show up in product name
strings <- strings %>%
      group_by(original) %>%
      mutate(count=n())
#mutate(rank <- seq(1, count[1], 1))
strings <- strings[!duplicated(strings), ]
strings <- strings[order(strings$count, decreasing = TRUE), ]

# export to fill out abbreviations
#write.csv(strings, "data/menu-matching/product-names_unique_substrings.csv", row.names = FALSE)

# incorporate full names
strings <- read.csv("data/menu-matching/product-names_unique_substrings_w_correction.csv",
                    stringsAsFactors = FALSE)
sapply(strings, class)

# replace all unchanged strings with its original value
strings$full[strings$full==""] <- strings$original[strings$full==""]
strings$length <- NULL

strings$original[strings$original=="02-Jan"] <- "1/2"
strings$original[strings$original=="03-Jan"] <- "1/3"
strings$original[strings$original=="04-Jan"] <- "1/4"
strings$original[strings$original=="06-Jan"] <- "1/6"
strings <- strings[!duplicated(strings$original), ]

# replace abbreviations with full spelling
# first, break product names into separate substrings in their own columns
# second, merge each column with the replacement strings
product <- product %>%
      separate(product, c("product1", "product2", "product3", "product4", "product5", "product6", "product7", "product8"), " ")

for (i in c(1:8)) {
      product <- merge(product, strings, by.x=paste0("product", i), by.y="original", sort=FALSE, all.x = TRUE)
      product$count <- NULL
      colnames(product)[i+9] <- paste0("full", i)
}
rm(i, strings)

# paste all substrings, but leave out the NAs
product$product <- apply(cbind(product$product1, product$product2, product$product3,
                               product$product4, product$product5, product$product6,
                               product$product7, product$product8),
                         1, function(x) paste(x[!is.na(x)], collapse = " "))
product$full <- apply(cbind(product$full1, product$full2, product$full3, product$full4,
                            product$full5, product$full6, product$full7, product$full8),
                      1, function(x) paste(x[!is.na(x)], collapse = " "))

names(product)
product <- product[, c(18:19, 9)]
product <- product[!duplicated(product), ]

# drop key words from product names: TEST, (), DO NOT ALTER THIS ITEM
product$full <- gsub("STEAK LOUIS", replacement = "ST LOUIS", product$full)
product$full <- gsub("TEST", "", product$full)
product$full <- gsub("\\(", "", product$full)
product$full <- gsub("\\)", "", product$full)
product$full <- gsub("TB ", "", product$full)
product <- product[product$full!="", ]

length(unique(product$full))


### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
menu$item_name <- toupper(menu$item_name)
names(menu)

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

### import matched items with nutritional information ----
match <- read.csv("data/menu-matching/manual-match/matching-results-full-list_menustat.csv",
                  stringsAsFactors = FALSE) #read items to be matched with menustat
match <- match[!duplicated(match), 1:3]

# merge with menustat data
match <- merge(match, menu, by.x="menustat.name", by.y="item_name", all.x=TRUE)
names(match)
match <- match[order(match$tacobell.name, match$year), c(2, 6, 1, 3:5, 7:24)]

# tag duplicates in terms of tacobell.name and year
match <- match %>%
      group_by(tacobell.name, year) %>%
      mutate(count=n()) %>%
      mutate(rank = seq(1, count[1], 1))
match <- match[, c(1:2, 25:26, 3:24)]

dup <- match %>% #de-dup non-drinks items
      filter(count>1&category!="Beverages")
table(dup$category)
dup <- dup[c(1,4,6,7,9,12,14,16,17,19,21,23,25,27,30,32,34,36,38,39,41,44,46,48), ]

dup.drink <- match %>% #de-dup drink items
      filter(count>1&category=="Beverages")
dup.drink <- dup.drink[!duplicated(dup.drink$tacobell.name, dup.drink$year), ]

match <- match %>% #combine non-dup and de-dupped items
      filter(count==1)
match <- rbind(match, dup, dup.drink)
match <- match[, -c(3:4)]
rm(dup, dup.drink)

drink <- match %>% #convert drink categories to match based on size
      filter(category=="Beverages")
names(drink)
drink <- drink[, c(1,3,8:10,2,4:7,11:24)]
#export drinks, clean up/convert sizes and import back
#write.csv(drink, "data/menu-matching/drinks-size-cleanup.csv", row.names = FALSE)
drink <- read.csv("data/menu-matching/drinks-size-cleanup.csv", stringsAsFactors = FALSE)
drink <- drink[, -c(15, 17, 23:32)]
colnames(drink)[1:2] <- c("serving_size", "calories")
#merge drinks back to match master data
match <- as.data.frame(match[match$category!="Beverages", ])
match <- rbind(match, drink)
rm(drink)

other <- read.csv("data/menu-matching/manual-match/matching-results-full-list_non-ms.csv",
                  stringsAsFactors = FALSE)

convert_to_num <- function(x) {
      x <- gsub(pattern="mg|g| mg| g|G| G", replacement="", x)
      x <- as.numeric(x)
}
other[, 11:20] <- lapply(other[, 11:20], convert_to_num)
match <- rbind(match, other) #merge other sourced items to match master data
rm(other, convert_to_num, menu)

#configure multiple item products
#e.g. convert 2-pack taco to its correct calories, b/c they're likely matched to one taco's calorie











