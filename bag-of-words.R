### applying bag of words matching

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
library(dplyr)
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
#install.packages("textstem")
library(textstem)
library(fuzzyjoin)

### import taco bell data, product and group ----
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

### clean house, drop items and categories not in the analysis ----
table(product$group)

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW|BYB|KFC|LJS|PH |PIZZA HUT|KRYSTAL|ICBIY (YOGURT)|TCBY (YOGURT)",
                          product$group) &
                         !grepl("AWR| AW|AW,|AW |BYB|KFC|LJS|PH|PIZZA HUT|TCBY|ICBIY|KRYSTAL",
                                product$product), ]
length(unique(product$product)) 

# drop non-descriptive items
product <- product[product$group!="N/A"&product$group!="CFM MANAGER SPECIALS"&
                         product$group!="COMBOS"&product$product!=""&
                         !grepl("* NEW PRODCT ADDED BY|COMBO|FRANCHISE LOCAL MENU|SPECIAL PROMOTION", product$product)&
                         product$product!="NEW ITEM", ]
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
length(unique(product$product))


# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]
length(unique(product$product))

# extract only unique product names
product <- product[!duplicated(product$product), c(4, 9)]

### extract unique substrings, for non-drinks and non-smoothies ----
#strings <- as.data.frame(unlist(strsplit(product$product[product$group!="DRINKS"|product$group!="SMOOTHIES"], split=" ")))
#colnames(strings)[1] <- "original"
#class(strings$original)
#strings$original <- as.character(strings$original)

# measure substring length
#strings$length <- nchar(strings$original)

# frequency, how often does a substring show up in product name
#strings <- strings %>%
#      group_by(original) %>%
#      mutate(count=n())
#strings <- strings[!duplicated(strings), ]
#strings <- strings[order(strings$count, decreasing = TRUE), ]
#write.csv(strings, "data/menu-matching/product-names_unique_substrings_bow_nodrinks.csv",
#          row.names = FALSE)

### read corrected string file, fill abbreviation and fix typo, also remove meaningless numbers ----
strings <- read.csv("data/menu-matching/product-names_unique_substrings_bow_nodrinks_corrected.csv",
                    stringsAsFactors = FALSE)
sapply(strings, class)
strings <- strings[, -c(2:3)]

# fix numbers that excel automatically converted to dates
strings$original[strings$original=="02-Jan"] <- "1/2"
strings$original[strings$original=="03-Jan"] <- "1/3"
strings$original[strings$original=="43834"] <- "1/4"
strings$original[strings$original=="43983"] <- "1/6"
strings$original[strings$original=="0.2"] <- ".2"
strings$original[strings$original=="0.39"] <- ".39"
strings <- strings[!duplicated(strings$original), ]
strings$full[strings$original=="CAN"] <- "CANTINA"


# replace abbreviations with full spelling
# first, break product names into separate substrings in their own columns
# second, merge each column with the replacement strings
product <- product %>%
      separate(product, c("product1", "product2", "product3", "product4", "product5", "product6", "product7", "product8"), " ")

for (i in c(1:8)) {
      product <- merge(product, strings, by.x=paste0("product", i), by.y="original", sort=FALSE, all.x = TRUE)
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
product$full <- ifelse(grepl(" DR|DR ", product$full)&!grepl("DR PEPPER|DRINK", product$full),
                       gsub("DR", "DR PEPPER", product$full),
                       product$full)
product$full <- gsub("AT HALF O|AT HALF OFF", "", product$full)
product <- product[product$full!="", ]
length(unique(product$full)) #3492

### trim white space, remove punctuation ----
product$full <- stripWhitespace(product$full)
product$full <- removePunctuation(product$full)

### add numbers back ----
# add numbers back
product$full <- gsub("THREE AND HALF", "3.5", product$full)
product$full <- gsub("TEN AND HALF", "10.5", product$full)
product$full <- gsub("TWELVE", "12", product$full)
product$full <- gsub("FOURTEEN", "14", product$full)
product$full <- gsub("FIFTEEN", "15", product$full)
product$full <- gsub("SIXTEEN", "16", product$full)
product$full <- gsub("TWENTY FOUR", "24", product$full)
product$full <- gsub("THIRTY TWO", "32", product$full)
product$full <- gsub("TWENTY", "20", product$full)
product$full <- gsub("FORTY FOUR", "44", product$full)
product$full <- gsub("SIXTH", "1/6", product$full)
product$full <- gsub("ONE AND HALF LITER", "1.5 LITER", product$full)
product$full <- gsub("ONE AND HALF", "1.5", product$full)
product$full <- gsub("HALF LB", "1/2 LB", product$full)
product$full <- gsub("THIRD LB", "1/3 LB", product$full)
product$full <- gsub("TEN", "10", product$full)
product$full <- gsub("FIVE-LAYER", "5-LAYER", product$full)
product$full <- gsub("SIX AND HALF", "6.5", product$full)
product$full <- gsub("SIX HUNDRED", "600", product$full)
product$full <- gsub("SIXTY FOUR", "64", product$full)
product$full <- gsub("SEVEN-LAYER", "7-LAYER", product$full)
product$full <- gsub("SEVEN UP", "7UP", product$full)
product$full <- gsub("TEN | TEN", "10", product$full)
product$full <- gsub("QUARTER", "1/4", product$full)
product$full <- gsub("THIRD", "1/3", product$full)
product$full <- gsub("TWO", "2", product$full)
product$full <- gsub("THREE", "3", product$full)
product$full <- gsub("FOUR", "4", product$full)
product$full <- gsub("FIVE", "5", product$full)
product$full <- gsub("SIX", "6", product$full)
product$full <- gsub("SEVEN", "7", product$full)
product$full <- gsub("EIGHT", "8", product$full)
product$full <- gsub("NINE", "9", product$full)
product$full <- gsub("HALF LB", "1/2 LB", product$full)
product$full <- gsub("HALF GALLON", "1/2 GALLON", product$full)
product$full <- gsub("IN TORTILLA", "INCH TORTILLA", product$full)
product$full <- gsub("H AND AND", "HNN", product$full)
product$full <- ifelse(grepl("ONE", product$full)&!grepl("CONE|ZONE|KONE", product$full),
                       gsub("ONE", "1", product$full), product$full)

### remove stop words ----
# remove stop words
# add custom stop words
stop <- c("CENT", "CENTS", "FOR 2", "VERSION", "COUPON", "HNN", "DENVER",
          "SANTA FE", "6 TO 1", "SOS", "FOR 4", "SMT", "SCHOOL LUNCH", "UPSELL",
          "MADISON OKC", "OMA", "BUT", "IF", "BETWEEN", "INTO", "THROUGH",
          "DURING", "BEFORE", "AFTER", "AT", "BY", "FOR", "WITH", "ABOUT",
          "OR", "BECAUSE", "AS", "UNTIL", "WHILE", "OF", "AGAINST", "ABOVE",
          "BELOW", "TO", "FROM", "UP", "DOWN", "IN", "OUT", "ON", "OFF", "OVER",
          "UNDER", "AGAIN", "FURTHER", "THEN", "ONCE", "HERE", "THERE", "ALL",
          "ANY", "BOTH", "EACH", "MORE", "OTHER", "SOME", "NOR", "NOT", "ONLY",
          "OWN", "SAME", "SO", "THAN", "TOO", "VERY", "ADD", "TB")
product$full <- removeWords(product$full, stop)
rm(stop)

### lemmatization ----
product$rename <- toupper(lemmatize_strings(product$full))

# fix numbers: 1/2, 1/3, etc
product$rename <- gsub(" / ", "/", product$rename)

# fix words lemmatization didnt address
product$rename <- gsub("7LAYER", "7 LAYERO", product$full)
product$rename <- gsub("5LAYER", "5 LAYERO", product$full)
product$rename <- gsub("TACOS", "TACO", product$full)
product$rename <- gsub("BURGERS", "BURGER", product$full)
product$rename <- gsub("NACHOS", "NACHO", product$full)

# remove white space
product$rename <- stripWhitespace(product$rename)

### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
menu$item_name <- toupper(menu$item_name)
#menu <- menu[grepl("16", menu$item_name), ]
menu <- menu[!duplicated(menu$item_name), c(1, 4:5)]
length(unique(menu$item_name))
#menu <- menu[menu$year<=2016, ]

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

### jaccard distance ----
# a default q=1
start_time <- Sys.time()
join_jaccard <- stringdist_join(product, menu, 
                                by=c("rename"="item_name"),
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", #q=1
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(rename) %>%
      top_n(1, -dist.jc)
end_time <- Sys.time()
end_time - start_time # approx. 45 secs
rm(start_time, end_time)

names(join_jaccard)
join_jaccard <- join_jaccard[!duplicated(join_jaccard$rename, join_jaccard$item_name), c(4, 6, 8)]
join_jaccard <- join_jaccard[order(join_jaccard$dist.jc, join_jaccard$rename), ]

# number of exact matches
length(unique(join_jaccard$rename[join_jaccard$dist.jc==0])) #264

### cosine distance ----
start_time <- Sys.time()
join_cosine <- stringdist_join(product, menu, 
                                by=c("rename"="item_name"),
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "cosine",
                                max_dist = 99, 
                                distance_col = "dist.cs") %>%
      group_by(rename) %>%
      top_n(1, -dist.cs)
end_time <- Sys.time()
end_time - start_time # approx. 45 secs
rm(start_time, end_time)

names(join_cosine)
join_cosine <- join_cosine[!duplicated(join_cosine$rename, join_cosine$item_name), c(4, 6, 8)]
join_cosine <- join_cosine[order(join_cosine$dist.cs, join_cosine$rename), ]

# number of exact matches
length(unique(join_cosine$rename[join_cosine$dist.cs==0])) #144



