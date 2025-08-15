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
#install.packages(c("stringdist", "fuzzyjoin"))
#install.packages("reshape2")
library(reshape2)

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
                         !grepl("AWR| AW|AW,|AW |BYB|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL",
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
                         product$product!="TB HELLO FRIEND"&
                         product$product!="GC REFUND"&
                         product$product!="TB EAT IN CAR", ]
length(unique(product$product))

# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]
length(unique(product$product))

# extract only unique product names
product <- product[!duplicated(product$product), c(4, 9)]

### extract unique substrings in tb data, for non-drinks and non-smoothies ----
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
strings <- read.csv("data/menu-matching/product-names_unique_substrings_bow_corrected.csv",
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
strings$full[strings$original=="FRUITISTA"] <- "FRUTISTA"

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
product$full <- gsub("FORTY 2", "42", product$full)
product$full <- gsub("PINK LEMONAIDE", "PINK LEMONADE", product$full)
product$full <- gsub("10DER", "TENDER", product$full)
product$full <- gsub("SWEE10ED", "SWEETENED", product$full)
product$full <- gsub("H1Y", "HONEY", product$full)
product$full <- ifelse(grepl("2 BEAN", product$full)&!grepl("2 BEAN BURGER", product$full),
                       gsub("2 BEAN", "TO BEAN", product$full), product$full)

### remove stop words ----
# remove stop words
# add custom stop words
stop <- c("CENT", "CENTS", "FOR 2", "VERSION", "COUPON", "HNN", "DENVER",
          "SANTA FE", "6 TO 1", "SOS", "FOR 4", "SMT", "SCHOOL LUNCH", "UPSELL",
          "MADISON OKC", "OMA", "BUT", "IF", "BETWEEN", "INTO", "THROUGH",
          "DURING", "BEFORE", "AFTER", "AT", "BY", "FOR", "WITH", "ABOUT",
          "OR", "BECAUSE", "AS", "UNTIL", "WHILE", "OF", "AGAINST", "ABOVE",
          "BELOW", "DOWN", "IN", "OUT", "ON", "OFF", "OVER",
          "UNDER", "AGAIN", "FURTHER", "THEN", "ONCE", "HERE", "THERE", "ALL",
          "ANY", "BOTH", "EACH", "MORE", "OTHER", "SOME", "NOR", "NOT", "ONLY",
          "OWN", "SAME", "SO", "THAN", "TOO", "VERY", "ADD", "TB", "HOT DEAL")
product$full <- removeWords(product$full, stop)
rm(stop)

### lemmatization ----
product$full <- lemmatize_strings(product$full)
product$full <- toupper(product$full)

# fix numbers: 1/2, 1/3, etc
product$full <- gsub(" / ", "/", product$full)

# fix words lemmatization didnt address
product$full <- gsub("7LAYER", "7 LAYERO", product$full)
product$full <- gsub("5LAYER", "5 LAYERO", product$full)
product$full <- gsub("TACOS", "TACO", product$full)
product$full <- gsub("BURGERS", "BURGER", product$full)
product$full <- gsub("NACHOS", "NACHO", product$full)

# remove white space
product$full <- stripWhitespace(product$full)

length(unique(product$full)) #3374
product <- product[!duplicated(product$full)&product$full!="", ]

### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
names(menu)
menu$item_name <- toupper(menu$item_name)
length(unique(menu$id)) #700

# drop duplicates
menu <- menu[!duplicated(menu$item_name, menu$id), c(1, 4:5)] 

### extract unique substrings in menustat ----
#strings <- as.data.frame(unlist(strsplit(menu$item_name, split=" ")))
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
#write.csv(strings, "data/menu-matching/product-names_unique_substrings_bow_menustat.csv",
#          row.names = FALSE)

### read corrected string file ----
strings <- read.csv("data/menu-matching/product-names_unique_substrings_bow_menustat_corrected.csv",
                    stringsAsFactors = FALSE)
sapply(strings, class)
strings <- strings[, -c(2:3)]

# fix numbers that excel automatically converted to dates
strings$original[strings$original=="02-Jan"] <- "1/2"

# replace abbreviations with full spelling
# first, break product names into separate substrings in their own columns
# second, merge each column with the replacement strings
menu <- menu %>%
      separate(item_name, c("item_name1", "item_name2", "item_name3", "item_name4",
                          "item_name5", "item_name6", "item_name7", "item_name8",
                          "item_name9", "item_name10", "item_name11", "item_name12",
                          "item_name13"), " ")

for (i in c(1:13)) {
      menu <- merge(menu, strings, by.x=paste0("item_name", i), by.y="original", sort=FALSE, all.x = TRUE)
      colnames(menu)[i+15] <- paste0("full", i)
}
rm(i, strings)

# paste all substrings, but leave out the NAs
menu$item_name <- apply(cbind(menu$item_name1, menu$item_name2, menu$item_name3,
                               menu$item_name4, menu$item_name5, menu$item_name6,
                               menu$item_name7, menu$item_name8, menu$item_name9,
                              menu$item_name10, menu$item_name11, menu$item_name12,
                              menu$item_name13),
                         1, function(x) paste(x[!is.na(x)], collapse = " "))
menu$full <- apply(cbind(menu$full1, menu$full2, menu$full3, menu$full4,
                            menu$full5, menu$full6, menu$full7, menu$full8,
                            menu$full9, menu$full10, menu$full11, menu$full12, menu$full13),
                      1, function(x) paste(x[!is.na(x)], collapse = " "))
names(menu)
menu <- menu[, c(29:30, 14:15)]

### remove unnecessary text/stop words, punctuation, whites pace ----
menu$full <- sapply(strsplit(menu$full, "FOR "), "[", 1)

stop <- c("CENT", "CENTS", "FOR 2", "VERSION", "COUPON", "HNN", "DENVER",
          "SANTA FE", "6 TO 1", "SOS", "FOR 4", "SMT", "SCHOOL LUNCH", "UPSELL",
          "MADISON OKC", "OMA", "BUT", "IF", "BETWEEN", "INTO", "THROUGH",
          "DURING", "BEFORE", "AFTER", "AT", "BY", "FOR", "WITH", "ABOUT",
          "OR", "BECAUSE", "AS", "UNTIL", "WHILE", "OF", "AGAINST", "ABOVE",
          "BELOW", "TO", "FROM", "UP", "DOWN", "IN", "OUT", "ON", "OFF", "OVER",
          "UNDER", "AGAIN", "FURTHER", "THEN", "ONCE", "HERE", "THERE", "ALL",
          "ANY", "BOTH", "EACH", "MORE", "OTHER", "SOME", "NOR", "NOT", "ONLY",
          "OWN", "SAME", "SO", "THAN", "TOO", "VERY", "ADD", "TB", "CRAVINGS",
          "WHY PAY MORE VALUE MENU", "STYLE", "REGIONAL", "CALLED", "ALSO",
          "15 CALORIE", "BUCK BOX", "USDA", "SELECT", "LAS", "VEGAS")
menu$full <- removeWords(menu$full, stop)
rm(stop)

menu$full <- removePunctuation(menu$full)
menu$full <- stripWhitespace(menu$full)

#lemmatization
menu$full <- toupper(lemmatize_strings(menu$full))

# fix numbers: 1/2, 1/3, etc
menu$full <- gsub(" / ", "/", menu$full)

# fix words lemmatization didnt address
menu$full <- gsub("7LAYER", "7 LAYER", menu$full)
menu$full <- gsub("5LAYER", "5 LAYER", menu$full)
menu$full <- gsub("TACOS", "TACO", menu$full)
menu$full <- gsub("BURGERS", "BURGER", menu$full)
menu$full <- gsub("NACHOS", "NACHO", menu$full)

length(unique(menu$full)) #759
menu <- menu[!duplicated(menu$full), ]

### jaccard distance, q=1 ----
# a default q=1
start_time <- Sys.time()
join_jaccard <- stringdist_join(product, menu, 
                                by="full",
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", #q=1
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(full.x) %>%
      top_n(1, -dist.jc)
end_time <- Sys.time()
end_time - start_time #approx. 37 secs
rm(start_time, end_time)

names(join_jaccard)
colnames(join_jaccard)[c(2, 5)] <- c("full.tb", "full.menustat")
join_jaccard <- join_jaccard[order(join_jaccard$dist.jc, join_jaccard$full.tb),
                             c(2, 5, 8, 1, 3:4, 6:7)]

# number of exact matches
length(unique(join_jaccard$full.tb[join_jaccard$dist.jc==0])) #282

### jaccard distance, q=2 ----
start_time <- Sys.time()
join_jaccard2 <- stringdist_join(product, menu, 
                                by="full",
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", q=2,
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(full.x) %>%
      top_n(1, -dist.jc)
end_time <- Sys.time()
end_time - start_time #approx. 37 secs
rm(start_time, end_time)

names(join_jaccard2)
colnames(join_jaccard2)[c(2, 5)] <- c("full.tb", "full.menustat")
join_jaccard2 <- join_jaccard2[order(join_jaccard2$dist.jc, join_jaccard2$full.tb),
                             c(2, 5, 7, 3, 1, 4, 6)]

# number of exact matches
length(unique(join_jaccard2$full.tb[join_jaccard2$dist.jc==0])) #176

### cosine distance ----
start_time <- Sys.time()
join_cosine <- stringdist_join(product, menu, 
                                by="full",
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "cosine",
                                max_dist = 99, 
                                distance_col = "dist.cs") %>%
      group_by(full.x) %>%
      top_n(1, -dist.cs)
end_time <- Sys.time()
end_time - start_time # approx. 36 secs
rm(start_time, end_time)

names(join_cosine)
colnames(join_cosine)[c(2, 5)] <- c("full.tb", "full.menustat")
join_cosine <- join_cosine[!duplicated(join_cosine$dist.cs, join_cosine$full.tb),
                           c(2, 5, 7, 3, 1, 4, 6)]
join_cosine <- join_cosine[order(join_cosine$dist.cs, join_cosine$full.tb), ]

# number of exact matches
length(unique(join_cosine$full.tb[join_cosine$dist.cs==0])) #

### for mturk, pilot, 1:5 options ----
pilot200 <- read.csv("data/menu-matching/manual-match/mturk/pilot/pilot200.csv", stringsAsFactors = FALSE)
pilot200 <- pilot200[!duplicated(pilot200$full), ]
pilot_join <- stringdist_join(pilot200, menu, 
                                by="full",
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", #q=1
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(full.x) %>%
      top_n(4, -dist.jc)
names(pilot_join)
pilot_join <- pilot_join[, c(1, 4, 7)]
colnames(pilot_join)[1:2] <- c("full_tb", "full.menustat")
pilot_join <- pilot_join[order(pilot_join$full_tb, pilot_join$dist.jc), ]

# tag num of matches per tacobell item, drop 5th item onward
pilot_join <- pilot_join %>%
      group_by(full_tb) %>%
      mutate(count=n()) %>%
      mutate(rank=seq(1, count[1], 1))
table(pilot_join$count)
pilot_join <- pilot_join[pilot_join$rank<=4, ]
pilot_join$count <- NULL

# reshape to wide
pilot_join1 <- dcast(pilot_join, full_tb ~ rank, value.var ="full.menustat")
colnames(pilot_join1)[2:5] <- c("item1", "item2", "item3", "item4")
pilot_join2 <- dcast(pilot_join, full_tb ~ rank, value.var ="dist.jc")
colnames(pilot_join2)[2:5] <- c("dist1", "dist2", "dist3", "dist4")
pilot_join <- merge(pilot_join1, pilot_join2, by="full_tb")
rm(pilot_join1, pilot_join2)
write.csv(pilot_join, "data/menu-matching/manual-match/mturk/pilot/pilot200-1v5.csv", row.names = FALSE)












