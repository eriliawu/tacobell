### build table for nutrition and calorie information
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
library(RColorBrewer)

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
product$original_name <- product$product

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

# extract only unique product names
product <- product[, c(2, 4, 13)]
#write.csv(product, "data/menu-matching/full_product_names.csv", row.names = FALSE)

### extract all substrings in product names to fill out abbreviations ----
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
      colnames(product)[i+10] <- paste0("full", i)
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
product <- product[, c(9:10, 19:20)]

# drop key words from product names: TEST, (), DO NOT ALTER THIS ITEM
product$full <- gsub("STEAK LOUIS", replacement = "ST LOUIS", product$full)
product$full <- gsub("TEST", "", product$full)
product$full <- gsub("\\(", "", product$full)
product$full <- gsub("\\)", "", product$full)
product$full <- gsub("TB ", "", product$full)
product <- product[product$full!="", ]

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

### configure multiple item products ----
#e.g. convert 2-pack taco to its correct calories, b/c they're likely matched to one taco's calorie
names(match)
match$tacobell.name <- trimws(match$tacobell.name, "both")
match <- match[order(match$tacobell.name, match$year), ]

# extract size information for non-beverage items
table(match$category)

multiple <- match %>% #only work on items with numbers in the name
      filter(category!="Beverages" & grepl("[[:digit:]]", tacobell.name))
multiple$tb.rename <- gsub("99|39|49|@1.99|35|69|0.99|79", "", multiple$tacobell.name) #take out price numbers
multiple <- multiple[, c(1,25,2:24)]

multiple$tb.rename <- gsub("1/2 LB", "HALF LB", multiple$tb.rename) #shield some numbers from being extratced
multiple$tb.rename <- gsub("5 LAYER", "FIVE LAYER", multiple$tb.rename)
multiple$tb.rename <- gsub("5-LAYER", "FIVE-LAYER", multiple$tb.rename)
multiple$tb.rename <- gsub("7 LAYER", "SEVEN LAYER", multiple$tb.rename)
multiple$tb.rename <- gsub("7-LAYER", "SEVEN-LAYER", multiple$tb.rename)
multiple$tb.rename <- gsub("3 CHEESE", "THREE CHEESE", multiple$tb.rename)
multiple$tb.qty <- as.integer(gsub("[[:alpha:]]|[[:punct:]]", "", multiple$tb.rename))

multiple$ms.rename <- gsub("1/2 LB", "HALF LB", multiple$menustat.name) #clean up quantity info in menustat names
multiple$ms.rename <- gsub("5 LAYER", "FIVE LAYER", multiple$ms.rename)
multiple$ms.rename <- gsub("5-LAYER", "FIVE-LAYER", multiple$ms.rename)
multiple$ms.rename <- gsub("7 LAYER", "SEVEN LAYER", multiple$ms.rename)
multiple$ms.rename <- gsub("7-LAYER", "SEVEN-LAYER", multiple$ms.rename)
multiple$ms.rename <- gsub("3 CHEESE", "THREE CHEESE", multiple$ms.rename)
multiple$ms.qty <- as.integer(gsub("[[:alpha:]]|[[:punct:]]", "", multiple$ms.rename))
multiple <- multiple[, c(2, 26:28, 1, 3:25)]

multiple[, c("tb.qty", "ms.qty")][is.na(multiple[, c("tb.qty", "ms.qty")])] <- 1
multiple[, c(14:24)] <- lapply(multiple[, c(14:24)], function(x) x/multiple$ms.qty*multiple$tb.qty) #convert all nutritional infor
names(multiple)
names(match)
multiple <- multiple[, -c(1:4)]

match <- match %>%
      filter(category=="Beverages"|!grepl("[[:digit:]]", tacobell.name))
match <- rbind(match, multiple)
match <- match[, -c(21:24)]
rm(multiple)
#write.csv(match, "data/menu-matching/matched-results/matched-items-by-year.csv", row.names = FALSE)

# collapse table to unique item level
# take mean of calorie of multi-year items
length(unique(match$tacobell.name)) == length(unique(match$tacobell.name, match$menustat.name)) #false
names(match)
match <- match[, -c(2:9)]
sapply(match, class)
match <- match %>%
      group_by(tacobell.name) %>%
      summarise_all(list(mean))
match$calories[match$calories==24000] <- 2000
match <- match[!is.na(match$tacobell.name), ]
#write.csv(match, "data/menu-matching/matched-results/matched-items-unique.csv", row.names = FALSE)

### link unique item level table to DW_PRODUCT ----
match <- read.csv("data/menu-matching/matched-results/matched-items-unique.csv",
                  stringsAsFactors = FALSE)
names(match)
names(product)
match <- match[order(match$calories), ]
length(unique(match$tacobell.name))
match <- merge(product, match, by.x = "full", by.y = "tacobell.name", all=TRUE)
match <- match[order(match$original_name), ]
match$dw_product[match$full=="EXTRA SIDE OF DINNER BEANS"] <- 2013
match$original_name[match$full=="EXTRA SIDE OF DINNER BEANS"] <- "TEST EX SIDE OF DINNER BEANS"
match$dw_product[match$full=="NE STEAK BURRITO SUPREME"] <- 4344
match$original_name[match$full=="NE STEAK BURRITO SUPREME"] <- "TEST NE STEAK BURRITO SUPREME"
match$dw_product[match$full=="BREAKFAST GRILLED STUFFED BURRITO"] <- 25166
match$original_name[match$full=="BREAKFAST GRILLED STUFFED BURRITO"] <- "BREAKFAST GSB"
length(unique(match$dw_product))
temp <- match %>%
      filter(dw_product==2013|dw_product==4344|dw_product==25166)
rm(temp)
match <- match[!((match$dw_product==2013|match$dw_product==4344|match$dw_product==25166)&is.na(match$calories)), ]
length(unique(match$dw_product))

# adding other dropped items to the table
product <- read.csv("data/from-bigpurple/product_detail.csv", stringsAsFactors = FALSE)
product <- merge(product, match, by.x="p_detail", by.y="dw_product", all=TRUE)
names(product)
product <- product[, -c(4:5)]
colnames(product)[1:3] <- c("DW_PRODUCT", "PRODUCTDESC", "FULLPRODUCT")
sum(is.na(product$DW_PRODUCT))

# add product group to table
product <- read.csv("data/menu-matching/matched-results/PRODUCT_CALORIE_DIM.csv", stringsAsFactors = FALSE)
detail <- read.csv("data/from-bigpurple/product_dim.csv",
         sep = ";", header = FALSE, quote = "\"'",
         stringsAsFactors = FALSE,
         col.names = c("dw_product", "dw_productgroup", "productcd",
                       "product", "product_statuscd",
                       "product_statusdt", "product_lastupdt", "lastupdtuserid"))
detail <- detail[, c(1:2)]
product <- merge(product, detail, by.x = "DW_PRODUCT", by.y = "dw_product")
names(product)
colnames(product)[3:15] <- c("FULLDESC", "CALORIES", "TOTAL_FAT", "SAT_FAT", "TRANS_FAT",
                             "CHOLESTEROL", "SODIUM", "POTASSIUM", "CARB", "FIBER",
                             "SUGAR", "PROTEIN", "DW_PRODUCTGROUP")
product <- product[, c(1, 15, 2:14)]
#write.csv(product, "data/menu-matching/matched-results/PRODUCT_CALORIE_DIM.csv", row.names = FALSE)






