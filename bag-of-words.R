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
strings <- as.data.frame(unlist(strsplit(product$product[product$group!="DRINKS"|product$group!="SMOOTHIES"], split=" ")))
colnames(strings)[1] <- "original"
class(strings$original)
strings$original <- as.character(strings$original)

# measure substring length
strings$length <- nchar(strings$original)

# frequency, how often does a substring show up in product name
strings <- strings %>%
      group_by(original) %>%
      mutate(count=n())
strings <- strings[!duplicated(strings), ]
strings <- strings[order(strings$count, decreasing = TRUE), ]
#write.csv(strings, "data/menu-matching/product-names_unique_substrings_bow_nodrinks.csv",
#          row.names = FALSE)

### read corrected string file, fill abbreviation and fix typo, also remove meaningless numbers ----
strings <- read.csv("data/menu-matching/product-names_unique_substrings_bow_nodrinks_corrected.csv",
                    stringsAsFactors = FALSE)
sapply(strings, class)
strings <- strings[, -c(2:3)]
strings <- strings[!duplicated(strings$original), ]

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
product$full <- gsub("TB ", "", product$full)
product$full <- ifelse(grepl("DR", product$full) & !grepl("DR PEPPER", product$full),
                       gsub("DR", "DR PEPPER", product$full),
                       product$full)
product <- product[product$full!="", ]
length(unique(product$full)) #3494

### more cleaning ----
# trim white space
# remove punctuation
# remove stop words
# stemming
product$full <- trimws(product$full, which="both")
product$full <- removePunctuation(product$full)














