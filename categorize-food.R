### applying bag of words matching

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
library(tm)
library(reshape2)

### import taco bell data, product and group ----
product <- read.csv("data/from-bigpurple/product_dim.csv",sep = ";", header = FALSE, quote = "\"'",stringsAsFactors = FALSE,
                    col.names = c("dw_product", "dw_productgroup", "productcd","product", "product_statuscd",
                                  "product_statusdt", "product_lastupdt", "lastupdtuserid")) %>% dplyr::select(1:2,4)
group <- read.csv("data/from-bigpurple/product_group_det.csv",sep = ";", header = FALSE, quote = "\"'",stringsAsFactors = FALSE,
                  col.names = c("dw_productgroup", "groupcd", "group","groups_tatuscd",
                                "group_statusdt", "group_lastupdt", "lastupdtuserid")) %>% dplyr::select(1,3)
product <- merge(product, group, by="dw_productgroup") %>%
   filter(!grepl("AWR| AW|AW,|AW |BYB|KFC|LJS|PH |PIZZA HUT|TCBY|ICBIY|KRYSTAL|* NEW PRODCT ADDED BY|COMBO|FRANCHISE LOCAL MENU|SPECIAL PROMOTION|NEW ITEM|TB I'M ALL EARS|DO NOT ALTER THIS ITEM|BORDER SWEAT SHIRT|TB I'M THINKING YOU ME|CFM DOWNLOAD 1|TB HELLO FRIEND|CANADA BATMAN CUP INDIVIDUAL|DELETED ITEM, DO NOT USE|CLEV INDIANS/TB BANDANNA 1.4|CFM DOWNLOAD 2|TB I'M THINKING YOU ME DINNER|CANADA BATMAN CUP W/PURCHASE|TB HELLO FRIENDGC REFUND|TB EAT IN CAR",product)&!grepl("AW|BYB|KFC|LJS|PH |PIZZA HUT|KRYSTAL|ICBIY (YOGURT)|TCBY (YOGURT)|N/A|COMBOS|NON-FOOD SALES",group))
rm(group)

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
      colnames(product)[i+11] <- paste0("full", i)
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
product <- product[, c(9:11,20:21)]

### categorize items using key word search ----
table(product$group)
product$category <- ifelse(product$group=="SALADS","salad",
                           ifelse(grepl("CHALUPAS|GORDITAS|TACO|TOSTADA",product$group)|grepl("CHALUPA|GORDITA|TACO|TOSTADA|ENCHILADA|TAQUITO",product$full),"taco",
                           ifelse(grepl("BURRITO|ENCHIRITOS",product$group)|grepl("BURRITO|ENCHIRITO",product$full),"burrito",
                           ifelse(grepl("CINNABON PRODUCTS|DESSERTS",product$group)|grepl("CINNABON|CINNABITES|APPLE EMPANADA",product$full),"dessert",
                           ifelse(grepl("DRINKS|SMOOTHIE",product$group)|grepl("PEPSI|DIET|SMOOTHIE|SHAKE",product$full),"beverage",
                           ifelse(grepl("NACHOS",product$group)|grepl("NACHO",product$full),"nacho",
                           ifelse(grepl("EXTRA/MINU",product$group)|grepl("SUB ",product$full),"substitution",
                           ifelse(grepl("BURGERS|TORTAS",product$group)|grepl("BURGER|TORTA|PIZZA|CHICKEN|BEEF|STEAK|QUESADILLA|MEXIMELT|CRUNCHWRAP",product$full),"other_entry",
                           ifelse(grepl("SIDES|FRIES",product$group)|grepl("FRIES|FRY",product$full),"side","other")))))))))
table(product$category)
product <- product[,c(2,6)]

product2 <- read.csv("data/from-bigpurple/product_dim.csv",sep = ";", header = FALSE, quote = "\"'",stringsAsFactors = FALSE,
                    col.names = c("dw_product", "dw_productgroup", "productcd","product", "product_statuscd",
                                  "product_statusdt", "product_lastupdt", "lastupdtuserid")) %>% dplyr::select(1:2,4)
group <- read.csv("data/from-bigpurple/product_group_det.csv",sep = ";", header = FALSE, quote = "\"'",stringsAsFactors = FALSE,
                  col.names = c("dw_productgroup", "groupcd", "group","groups_tatuscd",
                                "group_statusdt", "group_lastupdt", "lastupdtuserid")) %>% dplyr::select(1,3)
product2 <- merge(product2, group, by="dw_productgroup") 
product <- merge(product,product2,by="dw_product",all=TRUE)
product$category[is.na(product$category)] <- "other"
rm(product2,group)

# give numeric id to each category
product$dw_category <- ifelse(product$category=="beverage",1,ifelse(product$category=="burrito",2,
                              ifelse(product$category=="dessert",3,ifelse(product$category=="nacho",4,
                              ifelse(product$category=="other_entry",5,ifelse(product$category=="salad",6,
                              ifelse(product$category=="side",7,ifelse(product$category=="substitution",8,
                              ifelse(product$category=="taco",9,10)))))))))
table(product$dw_category)
names(product) <- toupper(names(product))
product <- product[,-5]
#write.csv(product,"data/upload-to-bigpurple/product-category.csv",row.names = FALSE)
### check early output ----
result <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_2010_Q1.csv",
                   stringsAsFactors = FALSE)
result$mean_cal <- result$cal/result$count
for (i in 1:10) {
  print(paste0("category ",i))
  print(summary(result$mean_cal[result$DW_CATEGORY==i]))
}