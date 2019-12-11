### match menu items from tacobell and menustat
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

# install and load packages ----
#install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)
#install.packages("stringdist")
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)

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

# drop commas and other punctuations
product$product <- gsub(pattern = ",", replacement = "", x=product$product)

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW", product$group)&!grepl("BYB", product$group)&!grepl("KFC", product$group)&!grepl("LJS", product$group)&!grepl("PH", product$group)&!grepl("PIZZA HUT", product$group)&product$group!="KRYSTAL"&product$group!="ICBIY (YOGURT)"&product$group!="TCBY (YOGURT)", ]
product <- product[!grepl("AWR", product$product)&!grepl("AW ", product$product)&!grepl("BYB", product$product)&!grepl("KFC", product$product)&!grepl("LJS", product$product)&!grepl("PH", product$product)&!grepl("PIZZA HUT", product$product)&!grepl("TCBY", product$product)&!grepl("ICBIY", product$product)&!grepl("KRYSTAL", product$product), ]
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
#write.csv(product, "data/menu-matching/full_product_names.csv", row.names = FALSE)

# extract all substrings in product names to fill out abbreviations ----
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
write.csv(strings, "data/menu-matching/product-names_unique_substrings.csv", row.names = FALSE)

# incorporate full names
strings <- read.csv("data/menu-matching/product-names_unique_substrings_with_abbr.csv", stringsAsFactors = FALSE)
strings$X <- NULL
sapply(strings, class)
strings$check <- as.integer(strings$check)

# replace all unchanged strings with its original value
strings$full[strings$full==""] <- strings$original[strings$full==""]
strings$check <- NULL
strings$length <- NULL

# keep only the strings that have known full spellings
#strings <- strings[is.na(strings$check) & strings$full!="", ]
#strings$check <- NULL

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
#product$product <- paste(product$product1, product$product2, product$product3,
 #                        product$product4, product$product5, product$product6,
  #                       product$product7, product$product8, sep=" ")
#product$full <- paste(product$full1, product$full2, product$full3, product$full4,
 #                     product$full5, product$full6, product$full7, product$full8,
  #                    sep=" ")

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
product <- product[product$product!="DO NOT ALTER THIS ITEM", ]
product$full <- gsub("TEST ", "", product$full)
product$full <- gsub("\\(", "", product$full)
product$full <- gsub("\\)", "", product$full)

### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
menu$item_name <- toupper(menu$item_name)
#menu <- menu[grepl("16", menu$item_name), ]
menu <- menu[!duplicated(menu$item_name), c(1, 4:5)]
length(unique(menu$item_name))
#menu <- menu[menu$year<=2016, ]

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

### find exact matches ----
#match <- merge(product, menu, by.x="product", by.y="item_name") #64 exact matches
#rm(match)

### fuzzy matching ----
colnames(menu)[2] <- "full"

start_time <- Sys.time()
join_jaccard <- stringdist_join(product, menu, 
                by="full",
                mode = "left",
                ignore_case = FALSE, 
                method = "jaccard", 
                max_dist = 99, 
                distance_col = "dist.jc") %>%
      group_by(full.x) %>%
      top_n(1, -dist.jc)
end_time <- Sys.time()
end_time - start_time #55 secs
rm(start_time, end_time)

names(join_jaccard)
join_jaccard <- join_jaccard[, c(2, 5, 7, 3, 6, 1, 4)]
colnames(join_jaccard)[1:2] <- c("full.tb", "full.menustat")
join_jaccard <- join_jaccard[order(join_jaccard$dist.jc, join_jaccard$full.tb), ]

# number of exact matches
length(unique(join_jaccard$full.tb[join_jaccard$dist.jc==0])) #226
length(join_jaccard$full.tb[join_jaccard$dist.jc==0]) #281

# visualize
hist(join_jaccard$dist.jc, breaks = 100,
     main="Distribution of distances",
     xlab = "Jaccard distance")

### merge matched menu items back to product table ----
# re-run the 1st two sections of the script
# get product ids
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

# drop commas and other punctuations
product$product <- gsub(pattern = ",", replacement = "", x=product$product)

# based on product group and product description
# drop other YUM brand products
product <- product[!grepl("AW", product$group)&!grepl("BYB", product$group)&!grepl("KFC", product$group)&!grepl("LJS", product$group)&!grepl("PH", product$group)&!grepl("PIZZA HUT", product$group)&product$group!="KRYSTAL"&product$group!="ICBIY (YOGURT)"&product$group!="TCBY (YOGURT)", ]
product <- product[!grepl("AWR", product$product)&!grepl("AW ", product$product)&!grepl("BYB", product$product)&!grepl("KFC", product$product)&!grepl("LJS", product$product)&!grepl("PH", product$product)&!grepl("PIZZA HUT", product$product)&!grepl("TCBY", product$product)&!grepl("ICBIY", product$product)&!grepl("KRYSTAL", product$product), ]

# drop non-descriptive items
product <- product[product$group!="N/A"&product$group!="CFM MANAGER SPECIALS"&product$group!="COMBOS"&product$product!=""&!grepl("* NEW PRODCT ADDED BY", product$product)&!grepl("COMBO", product$product)&!grepl("FRANCHISE LOCAL MENU", product$product)&product$product!="NEW ITEM"&!grepl("SPECIAL PROMOTION", product$product), ]

# drop non-food items
product <- product[product$group!="NON-FOOD SALES (PRE", ]


# keep only the product names and ids
# merge with join_jaccard table, identify the items that had exact matches
names(product)
product <- product[, c(2, 4)]
product <- merge(product, join_jaccard,by = "product", all = TRUE)
product <- product[product$dist.jc==0, c(1:2)]

### check sales volume represented by exact match items ----
# build empty shell for summary stats
sales_all <- data.frame(matrix(data=NA, nrow=(9*4-1), ncol = 4),
                        stringsAsFactors = FALSE)
colnames(sales_all)[1:4] <- c("year", "quarter", "sales", "matched")

detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
sapply(detail, class)

for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                  sales <- read.csv(paste0("data/from-bigpurple/sales-vol-by-product/sales_",
                                          i, "_Q0", j, ".csv"),
                                    sep = ";", header = FALSE, quote = "\"'",
                                    stringsAsFactors = FALSE,
                                    col.names = c("p_detail", "sales", "qty"))
                  #sapply(sales, class)
                  sales <- merge(detail, sales, by="p_detail", all=TRUE)
                  #print(paste0("1st merge done: ", "year ", i, " Q", j))
                        
                  # clean house
                  sales <- sales[!is.na(sales$sales), ]
                  sales <- sales[!grepl("AWR", sales$detail_desc)&!grepl("AW ", sales$detail_desc)&
                                 !grepl("BYB", sales$detail_desc)&!grepl("KFC", sales$detail_desc)&
                                 !grepl("LJS", sales$detail_desc)&!grepl("PH", sales$detail_desc)&
                                 !grepl("PIZZA HUT", sales$detail_desc)&!grepl("TCBY", sales$detail_desc)&
                                 !grepl("ICBIY", sales$detail_desc)&!grepl("KRYSTAL", sales$detail_desc), ]
                        
                  sales <- sales[sales$detail_desc!="CFM MANAGER SPECIALS"&
                                 sales$detail_desc!=""&
                                 !grepl("* NEW PRODCT ADDED BY", sales$detail_desc)&
                                 !grepl("COMBO", sales$detail_desc)&
                                 !grepl("FRANCHISE LOCAL MENU", sales$detail_desc)&
                                 sales$detail_desc!="NEW ITEM"&
                                 !grepl("SPECIAL PROMOTION", sales$detail_desc), ]
                        
                  sales <- merge(sales, join_jaccard, by.x = "detail_desc", by.y = "product", all = TRUE)
                  #print(paste0("2nd merge done: ", "year ", i, " Q", j))
                  #names(sales)
                  #detial$id <- NULL
                  sales <- sales[!is.na(sales$p_detail), ]
                  
                  # delete duplicated rows
                  sales <- sales[!duplicated(sales$detail_desc), c(1, 3:4, 7)]

                  # fill in summary stats
                  sales_all[j+4*(i-2007), 1] <- i
                  sales_all[j+4*(i-2007), 2] <- j
                  sales_all[j+4*(i-2007), 3] <- sum(sales$qty, na.rm = TRUE)
                  sales_all[j+4*(i-2007), 4] <- sum(sales$qty[sales$dist.jc==0], na.rm = TRUE)    
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)
sales_all$matched_pct <- sales_all$matched/sales_all$sales

# visualization
ggplot(data=sales_all, aes(x=paste(year, "Q", quarter, sep=""), y=matched_pct, group=1)) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      labs(title="Number of sold items represented by matched products",
           x="Year", y="Percent",
           caption="Data source: Taco Bell") +
      #scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items.jpeg", width=20, height=10, unit="cm")



