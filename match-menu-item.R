### match menu items from tacobell and menustat
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
write.csv(strings, "data/menu-matching/product-names_unique_substrings.csv", row.names = FALSE)

# incorporate full names
strings <- read.csv("data/menu-matching/product-names_unique_substrings_w_correction.csv",
                    stringsAsFactors = FALSE)
#strings$X <- NULL
sapply(strings, class)
#strings$check <- as.integer(strings$check)

# replace all unchanged strings with its original value
strings$full[strings$full==""] <- strings$original[strings$full==""]
#strings$check <- NULL
strings$length <- NULL

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
#menu <- menu[grepl("16", menu$item_name), ]
menu <- menu[!duplicated(menu$item_name), c(1, 4:5)]
length(unique(menu$item_name))
#menu <- menu[menu$year<=2016, ]

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

### generate word cloud, for both taco bell and menustat ----
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=2, max.words=200) { 
      library("tm")
      library("SnowballC")
      library("wordcloud")
      library("RColorBrewer") 
      
      if(type[1]=="file") text <- readLines(x)
      else if(type[1]=="url") text <- html_to_text(x)
      else if(type[1]=="text") text <- x
      
      # Load the text as a corpus
      docs <- Corpus(VectorSource(text))
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove stopwords for the language 
      docs <- tm_map(docs, removeWords, stopwords(lang))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      # Remove your own stopwords
      if(!is.null(excludeWords)) 
            docs <- tm_map(docs, removeWords, excludeWords) 
      # Text stemming
      if(textStemming) docs <- tm_map(docs, stemDocument)
      # Create term-document matrix
      tdm <- TermDocumentMatrix(docs)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      # check the color palette name 
      if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
      else colors = brewer.pal(8, colorPalette) 
      # Plot the word cloud
      set.seed(1234)
      wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
                random.order=FALSE, rot.per=0.35, 
                use.r.layout=FALSE, colors=colors)
      
      invisible(list(tdm=tdm, freqTable = d))
}

cloud_tb <- rquery.wordcloud(x=product$full, type="text", lang="english",
                             min.freq = 2, max.words = 200)
freq <- cloud_tb$freqTable
barplot(freq[1:15,]$freq, las=2, names.arg = freq[1:15,]$word,
        col="lightblue", main="Top 15 most frequent words from Taco Bell",
        ylab = "Word frequencies")

cloud_menustat <- rquery.wordcloud(x=menu$item_name, type="text",
                                   lang="english", min.freq = 2, max.words = 200)
freq <- cloud_menustat$freqTable
barplot(freq[1:15,]$freq, las=2, names.arg = freq[1:15,]$word,
        col="lightblue", main="Top 15 most frequent words from MenuStat",
        ylab = "Word frequencies")
rm(rquery.wordcloud, cloud_menustat, cloud_tb, freq)

### fuzzy matching, jaccard distance ----
colnames(menu)[2] <- "full"

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
end_time - start_time # approx. 55 secs
rm(start_time, end_time)

names(join_jaccard)
join_jaccard <- join_jaccard[, c(2, 5, 7, 3, 6, 1, 4)]
colnames(join_jaccard)[1:2] <- c("full.tb", "full.menustat")
join_jaccard <- join_jaccard[order(join_jaccard$dist.jc, join_jaccard$full.tb), ]

# number of exact matches
length(unique(join_jaccard$full.tb[join_jaccard$dist.jc==0])) #240
length(join_jaccard$full.tb[join_jaccard$dist.jc==0]) #317

# visualize
hist(join_jaccard$dist.jc, breaks = 100,
     main="Distribution of distances",
     xlab = "Jaccard distance, q=1")

# q=2
# re-match menu items that did not make a dist=0 match in round 1
product2 <- join_jaccard[(join_jaccard$dist.jc!=0)&!duplicated(join_jaccard$full.tb), c(6, 1, 4)]
colnames(product2)[2] <- "full"

join_jaccard2 <- stringdist_join(product2, menu, 
                                by="full",
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", q=2,
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(full.x) %>%
      top_n(1, -dist.jc) #

#names(join_jaccard2)
join_jaccard2 <- join_jaccard2[, c(2, 5, 7, 3, 6, 1, 4)]
colnames(join_jaccard2)[1:2] <- c("full.tb", "full.menustat")
join_jaccard2 <- join_jaccard2[order(join_jaccard2$dist.jc, join_jaccard2$full.tb), ]

length(unique(join_jaccard2$full.tb[join_jaccard2$dist.jc<=0.37])) #426
length(unique(join_jaccard2$full.tb[join_jaccard2$dist.jc>0.37])) #2844

hist(join_jaccard2$dist.jc, breaks = 100,
     main="Distribution of distances for re-matched items",
     xlab = "Jaccard distance, q=2")
rm(product2)

### fuzzy matching, jaro distance ----
join_jw <- stringdist_join(product, menu, 
                           by="full",
                           mode = "left",
                           ignore_case = FALSE, 
                           method = "jw",
                           max_dist = 99, 
                           distance_col = "dist.jw") %>%
      group_by(full.x) %>%
      top_n(1, -dist.jw)

names(join_jw)
join_jw <- join_jw[, c(2, 5, 7, 3, 6, 1, 4)]
colnames(join_jw)[1:2] <- c("full.tb", "full.menustat")
join_jw <- join_jw[order(join_jw$dist.jw, join_jw$full.tb), ]

# visualize
hist(join_jw$dist.jw, breaks = 100,
     main="Distribution of distances",
     xlab = "Jaro distance")

# compare distance results from jaccard (q=1) and jaro
dist.jc <- join_jaccard[!duplicated(join_jaccard$full.tb), c(1, 3)]
dist.jc$method <- "Jaccard"
colnames(dist.jc)[2] <- "dist"

dist.jw <- join_jw[!duplicated(join_jw$full.tb), c(1, 3)]
dist.jw$method <- "Jaro"
colnames(dist.jw)[2] <- "dist"

dist <- rbind(dist.jc, dist.jw)
rm(dist.jc, dist.jw)

# visualize
ggplot(data=dist,
       aes(x=dist, group=as.factor(method), fill=as.factor(method))) +
      geom_histogram(bins=100) +
      labs(title="Distribution of distances, Jaccard vs. Jaro",
           x="Distance",
           y="Frequency", fill="Distance measure",
           caption="Data source: Taco Bell") +
      #scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/product-matching/compare-jaro-vs-jaccard.jpeg", width=20, height=10, unit="cm")
rm(dist)

### merge matched menu items back to product table ----
# re-run lines 19-88
product <- product[!duplicated(product$product), ]

# keep only the product names and ids
# merge with join_jaccard table, identify the items that had exact matches
names(product)
product <- product[, c(1, 4)]
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
           caption="Data source: Taco Bell \nNote: 318 items (8.84%) with exact matches. The percentage is based on number of items sold.") +
      #scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items.jpeg", width=20, height=10, unit="cm")
rm(sales_all)

### clean up drink names, categorize ----
# re-run product cleaning code, lines 19-166
drinks <- product[product$group=="DRINKS", ]
length(unique(drinks$full))
names(drinks)

# strip drink names of size info
# OZ, CENT, SMALL, MEDIUM, LARGE, EXTRA LARGE
drinks$rename <- gsub("[0-9]+", "", drinks$full)
drinks$rename <- gsub("CENT| OZ|OZ |SMALL|MEDIUM|EXTRA LARGE|REGULAR|GALLON|
                      MEGA JUG|LITER|LARGE",
                      "", drinks$rename)
drinks$rename <- trimws(drinks$rename, "both")
drinks$rename <- gsub("UP", "7UP", drinks$rename)
drinks$rename <- gsub("7UPSELL", "UPSELL", drinks$rename)
drinks <- drinks[!grepl("ONION|NACHOS", drinks$rename), ]
length(unique(drinks$rename)) #266

# 3 categories: diet, pepsi & mt dew, other sugary drinks
drinks$category <- ifelse(grepl("DIET|WATER|COFFEE|UNSWEETENED|HOT TEA|BREWED TEA", drinks$rename)
                          &!grepl("SWEET |MOCHA|VANILLA|CARAMEL", drinks$rename), "Low-calorie",
                          ifelse(grepl("PEPSI|BAJA BLAST", drinks$rename),
                                 "Pepsi/Mt. Dew Baja Blast", "Other SSB"))

# export unique drink names and cateogrize
unique_drinks <- drinks[!duplicated(drinks$rename), ]
write.csv(unique_drinks, "data/menu-matching/unique-drinks.csv", row.names = FALSE)

# merge back
cat <- read.csv("data/menu-matching/unique-drinks.csv", stringsAsFactors = FALSE)
cat <- cat[, c(4:5)]
drinks <- merge(drinks, cat, by="rename")
drinks$category.x <- NULL
colnames(drinks)[5] <- "category"
rm(cat, unique_drinks)

table(drinks$category)
#drinks[drinks$category=="Pepsi/Mt. Dew Baja Blast", ]
length(unique(drinks$rename[drinks$category=="Low-calorie"]))
length(unique(drinks$rename[drinks$category=="Pepsi/Mt. Dew Baja Blast"]))
length(unique(drinks$rename[drinks$category=="Other SSB"]))
#write.csv(drinks, "data/menu-matching/drinks.csv", row.names = FALSE)

### match drinks names to sales volume, sugary and otherwise ----
sales_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
#sapply(detail, class)

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
                        
                        sales <- merge(sales, drinks, by.x = "detail_desc", by.y = "product", all = TRUE)
                        sales <- sales[!is.na(sales$category) & !is.na(sales$p_detail), ]
                        #print(paste0("2nd merge done: ", "year ", i, " Q", j))
                        #names(sales)
                        #detial$id <- NULL

                        # collapse all drink sales into 3 categories
                        sales <- aggregate(data=sales, qty~category, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)

sales_all$qty <- ifelse(sales_all$quarter==4, sales_all$qty/16, sales_all$qty/12)

# visualization
# sales, in percentage
ggplot(data=sales_all,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=as.factor(category), col=as.factor(category))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      labs(title="Drink sales, by category",
           x="Year", y="Sales percentage", col="Category",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-pct.jpeg", width=20, height=10, unit="cm")

# sales, in actual volume 
ggplot(data=sales_all,
       aes(x=paste(year, "Q", quarter, sep=""), y=qty,
           group=as.factor(category), col=as.factor(category))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(limits=c(0, 12000000)) +
      labs(title="Mean weekly drink sales volumes, low-calorie vs. SSB",
           x="Year", y="Sales", col="Category",
           caption="Data source: Taco Bell") +
      #scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-volume.jpeg", width=20, height=10, unit="cm")
rm(sales_all)

### matche drinks sales, drive thru vs. others----
sales_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
#sapply(detail, class)

for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                        sales <- read.csv(paste0("data/from-bigpurple/sales-vol-by-product/sales_",
                                                 i, "_Q0", j, "_drive-thru", ".csv"),
                                          stringsAsFactors = FALSE,
                                          col.names = c("p_detail", "occasion", "sales", "qty"))
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
                        
                        sales <- merge(sales, drinks, by.x = "detail_desc", by.y = "product", all = TRUE)
                        sales <- sales[!is.na(sales$category) & !is.na(sales$p_detail) &
                                             sales$occasion!=0, ]
                        #print(paste0("2nd merge done: ", "year ", i, " Q", j))
                        #names(sales)
                        #detial$id <- NULL
                        
                        # collapse all drink sales into 3 categories
                        sales <- aggregate(data=sales, qty~category+occasion, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)

sales_all$qty <- ifelse(sales_all$quarter==4, sales_all$qty/16, sales_all$qty/12)

# visualization
# sales, in percentage
sales_drive <- sales_all[sales_all$occasion==2, ]
ggplot(data=sales_drive,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=as.factor(category), col=as.factor(category))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 0.3)) +
      labs(title="Drive-through drink sales, by category",
           x="Year", y="Sales percentage", col="Category",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-drivethru_pct.jpeg", width=20, height=10, unit="cm")

sales_eatin <- sales_all[sales_all$occasion==1, ]
ggplot(data=sales_eatin,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=as.factor(category), col=as.factor(category))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 0.3)) +
      labs(title="Eat-in drink sales, by category",
           x="Year", y="Sales percentage", col="Category",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-eatin-pct.jpeg", width=20, height=10, unit="cm")

sales_takeout <- sales_all[sales_all$occasion==3, ]
ggplot(data=sales_takeout,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=as.factor(category), col=as.factor(category))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 0.3)) +
      labs(title="Takeout drink sales, by category",
           x="Year", y="Sales percentage", col="Category",
           caption="Data source: Taco Bell") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-takeout-pct.jpeg", width=20, height=10, unit="cm")

rm(sales_drive, sales_eatin, sales_takeout)

### 



