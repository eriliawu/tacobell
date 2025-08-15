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
menu <- menu[!duplicated(menu$item_name), c(1, 4:5, 8)]
length(unique(menu$item_name))

# remove signs
menu$item_name <- gsub(", ", " ", menu$item_name)

#menu <- menu[order(menu$id), ]
#write.csv(menu, "data/menustat/menustat_items.csv", row.names = FALSE)

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
# a default q=1
start_time <- Sys.time()
join_jaccard <- stringdist_join(product, menu, 
                by=c("full"="item_name"),
                mode = "left",
                ignore_case = FALSE, 
                method = "jaccard", #q=1
                max_dist = 99, 
                distance_col = "dist.jc") %>%
      group_by(full) %>%
      top_n(1, -dist.jc)
end_time <- Sys.time()
end_time - start_time 
rm(start_time, end_time)

names(join_jaccard)
join_jaccard <- join_jaccard[, c(2, 5, 8,7, 3, 1, 6, 4)]
join_jaccard <- join_jaccard[order(join_jaccard$dist.jc, join_jaccard$full), ]
#write.csv(join_jaccard, "data/menu-matching/matching-jaccard1_id.csv", row.names = FALSE)

# number of exact matches
length(unique(join_jaccard$full[join_jaccard$dist.jc==0])) #240
length(join_jaccard$full[join_jaccard$dist.jc==0]) #317

# visualize
hist(join_jaccard$dist.jc, breaks = 100,
     main="Distribution of distances",
     xlab = "Jaccard distance, q=1")

# q=2
# re-match menu items that did not make a dist=0 match in round 1
product2 <- join_jaccard[(join_jaccard$dist.jc!=0)&!duplicated(join_jaccard$full), c(1, 4, 5)]

join_jaccard2 <- stringdist_join(product2, menu, 
                                by=c("full"="item_name"),
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "jaccard", q=2,
                                max_dist = 99, 
                                distance_col = "dist.jc") %>%
      group_by(full) %>%
      top_n(1, -dist.jc) #

names(join_jaccard2)
join_jaccard2 <- join_jaccard2[, c(1, 5, 7, 2, 3, 6, 4)]
join_jaccard2 <- join_jaccard2[order(join_jaccard2$dist.jc, join_jaccard2$full), ]
#write.csv(join_jaccard2, "data/menu-matching/matching-jaccard2.csv", row.names = FALSE)

length(unique(join_jaccard2$full[join_jaccard2$dist.jc<=0.37])) #426
length(unique(join_jaccard2$full[join_jaccard2$dist.jc>0.37])) #2844

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
# manually search join_jaccard and join_jaccard2 tables
# whether the matches are actually matches
# re-run lines 19-88
product <- product[!duplicated(product$product), ]

# keep only the product names and ids
# merge with join_jaccard table, identify the items that had exact matches
names(product)
product <- product[, c(1, 4)]
product <- merge(product, join_jaccard,by = "product", all = TRUE)
product <- product[product$dist.jc==0, c(1:2)]

### check sales volume represented by exact match items where dist.jc=0 in join_jaccard ----
# build empty shell for summary stats
sales_all <- data.frame(matrix(data=NA, nrow=(9*4-1), ncol = 4),
                        stringsAsFactors = FALSE)
colnames(sales_all)[1:4] <- c("year", "quarter", "sales", "matched")

detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
sapply(detail, class)

match1 <- read.csv("data/menu-matching/archive/matching-jaccard1.csv", stringsAsFactors = FALSE)
match2 <- read.csv("data/menu-matching/archive/matching-jaccard2.csv", stringsAsFactors = FALSE)
names(match1)
names(match2)
match1 <- match1[, c(1, 4:6)]
match1 <- match1[!is.na(match1$match), ]
match2 <- match2[, c(1, 3, 5:6)]
match2$match <- ifelse(is.na(match2$match), 0, match2$match)
match <- rbind(match1, match2)
rm(match1, match2)
match <- match %>%
      group_by(full, match) %>%
      mutate(match2=max(match))
length(unique(match$product))
match <- match[!duplicated(match$product), ]
match <- match[, 4:5]
colnames(match)[2] <- "match"

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
                        
                  sales <- merge(sales, match, by.x = "detail_desc", by.y = "product", all = TRUE)
                  sales <- sales[!is.na(sales$p_detail), ]
                  
                  # delete duplicated rows
                  sales <- sales[!duplicated(sales$p_detail), ]

                  # fill in summary stats
                  sales_all[j+4*(i-2007), 1] <- i
                  sales_all[j+4*(i-2007), 2] <- j
                  sales_all[j+4*(i-2007), 3] <- sum(sales$qty, na.rm = TRUE)
                  sales_all[j+4*(i-2007), 4] <- sum(sales$qty[sales$match==1], na.rm = TRUE)    
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
      labs(title="Percent of sales with matched items",
           x="Year", y="Percent",
           caption="Note: 604 items were matched (17%). The percentage is based on number of items sold.") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items.jpeg", width=20, height=10, unit="cm")

### check sales volume of manually matched items, work by RAs ----
# re-run sections: read product data, clean house sections, extract all substings
# read match data
match <- read.csv("data/menu-matching/manual-match/RA/manual-matching-manual-research_full.csv",
                  stringsAsFactors = FALSE)
names(match)
match$tacobell <- trimws(match$tacobell, "both")
match$menustat <- trimws(match$menustat, "both")
match <- match[!duplicated(match$tacobell), ]

# merge back to product table
# to have actual product names to be merged to sales data
match <- merge(match, product, by.x="tacobell", by.y="full")
names(match)
match$match[match$match=="proxy ms"] <- "proxy"

# check sales volume by each item
sales_all <- NULL
detail <- product[, c(1:2, 4)] #rerun lines thru 92
colnames(detail)[2] <- "p_detail"

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
                        sales <- merge(detail, sales, by="p_detail")

                        # clean house
                        sales <- merge(sales, match, by="product", all=TRUE)
                        sales$qty[is.na(sales$qty)] <- 0
                        sales$match[is.na(sales$match)] <- "no match"
                        sales <- sales[!duplicated(sales), ]

                        # aggregate sales by match and full columns
                        sales <- aggregate(data=sales, qty~match, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)

table(sales_all$match)
# visualization, sales volume over time, match vs. non-match
ggplot(data=sales_all, aes(x=interaction(year, quarter, lex.order = TRUE), y=pct,
                      color=as.factor(match), group=as.factor(match))) +
      geom_point() +
      geom_line(size=1) +
      ggplot2::annotate(geom="text", x=1:35, y=-0.01, label=c(rep(c(1:4),8), c(1:3)), size = 4) + #create 2-layer x axis label
      ggplot2::annotate(geom="text", x=2.5+4*(0:8), y=-0.03, label=unique(sales_all$year), size=5) +
      coord_cartesian(ylim = c(0, 0.7), expand = FALSE, clip = "off") + 
      scale_y_continuous(labels = scales::percent, breaks=seq(0.1, 0.7, 0.05)) +
      labs(title="Percent of sales matched over time", x="Year", y="Percent") +
      scale_color_discrete(name="Match",
                           labels=c("Best match, MenuStat, from yes list (n=496)", "Internet (n=36)",
                                    "Best match, MenuStat, correct mistake by RA, maybe list (n=96)",
                                    "Non-best match, MenuStat (n=110)", "No match (n=2,744)", "Proxy (n=35)")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust = -15, size = 12),
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14))
#ggsave("tables/product-matching/sales-match-ra-manual-search.jpeg", width=20, height=10)

### clean up drink names, re-categorize and identify sizes ----
# re-run product cleaning code, lines 19-166
drinks <- product[product$group=="DRINKS", ]
length(unique(drinks$full))
names(drinks)

# strip drink names of size info
# OZ, CENT, SMALL, MEDIUM, LARGE, EXTRA LARGE
drinks$rename <- gsub("[0-9]+", "", drinks$full)
drinks$rename <- gsub("CENT| OZ|OZ |SMALL|MEDIUM|EXTRA LARGE|REGULAR|GALLON|MEGA JUG|
                      LITER|LARGE", "", drinks$rename)
drinks$rename <- trimws(drinks$rename, "both")
drinks$rename <- gsub("UP", "7UP", drinks$rename)
drinks$rename <- gsub("7UPSELL", "UPSELL", drinks$rename)
drinks$rename <- gsub("UPSELL", "", drinks$rename)
drinks <- drinks[!grepl("ONION|NACHOS", drinks$rename), ]
length(unique(drinks$rename)) #239

# export unique drink names and cateogrize
#unique_drinks <- drinks[!duplicated(drinks$rename), ]
#write.csv(unique_drinks, "data/menu-matching/unique-drinks2.csv", row.names = FALSE)

# merge back
cat <- read.csv("data/menu-matching/unique-drinks.csv", stringsAsFactors = FALSE)
cat <- cat[, c(4:8)]
drinks <- merge(drinks, cat, by="rename")
rm(cat)
drinks$rename <- NULL
colnames(drinks)[4] <- "rename"
table(drinks$category2)

# identify drink sizes
drinks$size <- ifelse(grepl("SMALL|12 OZ|16 OZ|9 OZ|14 OZ", drinks$full), "small",
                  ifelse(grepl("MEDIUM|20 OZ|24 OZ|REGULAR|18 OZ", drinks$full), "medium",
                  ifelse(grepl("EXTRA LARGE|MEGA JUG|40 OZ|44 OZ|42 OZ|GALLON|2 LITER", drinks$full), "xl",
                  ifelse(grepl("LARGE|30 OZ|32 OZ", drinks$full), "large",
                  ifelse(grepl("ADD |ADDITIVE", drinks$full), "additive", "unclear")))))

# descriptives on types of drinks
length(unique(drinks$rename)) #226
length(unique(drinks$rename[drinks$category2=="Additive"])) #11
length(unique(drinks$rename[drinks$category2=="Alcohol"])) #15
length(unique(drinks$rename[drinks$category2=="Vague"])) #8
length(unique(drinks$rename[drinks$category2=="Diet"])) #7
length(unique(drinks$rename[drinks$category2=="Low-cal"])) #19
length(unique(drinks$rename[drinks$category2=="Freeze"])) #90
length(unique(drinks$rename[drinks$category2=="SSB fountain"])) #50
length(unique(drinks$rename[drinks$category2=="Other SSB"])) #26

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
                        sales <- merge(detail, sales, by="p_detail", all=TRUE)
                        #print(paste0("1st merge done: ", "year ", i, " Q", j))
                        
                        sales <- merge(sales, drinks, by="product", all = TRUE)
                        sales <- sales[!is.na(sales$category) & !is.na(sales$p_detail), ]

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

# % of SSB sales in 2015 Q1
q1 <- sales_all[sales_all$year==2015&sales_all$quarter==1, ]
q1 <- aggregate(data=q1, pct~category, sum)
sum(q1$pct[q1$category!="Diet soda"&q1$category!="Alcohol"&q1$category!="Water/coffee/tea"])
sum(q1$pct[q1$category=="Freeze"|q1$category=="Other SSB"|
                 q1$category=="Pepsi/MDBB"|q1$category=="0.022332915"]) #0.57
rm(q1)

# visualization
# sales, in percentage
ggplot(data=sales_all,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct, group=category, col=category)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
      labs(title="Drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category",
           caption="Note: y-axis represents % of sales over all sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-pct-size.jpeg", width=10, height=6, unit="in", dpi=600)
rm(sales_all)

### match drinks sales, drive thru vs. others----
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
                        # collapse all drink sales into 3 categories
                        sales <- aggregate(data=sales, qty~category2+occasion+fountain+size, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)

                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)
sum(sales_all$pct[sales_all$year==2015&sales_all$quarter==2])
sales_all$qty <- ifelse(sales_all$quarter==4, sales_all$qty/16, sales_all$qty/12)

# % of ssb sales in 2015 q1, drive-thru only
q1 <- sales_all[sales_all$year==2015&sales_all$quarter==1&sales_all$occasion==2, ]
q1$pct <- q1$qty / sum(q1$qty)
q1 <- aggregate(data=q1, pct~category2, sum)
sum(q1$pct[q1$category=="Freeze"|q1$category=="Other SSB"|
                 q1$category=="SSB fountain"]) #0.82
rm(q1)

# % of ssb in 2015 q1, drive-thru only, by each taxed restaurant
q1 <- read.csv("data/from-bigpurple/sales-vol-by-product/sales_2015_Q01_by_restid.csv")
names(q1)
colnames(q1) <- c("restid", "product", "occasion", "qty")
#q1 <- q1[q1$occasion==2, ]
q1 <- merge(q1, detail, by.x ="product", by.y = "p_detail")
drinks <- drinks[!duplicated(drinks$product),]
q1 <- merge(q1, drinks, by.x = "detail_desc", by.y = "product")
soda <- read.csv("../soda-tax/soda-tax-list.csv")
names(soda)
table(soda$state)
soda <- soda[, c(1:3,6:7)]
soda <- merge(q1, soda, by="restid")
soda$ssb <- ifelse(grepl("Freeze|Other SSB|SSB fountain", soda$category2), 1, 0)
soda <- aggregate(data=soda, qty~restid+ssb, sum)
soda <- soda[order(soda$restid),]
tmp <- aggregate(data=soda, qty~restid, sum)
soda <- merge(soda, tmp, by="restid")
colnames(soda)[3:4] <- c("qty", "total")
soda$pct <- soda$qty/soda$total
hist(soda$pct[soda$ssb==1], breaks=68, main="Variation of SSB sales, taxed restaurants",
     xlab = "% of drink sales from SSB", ylab = "Frequency")
summary(soda$pct[soda$ssb==1])

# % offountain drink sales in 2015 q1, ssb or otherwise
q1 <- sales_all[sales_all$occasion==2&sales_all$year==2015&sales_all$quarter==1, ]
q1 <- aggregate(data=q1, qty~category+fountain, sum)
q1$pct <- q1$qty / sum(q1$qty)
sum(q1$pct[q1$fountain==1]) #79%
sum(q1$pct[q1$fountain==1&
                 (q1$category=="Freeze"|q1$category=="Other SSB"|q1$category=="Sweetened coffee/tea"|
                        q1$category=="Pepsi/Mt. Dew Baja Blast")]) / sum(q1$pct[q1$fountain==1]) #88%
rm(q1)

# sugary fountain drink sales, size comparison, 2015 q1
# re-run the for loop with category2 in the aggregate formula
q1 <- sales_all[sales_all$year==2015&sales_all$quarter==1&sales_all$occasion==2&
                      sales_all$fountain==1&(sales_all$category2!="Diet"&sales_all$category2!="Low-cal"), ]
q1 <- aggregate(data=q1, qty~size, sum)
q1$pct <- q1$qty/sum(q1$qty)

# visualization
# sales, in percentage, drive-thru, eat-in and takeout
ggplot(data=subset(sales_all, sales_all$occasion==2),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Drive-through drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all drive-through sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-drivethru_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)

ggplot(data=subset(sales_all, sales_all$occasion==1),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Eat-in drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all eati-in sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-eatin_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)

ggplot(data=subset(sales_all, sales_all$occasion==3),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Takeout drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all takeout sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-takeout_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)

# look at drinks sales in drive-thru only
sales_all <- sales_all[sales_all$occasion==2&sales_all$year==2015&sales_all$quarter==1, ]
sum(sales_all$qty[sales_all$fountain==1]) / sum(sales_all$qty)
sum(sales_all$qty[sales_all$fountain==1&sales_all$category!="Diet soda"]) / sum(sales_all$qty[sales_all$fountain==1])

### match drink sales, overall, drive thru vs. others ----
sales_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
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
                        sales <- aggregate(data=sales, qty~size, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, sales)
sales_all$qty <- ifelse(sales_all$quarter==4, sales_all$qty/16, sales_all$qty/12)

# visualization
# sales, in percentage
ggplot(data=sales_all,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=as.factor(size), col=as.factor(size))) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      labs(title="Drink sales, by size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all sales.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-pct-size.jpeg", width=10, height=6, unit="in", dpi=600)
rm(sales_all)

# visualization
# sales, in percentage, drive-thru, eat-in and takeout
ggplot(data=subset(sales_all, sales_all$occasion==2),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Drive-through drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all drive-through sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/product-matching/drink-sales-drivethru_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)

ggplot(data=subset(sales_all, sales_all$occasion==1),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Eat-in drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all eati-in sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-eatin_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)

ggplot(data=subset(sales_all, sales_all$occasion==3),
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(category)),
           col=as.factor(category))) +
      geom_point(size=0.5) +
      geom_line(size=0.5, aes(linetype=as.factor(size))) +
      scale_y_continuous(labels = scales::percent, limits=c(0.01, 1)) +
      labs(title="Takeout drink sales, by category and size",
           x="Year", y="Sales percentage", col="Category", linetype="Size",
           caption="Note: y-axis represents % of sales over all takeout sales. Sales of less than 1% were excluded.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-takeout_pct_size.jpeg", width=10, height=6, unit="in", dpi=600)
### looking at the sales by drive-thru vs. others ----
sales_all <- NULL
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
                        # collapse all drink sales into 3 categories
                        sales <- aggregate(data=sales, qty~occasion+size, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)

                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)
sum(sales_all$pct[sales_all$year==2015&sales_all$quarter==2])
sales_all$qty <- ifelse(sales_all$quarter==4, sales_all$qty/16, sales_all$qty/12)
sales_all$occasion <- ifelse(sales_all$occasion==2, "drive-through",
                             ifelse(sales_all$occasion==1, "eat-in", "takeout"))

# visualize, drink sales by size and occasion
ggplot(data=sales_all,
       aes(x=paste(year, "Q", quarter, sep=""), y=pct,
           group=interaction(as.factor(size), as.factor(occasion)),
           col=as.factor(size))) +
      geom_point(size=0.5) +
      geom_line(aes(linetype=as.factor(occasion))) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 0.5)) +
      labs(title="Drive-through drink sales, by category and size",
           x="Year", y="Sales percentage", col="Size", linetype="Occasion",
           caption="Note: y-axis represents % of sales over all drink sales.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/drink-sales-pct-size.jpeg", width=10, height=6, unit="in", dpi=600)

### descriptives for soda tax grant ----
# mean number of items purchased per order, mean spending
spending_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else {
                        sales <- read.csv(paste0("data/from-bigpurple/sales-vol-by-product/sales_",
                                                 i, "_Q0", j, ".csv"),
                                          sep = ";", header = FALSE, quote = "\"'",
                                          stringsAsFactors = FALSE,
                                          col.names = c("p_detail", "sales", "qty"))
                        sales <- merge(detail, sales, by="p_detail", all=TRUE)
                        
                        # clean house
                        sales <- sales[!is.na(sales$sales), ]
                        sales <- sales[!grepl("AWR", sales$detail_desc)&!grepl("AW ", sales$detail_desc)&
                                             !grepl("BYB", sales$detail_desc)&!grepl("KFC", sales$detail_desc)&
                                             !grepl("LJS", sales$detail_desc)&!grepl("PH", sales$detail_desc)&
                                             !grepl("PIZZA HUT", sales$detail_desc)&!grepl("TCBY", sales$detail_desc)&
                                             !grepl("ICBIY", sales$detail_desc)&!grepl("KRYSTAL", sales$detail_desc), ]
                        # aggregate mean spending
                        sales <- sales[sales$p_detail>0, ]
                        sales$year <- i
                        sales$quarter <- j
                        spending <- aggregate(data=sales, sales~year+quarter, sum)
                        
                        # more cleaning for mean num of items purchased per order
                        sales <- sales[sales$detail_desc!="CFM MANAGER SPECIALS"&
                                             sales$detail_desc!=""&
                                             !grepl("* NEW PRODCT ADDED BY", sales$detail_desc)&
                                             !grepl("COMBO", sales$detail_desc)&
                                             !grepl("FRANCHISE LOCAL MENU", sales$detail_desc)&
                                             sales$detail_desc!="NEW ITEM"&
                                             !grepl("SPECIAL PROMOTION", sales$detail_desc), ]
                        sales <- sales[sales$detail_desc!="TB I'M ALL EARS"&sales$detail_desc!="SPECIAL"&
                                             sales$detail_desc!="DO NOT ALTER THIS ITEM"&
                                             sales$detail_desc!="BORDER SWEAT SHIRT"&
                                             sales$detail_desc!="TB I'M THINKING YOU ME"&
                                             sales$detail_desc!="CFM DOWNLOAD 1"&
                                             sales$detail_desc!="TB HELLO FRIEND"&
                                             sales$detail_desc!="CANADA BATMAN CUP INDIVIDUAL"&
                                             sales$detail_desc!="DELETED ITEM, DO NOT USE"&
                                             sales$detail_desc!="CLEV INDIANS/TB BANDANNA 1.4"&
                                             sales$detail_desc!="CFM DOWNLOAD 2"&
                                             sales$detail_desc!="TB I'M THINKING YOU ME DINNER"&
                                             sales$detail_desc!="CANADA BATMAN CUP W/PURCHASE"&
                                             sales$detail_desc!="TB HELLO FRIEND", ]
                        
                        # aggregate mean num of items purchased
                        num <- aggregate(data=sales, qty~year+quarter, sum)
                        spending <- merge(spending, num, by=c("year", "quarter"))
                        spending_all <- rbind(spending_all, spending)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales, num, spending)

### mean calories in medium and large drinks ----
# re-read menustat data
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
menu$item_name <- toupper(menu$item_name)
names(menu)
table(menu$category)
menu <- menu[menu$category=="Beverages"&menu$year==2015, ]
table(menu$serving_size)
mean(menu$calories[menu$calories!=0 & menu$serving_size==20]) #235
mean(menu$calories[menu$calories!=0 & menu$serving_size==30]) #350
