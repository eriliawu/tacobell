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
library(lme4)
library(plm)
library(lmerTest)
library(stargazer)
library(table1)
library(tableone)
library(broom)
library(car)
library(usmap)
library(maps)
library(car) #testing joint significance
library(zoo)
library(wordcloud)

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
product <- product[,c(2,5:6)]

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
product <- product %>% dplyr::select(-c(4,6))

### clean transaction data, link to restaurant and product information ----
sample07q1 <- read.csv("data/from-bigpurple/sales-by-product-restid/sales-by-product-restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid","monthno","qty","dw_product"))
sample07q1$qty <- sample07q1$qty/2 
qty <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/sales-by-product-restid/sales-by-product-restid_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("restid","monthno","qty","dw_product"))
        qty <- rbind(qty, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
qty <- rbind(qty, sample07q1)
rm(sample, sample07q1, i, j)
qty <- merge(qty,product,by="dw_product",all.x = TRUE)

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,qty,by="restid")

# aggregate on address, instead of restid
restaurant <- restaurant %>% dplyr::select(-restid) %>%
  mutate(ownership = ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(concept = ifelse(concept=="TBC",1,0)) %>%
  group_by(address,concept,ownership,monthno,category,full) %>%
  summarise(qty = sum(qty)) %>%
  distinct() %>%
  group_by(address,concept,ownership,monthno) %>%
  filter(category!="substitution") %>%
  arrange(address,concept,ownership,monthno,desc(qty)) %>%
  mutate(total_qty = sum(qty)) %>%  mutate(pct = qty / total_qty) %>%
  mutate(rank = row_number()) %>% filter(rank<=20) #filter top sales items

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
matched <- matched %>% dplyr::select(address,entry,ownership,concept,weights,match_place,monthno,year,month,treat,count) %>%
  filter(monthno>=entry+6 & monthno<=entry+17)
matched <- merge(matched,restaurant,by=c("address","concept","ownership","monthno"))

# link to calorie information
calorie <- read.csv("data/menu-matching/matched-results/PRODUCT_CALORIE_DIM.csv",stringsAsFactors = FALSE)
calorie <- calorie %>% filter(!is.na(CALORIES)) %>% dplyr::select(FULLDESC,CALORIES) %>%
  rename(full=FULLDESC,calorie=CALORIES) %>% distinct()
matched <- merge(matched,calorie,by="full") %>%
  arrange(address,concept,ownership,monthno,rank) %>%
  mutate(relative2 = monthno - entry) %>%
  relocate(address,match_place,weights,year,month,relative2,full,category,qty,rank,calorie,concept,ownership,entry,treat,count,monthno) %>%
  group_by(address,concept,ownership,monthno) 

rm(product,qty,restaurant,calorie)
### look at top sales items, treated v. comp restaurants; by treated location ----
# what % of sale do top items represent
matched <- matched %>%
  group_by(address,concept,ownership,match_place,monthno) %>%
  mutate(accum_pct = sum(pct[1:10])) %>%
  filter(!is.na(treat))
# accumulative sales % of top 10 items, by treat v. comp
ggplot(matched%>%filter(relative2==11)%>%dplyr::select(address,ownership,concept,monthno,accum_pct,treat)%>%distinct(),
       aes(x=accum_pct, color=factor(treat), fill=factor(treat))) +
  geom_histogram(bins=500,alpha=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_x_continuous(labels = scales::percent,breaks = seq(0,1,0.1)) + #select which months to display
  labs(title="Sales from top 10 items, month 12", x="% of sales from top 10 items", y="Number of restaurants", 
       caption="") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  scale_fill_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 20), #position/size of title
        axis.title.x = element_text(vjust=-3, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/investigate-sales-mon12to14/hist-top-sales.jpeg", dpi="retina")

# sales by category over time
tmp<- matched%>%group_by(treat,relative2,category)%>%
  summarize(qty=sum(qty)) %>% #total sale by category
  group_by(treat,relative2)%>%
  mutate(total_qty = sum(qty)) %>% mutate(pct = qty/total_qty) %>% mutate(relative = relative2+1)
ggplot(data=tmp%>%filter(grepl("beverage|burrito|nacho|entry|taco",category)),
       aes(x=relative, y=pct, color=category,linetype=factor(treat,levels = c(1,0)))) +
  geom_line() + 
  geom_vline(xintercept = 12, color="grey",linetype="dashed") +
  geom_vline(xintercept = 14, color="grey",linetype="dashed") +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_x_continuous(breaks=seq(7,18,1)) + #select which months to display
  scale_y_continuous(limits=c(0,0.8),breaks=seq(0,0.8,0.1),labels=scales::percent) + #important to set ylim here to include text labels
  labs(title="Sales by food category",x="Month",y="Percent of sales", caption="") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entry","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","purple","orange")) + 
  scale_linetype_discrete(name="Menu labeling",labels=c("Yes","No")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/investigate-sales-mon12to14/sales-by-category.jpeg", dpi="retina")

#bar plot, most popular items
ca <- sales_sorted%>%filter(match_place=="ca"&relative2==11)
ggplot(data=ca, aes(x=reorder(full,n), y=n, color=factor(treat,levels = c(1,0)),fill=factor(treat,levels = c(1,0)))) +
  geom_bar(stat="identity", position=position_dodge(),size=0.5) +
  coord_flip(expand = FALSE, clip = "off") +
  scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,200)) +
  scale_color_discrete(name="Menu labeling",labels=c("Yes","No")) +
  scale_fill_discrete(name="Menu labeling",labels=c("Yes","No")) +
  labs(title="Items frequently ranked top 10 sales in stores for California, month 12",x="",y="", caption="") +
  theme(plot.margin = margin(1,1,1,1, "lines"),
        axis.text.y = element_text(size = 8))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/investigate-sales-mon12to14/frequent-top-item-ca12.jpeg", dpi="retina")

#make word cloud
# summarize number of times an item ranked top 10 at any CA treated store, month 12
sales_sorted <- matched %>% filter(rank<=10) %>% group_by(treat,match_place,relative2,full) %>% 
  count(full,sort = TRUE) %>% arrange(desc(treat),match_place,relative2)
#change column n to be %
ca12 <- sales_sorted%>%filter(match_place=="ca"&relative2==11&treat==0)
ca13 <- sales_sorted%>%filter(match_place=="ca"&relative2==12&treat==0)
ca14 <- sales_sorted%>%filter(match_place=="ca"&relative2==13&treat==0)
png("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/investigate-sales-mon12to14/freq-items-wordcloud-ca.comp.jpeg",width = 1466,height = 868)
par(mfrow=c(1,3))
wordcloud(ca12$full,ca12$n,min.freq=1,random.order = FALSE,
          color=brewer.pal(8,"Dark2"),rot.per = 0,scale = c(4,1))
wordcloud(ca13$full,ca13$n,min.freq=1,random.order = FALSE,
          color=brewer.pal(8,"Dark2"),rot.per = 0,scale = c(4,1))
wordcloud(ca14$full,ca14$n,min.freq=1,random.order = FALSE,
          color=brewer.pal(8,"Dark2"),rot.per = 0,scale = c(4,1))
dev.off()
par(mfrow=c(1,1))


place <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
place$relative2 <- place$monthno - place$entry
unique(length(place$address[place$treat==1&place$match_place=="ca"&place$relative2==11])) #1183,353




