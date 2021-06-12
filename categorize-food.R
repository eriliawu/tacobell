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
### clean order-type data ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("yearno", "monthno","restid","category","occasion","calorie","count"))
sample07q1$calorie <- sample07q1$calorie/2 
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("yearno", "monthno","restid","category","occasion","calorie","count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)
calorie <- calorie[calorie$occasion==2,]

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,calorie,by="restid")

# aggregate on address, instead of restid
restaurant$restid <- NULL
restaurant$yearno <- NULL
restaurant$ownership <- ifelse(restaurant$ownership=="COMPANY",1,0)
restaurant$concept <- ifelse(restaurant$concept=="TBC",1,0)
restaurant <- aggregate(data=restaurant, .~address+concept+ownership+monthno+category, sum)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant$calorie <-restaurant$calorie/restaurant$count
restaurant$occasion <- NULL

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(13:14,16:22)] #delete transaction data related to overall calorie info
colnames(matched)[13] <- "calorie_overall"
category <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
rm(calorie,restaurant,matched)

### preparing data, order-type ----
category$tract_num <- substr(category$tract_num, 2, 12)
category <- category %>%
  filter(complete.cases(category)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  dplyr::select(id, address:count) %>%
  arrange(id, category, monthno) %>% #find out for how many months a restaurant was open
  group_by(id, treat, match_place, category) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(category$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
category$relative <- category$monthno - category$entry +1
category$relative2 <- category$monthno - category$entry #month 0 is first month of ML
category$post <- ifelse(category$relative2<0, 0, 1)

# month as relative and factor
# set month 1 as ref group
category$relative.factor <- factor(category$relative)
category <- within(category, relative.factor<-relevel(relative.factor, ref="1"))
summary(category$relative)
category$relative2.factor <- factor(category$relative2)
category <- within(category, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
category <- category %>%
  group_by(id, treat, match_place, post,category) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
category$open_before <- ifelse(category$post==0, category$open_before, category$open_month-category$open_before)
category$open_after <- ifelse(category$post==1, category$open_after, category$open_month-category$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- category %>%
  group_by(id, treat, match_place,category) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24,category) %>%
  distinct()
category <- merge(category, tmp, by=c("id", "treat", "match_place","category"), all = TRUE)
tmp <- category %>%
  group_by(id, treat, match_place,category) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24,category) %>%
  distinct()
category <- merge(category, tmp, by=c("id", "treat", "match_place","category"), all = TRUE)
category$open6 <- ifelse(category$before6==1&category$after6==1,1,0)
category$open12 <- ifelse(category$before12==1&category$after12==1,1,0)
category$open18 <- ifelse(category$before18==1&category$after18==1,1,0)
category$open24 <- ifelse(category$before24==1&category$after24==1,1,0)
rm(tmp)

category <- within(category, relative2.factor<-relevel(relative2.factor, ref="-3"))
category$id_match <- paste0(category$id, category$match_place)

### main analysis, calorie=treat*month, order type ----
tidy_mod.factor_all <- NULL
for (i in c(1,2,4:6,9)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = category%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==6), 
                    index = "id_match", weights = weights, model = "within")
  tidy_mod.factor <- tidy(mod.factor)
  tidy_mod.factor <- tidy_mod.factor %>%
    dplyr::select(term,estimate,p.value) %>%
    rename(month=term,coef.month=estimate,p=p.value) %>%
    filter(!grepl("as.factor|calorie", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0) %>%
    add_row(month="-3",coef.month=0,group=1) %>%
    mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))
  tidy_mod.factor$category <- i
  tidy_mod.factor_all <- rbind(tidy_mod.factor_all,tidy_mod.factor)
  tidy_mod.factor_all <- tidy_mod.factor_all[!is.na(tidy_mod.factor_all$diff),]
}

ggplot(data=tidy_mod.factor_all,aes(x=month,y=diff,group=factor(category),color=factor(category))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-100, ymax=50, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-75, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,50),breaks=seq(-100,50,10)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, by food category", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entry","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/main-effect-by-category.jpeg", dpi="retina")

### detect trend using diff-in-diff, order type ----
tmp1 <- tidy_mod.factor %>% 
  filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month, diff) %>%
  mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor %>% 
  filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month, diff) %>%
  arrange(month) %>% rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
  mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
rm(tmp1,tmp2)
#hypothesis testing
tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
for (i in 4:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",i))[2,4]
}
trend <- cbind(trend, tmp)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(category))) + 
  #geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color="hotpink", group=1)) +
  #ggplot2::annotate(geom="label", x=6, y=-30, label="   p<0.05", size=3) + 
  #geom_point(aes(x=5.35,y=-30),color="hotpink",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,100),breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by otder type", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + âmonth_1-12 + ârestaurant") + 
  scale_color_manual(name="Ordery type", labels=c("Eat-in","Drive-through","Takeout"),
                     values = c("hotpink","olivedrab3","#13B0E4")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-rematched/mealtime-orderType/diff-in-diff-orderType.jpeg", dpi="retina")


