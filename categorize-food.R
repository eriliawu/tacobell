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
                           ifelse(grepl("NACHOS|BURGERS|TORTAS",product$group)|grepl("NACHO|BURGER|TORTA|PIZZA|CHICKEN|BEEF|STEAK|QUESADILLA|MEXIMELT|CRUNCHWRAP",product$full),"other_entree",
                           ifelse(grepl("EXTRA/MINU",product$group)|grepl("SUB ",product$full),"substitution",
                           ifelse(grepl("SIDES|FRIES",product$group)|grepl("FRIES|FRY",product$full),"side","other"))))))))
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
                              ifelse(product$category=="dessert",3,ifelse(product$category=="other_entree",4,
                              ifelse(product$category=="salad",5,ifelse(product$category=="side",6,
                              ifelse(product$category=="substitution",7,
                              ifelse(product$category=="taco",8,9))))))))
table(product$dw_category)
names(product) <- toupper(names(product))
product <- product[,-5]
#write.csv(product,"data/upload-to-bigpurple/product-category.csv",row.names = FALSE)

### clean order-type data ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-category-occasion/mean-calorie-by-group-occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("monthno","restid","category","occasion","calorie","fat","carb","protein",
                                     "sat_fat","sugar","fiber","sodium","count"))
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
                           col.names=c("monthno","restid","category","occasion","calorie","fat","carb","protein",
                                       "sat_fat","sugar","fiber","sodium","count"))
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
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(13:21)] #delete transaction data related to overall calorie info
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
                    data = category%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
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

ggplot(data=tidy_mod.factor_all,aes(x=month,y=diff,group=as.character(category),color=as.character(category))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-100, ymax=50, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-75, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,50),breaks=seq(-100,50,10)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, by food category", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \nNot included in the analysis: condiments, desserts, side dishes and foods that could not be categorized.") + 
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
tmp1 <- tidy_mod.factor_all %>% group_by(category) %>%
  filter(month>=-30&month<0) %>% dplyr::select(month, diff,category) %>%
  mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_all %>% 
  filter(month>=1&month<=30) %>% dplyr::select(month, diff,category) %>%
  arrange(month) %>% rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by=c("month","category")) %>% group_by(category,month) %>% arrange(category,month) %>%
  mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
rm(tmp1,tmp2)
#hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
p <- NULL
for (i in c(1,2,4:6,9)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = category%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
                    index = "id_match", weights = weights, model = "within")
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  p <- rbind(p,tmp)
}
trend <- cbind(trend, p)
rm(i,j,tmp,p,presum)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(category))) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=factor(category))) +
  ggplot2::annotate(geom="label", x=6, y=-50, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-50),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,50),breaks=seq(-100,50,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Diff-in-diff analysis, by food category", x="Month", y="Calories", 
       caption="Pre-period difference is the mean of accumulated difference between month -3 and -8. \nPost-period difference is the difference between treatment and comparison groups of that month. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entry","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/diff-in-diff-by-category.jpeg", dpi="retina")

### calorie trend by category ----
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
calorie <- calorie %>%
  mutate(calorie = calorie/count) %>%
  filter(occasion==2) %>%
  group_by(monthno,category) %>%
  summarise(mean_cal = mean(calorie),mean_count=mean(count)) %>%
  filter(!is.na(category))

time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
time <- time %>% dplyr::select(7,17,38) %>% setNames(c("monthno", "year", "month")) %>%
  mutate(year = as.integer(substr(year, 2, 5))) %>%
  mutate(month = as.integer(substr(month, 6, 7))) %>%
  filter(year>=2006) %>% distinct()
calorie <- merge(calorie,time,by="monthno")
rm(time)

ggplot(data=calorie%>%filter(!grepl("3|7|8|10",category)),
       aes(x=interaction(year,month,lex.order = TRUE), y=mean_cal,color=factor(category),group=factor(category))) +
  geom_line() + 
  ggplot2::annotate(geom="text",x=1:106,y=185,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=160,label=unique(calorie$year),size=3) +
  geom_vline(xintercept = 50, color="grey",linetype="dashed") +
  geom_vline(xintercept = 62, color="grey",linetype="dashed") +
  coord_cartesian(ylim=c(200,1000), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(0,1000),breaks=seq(0,1000,100)) + #important to set ylim here to include text labels
  labs(title="Mean calorie per order, by food category",x="",y="Calories",
       caption="Categories not included: Desserts, condiments, substitutions and foods that do not fall into any category.") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entry","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/mean-calorie-trend-by-category.jpeg", dpi="retina")

### sales trend, by category ----
sample07q1 <- read.csv("data/from-bigpurple/sales-by-category/sales-by-category_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("monthno","category","occasion","qty"))
sample07q1$qty <- sample07q1$qty/2 
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/sales-by-category/sales-by-category_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("monthno","category","occasion","qty"))
        sales <- rbind(sales, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
sales <- rbind(sales, sample07q1)
rm(i,j,sample,sample07q1)
names(sales)
sales <- sales %>% filter(occasion==2) %>%
  dplyr::select(-occasion) %>%
  group_by(monthno,category) %>% summarise(qty = sum(qty)) %>%
  mutate(total = sum(qty)) %>% mutate(pct = qty/total)

time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
time <- time %>% dplyr::select(7,17,38) %>% setNames(c("monthno", "year", "month")) %>%
  mutate(year = as.integer(substr(year, 2, 5))) %>%
  mutate(month = as.integer(substr(month, 6, 7))) %>%
  filter(year>=2006) %>% distinct()
sales <- merge(sales, time, by="monthno") %>% filter(complete.cases(.))
rm(time)

ggplot(data=sales%>%filter(category<=5|category==8),
       aes(x=interaction(year,month,lex.order = TRUE), y=pct,color=factor(category),group=factor(category))) +
  geom_line() + 
  ggplot2::annotate(geom="text",x=1:106,y=-0.01,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.03,label=unique(sales$year),size=3) +
  geom_vline(xintercept = 50, color="grey",linetype="dashed") +
  geom_vline(xintercept = 62, color="grey",linetype="dashed") +
  coord_cartesian(ylim=c(0,0.5), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,0.05),labels=scales::percent) + #important to set ylim here to include text labels
  labs(title="Sales by food category, drive-thru only",x="Time",y="Percent of sales",
       caption = "Categories not included: condiments, substitutions and foods that do not fall into any category.") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Other entree","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/by-category/sales-by-category.jpeg", dpi="retina")

### appendix figure 4B, diff in diff ----
tidy_mod.factor_all <- NULL
for (i in c(1:5,8)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = category%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
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

tmp1 <- tidy_mod.factor_all %>% group_by(category) %>%
  filter(month>=-30&month<0) %>% dplyr::select(month, diff,category) %>%
  mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_all %>% 
  filter(month>=1&month<=30) %>% dplyr::select(month, diff,category) %>%
  arrange(month) %>% rename(post_mean = diff)
trend <- merge(tmp1,tmp2,by=c("month","category")) %>% group_by(category,month) %>% arrange(category,month) %>%
  mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
rm(tmp1,tmp2)
#hypothesis testing
presum <- "treat:relative2.factor-4 + treat:relative2.factor-5 + treat:relative2.factor-6 + treat:relative2.factor-7 + treat:relative2.factor-8"
p <- NULL
for (i in c(1:5,8)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = category%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
                    index = "id_match", weights = weights, model = "within")
  tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")
  for (j in 4:29) {
    tmp$p[j-1] <- linearHypothesis(mod.factor, paste0(presum," = 6*treat:relative2.factor",j))[2,4]
  }
  p <- rbind(p,tmp)
}
trend <- cbind(trend, p)
rm(i,j,tmp,p,presum)

ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean, color=factor(category))) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean, color=factor(category))) +
  ggplot2::annotate(geom="label", x=6, y=-50, label="   p<0.05", size=3) + 
  geom_point(aes(x=5.35,y=-50),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-100,25),breaks=seq(-100,25,25)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="", x="Month", y="Calories") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Other entree","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig4B-diff-in-diff-by-category.jpeg", dpi="retina")

### appendix figure 4A, sales trend, by category ----
tmp <- category %>% filter(category<=6|category==8) %>%
  group_by(year,month,category) %>% summarise(count = sum(count)) %>%
  group_by(year,month) %>%
  mutate(total = sum(count)) %>% mutate(pct = count/total) %>% filter(!is.na(year))

ggplot(data=tmp%>%filter(category<=5|category==8),
       aes(x=interaction(year,month,lex.order = TRUE), y=pct,color=factor(category),group=factor(category))) +
  geom_line() + 
  ggplot2::annotate(geom="text",x=1:106,y=-0.01,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.03,label=unique(tmp$year),size=3) +
  coord_cartesian(ylim=c(0,0.5), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,0.05),labels=scales::percent) + #important to set ylim here to include text labels
  labs(title="",x="Time",y="Percent of sales") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Other entree","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("manuscript/figures/appendix-fig4A-sales-by-category.jpeg", dpi="retina")

