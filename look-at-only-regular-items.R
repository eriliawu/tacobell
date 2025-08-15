### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
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

### read sales data for all 35 quarters, get items consistently on sale, all quarters ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale/consistent-for-sale-item_2007_Q1.csv",
                       stringsAsFactors = FALSE, col.names = c("product"))
sample07q1$year <- 2007
sample07q1$quarter <- 1
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/consistent-sale/consistent-for-sale-item_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE, col.names=c("product"))
        sample$year <- i
        sample$quarter <- j
        sales <- rbind(sales, sample)
      }, error=function(e){cat("ERROR:",conditionMessage(e), "\n")}
    )
  }
}
sales <- rbind(sales, sample07q1)
rm(sample, sample07q1, i, j)

# merge with product names
product <- read.csv("data/from-bigpurple/product_detail.csv",stringsAsFactors = FALSE)
sales <- merge(sales,product,by.x = "product",by.y = "p_detail",all.x = TRUE)

# identify items consistently for sale thru all 35 quarters
tmp <- sales %>% rename(DW_PRODUCT=product,product=detail_desc) %>%
  group_by(product) %>% dplyr::select(-DW_PRODUCT) %>% distinct() %>%
  arrange(product,year,quarter) %>% mutate(n=n()) %>%
  mutate(max_year=max(year)) %>% mutate(min_year=min(year)) %>%
  filter(max_year==2015&min_year==2007&n==35) %>% dplyr::select(product) %>% distinct()
#merge the product names back into numeric IDs
sales <- merge(sales,tmp,by.x="detail_desc",by.y="product") %>% dplyr::select(product) %>%
  rename(DW_PRODUCT=product) %>% distinct()
#write.csv(sales,"data/upload-to-bigpurple/consistent-sales.csv",row.names = FALSE)
#next step: join these regular for sale items with the sale data in database
#run the same mean calories query

### read in sales data by category, link to restaurant data ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale-2010onward-forCA/mean-calories-bycategory_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("yearno", "monthno","restid", "category", "calorie", "count"))
sample07q1$calorie <- sample07q1$calorie/2 
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/consistent-sale-2010onward-forCA/mean-calories-bycategory_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("yearno", "monthno","restid", "category", "calorie", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,calorie,by="restid")

# aggregate on address, instead of restid
restaurant <- restaurant %>%
  dplyr::select(-yearno,-restid) %>% 
  mutate(ownership = ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(concept = ifelse(concept=="TBC",1,0)) %>%
  group_by(address,concept,ownership,monthno,category) %>%
  summarise(calorie = sum(calorie), count = sum(count)) %>% distinct() %>%
  mutate(calorie = calorie/count)

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(14:18)] #delete transaction data related to overall calorie info
matched <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
rm(calorie,restaurant)

### prepare data ----
matched <- matched %>%
  filter(complete.cases(matched)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  group_by(id, treat, match_place,category) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(matched$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
matched$relative <- matched$monthno - matched$entry +1
matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
matched$post <- ifelse(matched$relative2<0, 0, 1)

# month as relative and factor
# set month 1 as ref group
matched$relative.factor <- factor(matched$relative)
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="1"))
summary(matched$relative)

matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
matched <- matched %>%
  group_by(id, treat, match_place, post, category) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
matched$open_before <- ifelse(matched$post==0, matched$open_before, matched$open_month-matched$open_before)
matched$open_after <- ifelse(matched$post==1, matched$open_after, matched$open_month-matched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- matched %>%
  group_by(id, treat, match_place, category) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24, category) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place", "category"), all = TRUE)
tmp <- matched %>%
  group_by(id, treat, match_place, category) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24, category) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place", "category"), all = TRUE)
matched$open6 <- ifelse(matched$before6==1&matched$after6==1,1,0)
matched$open12 <- ifelse(matched$before12==1&matched$after12==1,1,0)
matched$open18 <- ifelse(matched$before18==1&matched$after18==1,1,0)
matched$open24 <- ifelse(matched$before24==1&matched$after24==1,1,0)
rm(tmp)

matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))
matched$id_match <- paste0(matched$id, matched$match_place)

### regression, factor model ----
tidy_mod.factor_all <- NULL
for (i in c(1:5,9)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i&match_place=="ca"), 
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
rm(i)

ggplot(data=tidy_mod.factor_all,aes(x=month,y=diff,group=as.character(category),color=as.character(category))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-80, ymax=50, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-60, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-80,50),breaks=seq(-80,50,10)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, CA regular menu items", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \nRestrict to consistently available items between Jan 2010 and the end of study period. There are 119 such items.") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Nacho","Other entree","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","purple","red")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/regular-items-only/main-effect-ca-items.jpeg", dpi="retina")

### average number of items by cat ordered in each purchase; % of orders containing items, by cat ----
sample07q1 <- read.csv("data/from-bigpurple/sales-by-category/pct_order_containing_each_category2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("yearno", "monthno","category","num_orders","qty"))
sample07q1$qty <- sample07q1$qty/2 
count <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/sales-by-category/pct_order_containing_each_category",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("yearno", "monthno","category","num_orders","qty"))
        count <- rbind(count, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
count <- rbind(count, sample07q1)
rm(sample, sample07q1, i, j)
count <- aggregate(data=count, .~monthno+yearno+category, sum)

time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
time <- time %>% dplyr::select(7,17,38) %>% setNames(c("monthno", "year", "month")) %>%
  mutate(year = as.integer(substr(year, 2, 5))) %>%
  mutate(month = as.integer(substr(month, 6, 7))) %>%
  filter(year>=2006) %>% distinct()
count <- merge(count, time, by="monthno") %>% filter(complete.cases(.))
rm(time)

sales <- read.csv("data/from-bigpurple/number-orders-overtime.csv")
sales <- aggregate(data=sales,total_num_orders~DW_MONTH,sum)
sales <- merge(sales,count,by.y="monthno",by.x = "DW_MONTH")
sales <- sales[order(sales$DW_MONTH,sales$category),]
sales$total_num_orders[sales$DW_MONTH==272] <- 66534967
sales$total_num_orders[sales$DW_MONTH==291] <- 37884952
sales$pct <- sales$num_orders / sales$total_num_orders
sales$mean <- sales$qty / sales$total_num_orders

# ave number of items ordered per purchase; e.g. on avg, customers order 2 tacos per purchase
ggplot(data=sales%>%filter(category<=5|category==9),
       aes(x=interaction(year,month,lex.order = TRUE), y=mean,color=factor(category),group=factor(category))) +
  geom_line() + 
  geom_vline(xintercept = 50, color="grey",linetype="dashed") +
  geom_vline(xintercept = 62, color="grey",linetype="dashed") +
  ggplot2::annotate(geom="text",x=1:106,y=-0.075,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.2,label=unique(count$year),size=3) +
  coord_cartesian(ylim=c(0,3), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.5,3),breaks=seq(-0.5,3,0.5)) + #important to set ylim here to include text labels
  labs(title="Number of items per order, by category",x="",y="Number of items",
       caption="") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Nacho","Other entree","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","purple","red")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/ave-num-items-per-order-by-category.jpeg", dpi="retina")

## % of orders containing at least 1 burrito, 1 taco, or 1 drink
ggplot(data=sales%>%filter(category<=5|category==9),
       aes(x=interaction(year,month,lex.order = TRUE), y=pct,color=factor(category),group=factor(category))) +
  geom_line() + 
  geom_vline(xintercept = 50, color="grey",linetype="dashed") +
  geom_vline(xintercept = 62, color="grey",linetype="dashed") +
  ggplot2::annotate(geom="text",x=1:106,y=-0.025,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.075,label=unique(count$year),size=3) +
  coord_cartesian(ylim=c(0,1), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.5,1),breaks=seq(-0.5,1,0.1),labels = scales::percent) + #important to set ylim here to include text labels
  labs(title="% of order containing an item, by category",x="",y="Percent of sales",
       caption="") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Dessert","Nacho","Other entree","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","orange","purple","red")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/pct-order-with-item-by-category.jpeg", dpi="retina")

### read sales data for all 35 quarters, get items consistently on sale thru between jan 2010 onwards, zoom in on CA ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale/consistent-for-sale-item_2007_Q1.csv",
                       stringsAsFactors = FALSE, col.names = c("product"))
sample07q1$year <- 2007
sample07q1$quarter <- 1
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/consistent-sale/consistent-for-sale-item_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE, col.names=c("product"))
        sample$year <- i
        sample$quarter <- j
        sales <- rbind(sales, sample)
      }, error=function(e){cat("ERROR:",conditionMessage(e), "\n")}
    )
  }
}
sales <- rbind(sales, sample07q1)
rm(sample, sample07q1, i, j)

# merge with product names
product <- read.csv("data/from-bigpurple/product_detail.csv",stringsAsFactors = FALSE)
sales <- merge(sales,product,by.x = "product",by.y = "p_detail",all.x = TRUE)
sales <- sales[sales$year>=2010,]

# identify items consistently for sale thru all 35 quarters
tmp <- sales %>% rename(DW_PRODUCT=product,product=detail_desc) %>%
  group_by(product) %>% dplyr::select(-DW_PRODUCT) %>% distinct() %>%
  arrange(product,year,quarter) %>% mutate(n=n()) %>%
  mutate(max_year=max(year)) %>% mutate(min_year=min(year)) %>%
  filter(max_year==2015&min_year==2010&n==23) %>% dplyr::select(product) %>% distinct()
#merge the product names back into numeric IDs
sales <- merge(sales,tmp,by.x="detail_desc",by.y="product") %>% dplyr::select(product) %>%
  rename(DW_PRODUCT=product) %>% distinct()
#write.csv(sales,"data/upload-to-bigpurple/consistent-sales-2010onward-forCA.csv",row.names = FALSE)
#next step: join these regular for sale items with the sale data in database
#run the same mean calories query

### read in sales data, link to restaurant data, for overall or CA ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale/mean-calories_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("yearno", "monthno","restid", "calorie", "count"))
sample07q1$calorie <- sample07q1$calorie/2 
calorie <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
      {
        sample <- read.csv(paste0("data/from-bigpurple/consistent-sale/mean-calories_",
                                  i,"_Q",j,".csv"),
                           stringsAsFactors = FALSE,
                           col.names=c("yearno", "monthno","restid", "calorie", "count"))
        calorie <- rbind(calorie, sample)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

#read in restaurant information
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv",stringsAsFactors = FALSE)
restaurant <- restaurant[,c(1:2,10,14)]
restaurant <- merge(restaurant,calorie,by="restid")

# aggregate on address, instead of restid
restaurant <- restaurant %>%
  dplyr::select(-yearno,-restid) %>% 
  mutate(ownership = ifelse(ownership=="COMPANY",1,0)) %>%
  mutate(concept = ifelse(concept=="TBC",1,0)) %>%
  group_by(address,concept,ownership,monthno) %>%
  summarise(calorie = sum(calorie), count = sum(count)) %>% distinct() %>%
  mutate(calorie = calorie/count)

# read in matched restaurant info
matched <- read.csv("data/calorie-aims/matched-restaurants-trimmed-drive-thru-correct.csv", stringsAsFactors = FALSE)
matched <- matched[, -c(14:18)] #delete transaction data related to overall calorie info
matched <- merge(matched,restaurant,by=c("address","ownership","concept","monthno"))
rm(calorie,restaurant)

### prepare data ----
matched <- matched %>%
  filter(complete.cases(matched)) %>%
  mutate(id = group_indices(., address, tract_num, ownership, concept)) %>%
  group_by(id, treat, match_place) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_month=max(rank))
summary(matched$rank) #sanity check, the max should be 106

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator
matched$relative <- matched$monthno - matched$entry +1
matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
matched$post <- ifelse(matched$relative2<0, 0, 1)

# month as relative and factor
# set month 1 as ref group
matched$relative.factor <- factor(matched$relative)
matched <- within(matched, relative.factor<-relevel(relative.factor, ref="1"))
summary(matched$relative)

matched$relative2.factor <- factor(matched$relative2)
matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="0"))

# calculate open_month both before and after ML
matched <- matched %>%
  group_by(id, treat, match_place, post) %>%
  mutate(rank=row_number(id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))
matched$open_before <- ifelse(matched$post==0, matched$open_before, matched$open_month-matched$open_before)
matched$open_after <- ifelse(matched$post==1, matched$open_after, matched$open_month-matched$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<=23&relative2>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<=17&relative2>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<=11&relative2>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<=5&relative2>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, after6,after12,after18,after24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
tmp <- matched %>%
  group_by(id, treat, match_place) %>%
  filter(relative2<0&relative2>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative2<0&relative2>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative2<0&relative2>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative2<0&relative2>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(id, treat, match_place, before6,before12,before18,before24) %>%
  distinct()
matched <- merge(matched, tmp, by=c("id", "treat", "match_place"), all = TRUE)
matched$open6 <- ifelse(matched$before6==1&matched$after6==1,1,0)
matched$open12 <- ifelse(matched$before12==1&matched$after12==1,1,0)
matched$open18 <- ifelse(matched$before18==1&matched$after18==1,1,0)
matched$open24 <- ifelse(matched$before24==1&matched$after24==1,1,0)
rm(tmp)

matched <- within(matched, relative2.factor<-relevel(relative2.factor, ref="-3"))
matched$id_match <- paste0(matched$id, matched$match_place)

### regression, factor model ----
mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                  data = matched%>%filter((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29)), 
                  index = "id_match", weights = weights, model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE)
# clean data
tidy_mod.factor <- tidy_mod.factor %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|calorie", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treat:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))

# add year and month factor as covariate
summary(tidy_mod.factor$calorie) #[-95,133]
summary(tidy_mod.factor$diff) #[-95,14]
ggplot(data=tidy_mod.factor,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_hline(yintercept = -325, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor%>%filter(!is.na(diff)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor%>%filter(!is.na(diff)&p<0.05),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -400, ymax = 200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-16.5,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-400,200),breaks=seq(-400,200,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference")) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, regular items", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treat and comparison group. \n calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \nRestrict to items that were consistently available throughout the whole study period.") + 
  scale_color_discrete(name="Menu labeling", labels=c("No", "Yes")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/regular-items-only/main-effect.jpeg", dpi="retina")

