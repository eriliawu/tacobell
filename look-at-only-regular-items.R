### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)
dev.off()

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

### read menustat data, figure out which items were consistenyl offered ----
menu <- read.csv("data/menustat/nutrition_info_all.csv",stringsAsFactors = FALSE)

#use item id to identify the same items
#clean up strings in the item names that unlikely appear in taco bell menu item names
menu <- menu %>% dplyr::select(id,year,item_name,calories) %>%
  filter(year<=2015) %>%
  arrange(id,year) %>% group_by(id) %>%
  mutate(n=n()) %>% mutate(rank=row_number()) %>% filter(n==6) %>% ungroup() %>%
  mutate(item_name = gsub(",|w/|Chips|Why Pay More Value Menu|Fresco Menu","",item_name)) %>%
  dplyr::select(item_name) %>%
  mutate(item_name = gsub("\\s+", " ", item_name)) %>%
  mutate(item_name=trimws(item_name)) %>% distinct()

### read sales data for all 35 quarters, get items consistently on sale thru all quarters ----
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
sales <- sales %>% rename(DW_PRODUCT=product,product=detail_desc) %>%
  group_by(product) %>% arrange(product,year,quarter) %>%
  mutate(n=n()) %>% mutate(max_year=max(year)) %>% mutate(min_year=min(year)) %>%
  filter(max_year==2015&min_year==2007&n>=30) %>% ungroup() %>%
  dplyr::select(DW_PRODUCT) %>% distinct() 
#write.csv(sales,"data/upload-to-bigpurple/consistent-sales.csv",row.names = FALSE)
#next step: join these regular for sale items with the sale data in database
#run the same mean calories query

### read in sales data, link to restaurant data ----
sample07q1 <- read.csv("data/from-bigpurple/consistent-sale/mean-calories_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("yearno", "monthno","restid", "category", "calorie", "count"))
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

restaurant <- aggregate(data=restaurant, .~address+concept+ownership+monthno+category, sum)
restaurant <- restaurant[!duplicated(restaurant), ]
restaurant$calorie <- restaurant$calorie/restaurant$count

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
for (i in c(1,2,4:5,9)) {
  mod.factor <- plm(formula = calorie~treat*relative2.factor+as.factor(month),
                    data = matched%>%filter(((relative2>=-30&relative2<=-3)|(relative2>=2&relative2<=29))&category==i), 
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
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-10, ymax=10, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-6, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-10,10),breaks=seq(-10,10,2)) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Effect of menu labeling on calories purchased, regular menu items", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \nMost estimates are not significant.") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entree","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/main-effect-by-category.jpeg", dpi="retina")

### average number of items ordered in each purchase; e.g. on avg, customers order 3 tacos per order ----
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
count$mean <- count$qty / count$num_orders

ggplot(data=count%>%filter(!grepl("3|7|8|10",category)),
       aes(x=interaction(year,month,lex.order = TRUE), y=mean,color=factor(category),group=factor(category))) +
  geom_line() + 
  geom_vline(xintercept = 50, color="grey",linetype="dashed") +
  geom_vline(xintercept = 62, color="grey",linetype="dashed") +
  ggplot2::annotate(geom="text",x=1:106,y=-0.1,label=c(NA,rep(c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA),8),c(1,NA,3,NA,5,NA,7,NA,9)),size = 2) + 
  ggplot2::annotate(geom="text",x=c(1,7+12*(0:8)),y=-0.3,label=unique(count$year),size=3) +
  coord_cartesian(ylim=c(0,5), expand = FALSE, clip = "off") + #important to have ylim set to what i actually want to display
  scale_y_continuous(limits=c(-0.5,5),breaks=seq(-0.5,5,0.5)) + #important to set ylim here to include text labels
  labs(title="Number of items per order, by category",x="",y="Percent of sales",
       caption="") + 
  scale_color_manual(name="Category",labels=c("Beverage","Burrito","Nacho","Other entry","Salad","Taco"),
                     values=c("hotpink","olivedrab3","#13B0E4","grey","orange","purple")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -15, size = 12),
        axis.text.x = element_blank(), #turn off default x axis label
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/aim1-diff-in-diff/regression/month-as-factor-drive-thru/sales-by-category.jpeg", dpi="retina")


### % of orders containing at least 1 burrito, 1 taco, or 1 drink ----
