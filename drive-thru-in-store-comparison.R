### comapre drive-thru and in-store purchase characteristics
### priming for bevergae imputation

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)

### install and load packages ----
library(tidyverse)

### load beverage sales data and clean ----
sales_all <- NULL
sales_bev <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        all <- read.csv(paste0("data/from-bigpurple/drive-thru-in-store-comparison/sales_all_",
                                  i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                        col.names = c("occasion","count","cal","year","quarter"))
        sales_all <- rbind(all,sales_all)
        bev <- read.csv(paste0("data/from-bigpurple/drive-thru-in-store-comparison/sales_bev_",
                                  i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                        col.names = c("occasion","count_bev","cal_bev","year","quarter"))
        sales_bev <- rbind(bev,sales_bev)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(all,bev,i,j)

sales <- merge(sales_all,sales_bev,by=c("occasion","year","quarter"))
rm(sales_all,sales_bev)

#fix 2007 Q1 double counting issue
sales <- sales %>%
  mutate(cal = ifelse(year==2007 & quarter==1, cal/2, cal)) %>%
  filter(occasion>=1 & occasion<=3) %>%
  arrange(year,quarter,occasion) %>%
  mutate(quarterno = rep(1:35, each=3)) %>%
  mutate(mean_cal_bev = (cal_bev/count_bev)/(cal/count)) %>% # % calorie from beverage in an order
  mutate(pct_order_bev = count_bev / count) # % of orders that has a beverage

### % calorie from beverage ----
ggplot(data=sales,aes(x=quarterno, y=mean_cal_bev,color=as.factor(occasion))) + 
  #geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_hline(yintercept = -250, color="grey", linetype="dashed", size=0.5) +
  geom_point() + geom_line() +
  ggplot2::annotate(geom="text", x=1:35, y=-0.03, label=c(rep(1:4,8),1:3), size = 3) + #month
  ggplot2::annotate(geom="text", x=2.5+4*seq(0,8,1), y=-0.075, label=2007:2015, size=3.5) + #year
  #ggplot2::annotate(geom="label", x=0, y=-100, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  #ggplot2::annotate(geom="label", x=16.5, y=-200, label="   P<0.05", size=3) + 
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0,1)) + 
  scale_y_continuous(limits=c(-0.2,1),breaks=seq(-0.2,1,0.1),labels=scales::percent) +
  scale_x_continuous(breaks=c(1:35)) + #select which months to display
  labs(title="% calorie from beverage", x="", y="", caption="") + 
  scale_color_discrete(name="Order type",labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/drive-thru-in-store-comparison/pct-calorie-from-bev.jpeg", dpi="retina")

### % order that has a beverage ----
ggplot(data=sales,aes(x=quarterno, y=pct_order_bev,color=as.factor(occasion))) + 
  #geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  #geom_hline(yintercept = -250, color="grey", linetype="dashed", size=0.5) +
  geom_point() + geom_line() +
  ggplot2::annotate(geom="text", x=1:35, y=-0.03, label=c(rep(1:4,8),1:3), size = 3) + #month
  ggplot2::annotate(geom="text", x=2.5+4*seq(0,8,1), y=-0.075, label=2007:2015, size=3.5) + #year
  #ggplot2::annotate(geom="label", x=0, y=-100, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  #ggplot2::annotate(geom="label", x=16.5, y=-200, label="   P<0.05", size=3) + 
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0,1)) + 
  scale_y_continuous(limits=c(-0.2,1),breaks=seq(-0.2,1,0.1),labels=scales::percent) +
  scale_x_continuous(breaks=c(1:35)) + #select which months to display
  labs(title="% order with a beverage", x="", y="", caption="") + 
  scale_color_discrete(name="Order type",labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/drive-thru-in-store-comparison/pct-order_with-bev.jpeg", dpi="retina")

### size/type difference in beverage orders ----
sales_all <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        sales <- read.csv(paste0("data/from-bigpurple/top-selling-items/top-selling-food-item_restid_occasion_",
                               i,"_Q",j,".csv"), 
                          col.names = c("month","restid","occasion","detail","cal","desc","qty"))
        print(paste0(i,"Q",j," works"))
        sales_all <- rbind(sales,sales_all)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(sales,i,j)

# aggregate to month-restid-occasion-item
sales_all <- sales_all %>% group_by(month,restid,occasion,desc,detail) %>% 
  summarise(qty=sum(qty))

drinks <- read.csv("data/menu-matching/matched-results/PRODUCT_CALORIE_DIM.csv",
                       colClasses = c(rep(NA,4),rep("NULL",11)))
drinks <- drinks %>% filter(DW_PRODUCTGROUP==16 & FULLDESC!="") %>% 
  rename(detail=DW_PRODUCT, product_desc=PRODUCTDESC, full=FULLDESC) 

#match on beverage content
#strip away size information
drinks$rename <- gsub("[0-9]+", "", drinks$full)
drinks$rename <- gsub("CENT| OZ|OZ |SMALL|MEDIUM|EXTRA LARGE|REGULAR|GALLON|MEGA JUG|
                      LITER|LARGE", "", drinks$rename)
drinks$rename <- trimws(drinks$rename, "both")
drinks$rename <- gsub("UP", "7UP", drinks$rename)
drinks$rename <- gsub("7UPSELL", "UPSELL", drinks$rename)
drinks$rename <- gsub("UPSELL", "", drinks$rename)
drinks <- drinks[!grepl("ONION|NACHOS", drinks$rename), ]

#identify whether a beverage is a fountain drink
#identify the type of beverage: ssb, diet, etc.
cat <- read.csv("data/menu-matching/unique-drinks.csv", stringsAsFactors = FALSE)
cat <- cat[, c(4:8)]
drinks <- merge(drinks, cat, by="rename")
rm(cat)
drinks <- drinks %>% dplyr::select(detail,fountain,category2) %>% 
  #arrange(detail) %>% group_by(detail) %>% 
  #mutate(rep = n()) %>% mutate(max=max(rep)) %>% filter(max!=1) %>% 
  distinct()

sales_all <- merge(sales_all,drinks,by="detail")
rm(drinks)

#identify bev sizes
sales_all$size <- ifelse(grepl("SMALL|12 OZ|16 OZ|9 OZ|14 OZ", sales_all$desc), "small",
                      ifelse(grepl("MEDIUM|20 OZ|24 OZ|REGULAR|18 OZ", sales_all$desc), "medium",
                             ifelse(grepl("EXTRA LARGE|MEGA JUG|40 OZ|44 OZ|42 OZ|GALLON|2 LITER|XLG", sales_all$desc), "xl",
                                    ifelse(grepl("LARGE|30 OZ|32 OZ|LRG", sales_all$desc), "large",
                                           ifelse(grepl("ADD |ADDITIVE", sales_all$desc), "additive", "unclear")))))

# visualize purchase by beverage size
size <- sales_all %>% group_by(month,occasion,size) %>% 
  summarise(qty=sum(qty)) %>% group_by(month,occasion) %>% 
  mutate(pct = qty/sum(qty)) %>% filter(occasion>=1 & occasion<=3) %>% 
  filter(!grepl("unclear|xl",size))

ggplot(data=size,aes(x=month, y=pct)) + 
  geom_line(aes(color=as.factor(occasion))) + #aes(linetype=size)
  geom_text(size=2,data=data.frame(x=204:309,y=-0.015,label=c(NA,rep(c(1,NA,NA,4,NA,NA,7,NA,NA,10,NA,NA),8),c(1,NA,NA,4,NA,NA,7,NA,NA))),
            aes(x,y,label=label,inherit.aes=FALSE)) + #month labels; use geom_text to accomodate facet_grid
  geom_text(size=2.5,data=data.frame(x=c(210+12*c(0:8)),y=-0.05,label=2007:2015),aes(x,y,label=label,inherit.aes=FALSE)) + #year labels; use geom_text to accomodate facet_grid
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0,0.8)) + 
  scale_y_continuous(limits=c(-0.2,0.8),breaks=seq(-0.2,0.8,0.1),labels=scales::percent) +
  scale_x_continuous(breaks=c(1:35)) + #select which months to display
  labs(title="Beverage size distribution", x="", y="% of beverage sales within order type", caption="") + 
  scale_color_manual(name="Order type",labels=c("Eat-in","Drive-through","Takeout"),
                     values=c("orange","hotpink","skyblue")) +
  facet_grid(. ~ factor(size,levels = c("small","medium","large")), labeller = as_labeller(c(large="Large (30oz)",medium="Medium (20oz)",small="Small (16oz)"))) + #change the facet order, re-label facet names
  #scale_linetype_manual(name="Beverage size",values=c("solid","longdash","dotted"),labels=c("Large","Medium","Small"))+
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 
#ggsave("tables/drive-thru-in-store-comparison/bev-size-distribution.jpeg", dpi="retina")


### mean number of drinks ----
#total number of bevs order by occasion-month
bev_purchase <- size %>% 
  add_column(quarter=c(rep(1,9*1),rep(rep(1:4,each=9*3),8),rep(1:3,each=9*3))) %>% #add quarter number 
  add_column(year=c(rep(2007,1*3*3),rep(2007:2014,each=3*3*12),rep(2015,3*3*9))) %>%#add year number
  group_by(quarter,year,occasion,) %>% 
  summarise(num_bev_sales=sum(qty)) 

#read number of orders per month-year
sales_all <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        sales <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-occasion/mean-calorie_occasion_",
                                 i,"_Q",j,".csv"), 
                          col.names = c("year","month","occasion","cal","fat","sat","carb","protein","sodium","count"),
                          colClasses = c("NULL",NA,NA,rep("NULL",6),NA))
        sales_all <- rbind(sales,sales_all)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(sales,i,j)
sales_all <- sales_all %>% group_by(month,occasion) %>% summarise(count=sum(count)) %>% 
  filter(!is.na(occasion)) %>% 
  add_column(quarter=c(rep(1,3*1),rep(rep(1:4,each=3*3),8),rep(1:3,each=3*3))) %>% 
  add_column(year=c(rep(2007,1*3),rep(2007:2014,each=3*12),rep(2015,3*9))) %>% 
  group_by(year,quarter,occasion) %>% summarise(count=sum(count)) %>% ungroup() %>% 
  add_column(quarterno = rep(1:35, each=3)) 

#merge two datasets
bev_purchase <- merge(x=bev_purchase,y=sales_all,by=c("occasion","year","quarter"))
bev_purchase <- bev_purchase %>% mutate(mean_bev_sales = num_bev_sales/count)
rm(sales_all)

ggplot(data=bev_purchase,aes(x=quarterno, y=mean_bev_sales,color=as.factor(occasion))) + 
  geom_point() + geom_line() +
  ggplot2::annotate(geom="text", x=1:35, y=-0.07, label=c(rep(1:4,8),1:3), size = 3) + #month
  ggplot2::annotate(geom="text", x=2.5+4*seq(0,8,1), y=-0.2, label=2007:2015, size=3.5) + #year
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0,3)) + 
  scale_y_continuous(limits=c(-1,3),breaks=seq(-1,3,0.5)) +
  scale_x_continuous(breaks=c(1:35)) + #select which months to display
  labs(title="Mean number of beverages per order", x="", y="", caption="") + 
  scale_color_discrete(name="Order type",labels=c("Eat-in","Drive-through","Takeout")) +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/drive-thru-in-store-comparison/mean-bev-per-order.jpeg", dpi="retina")
