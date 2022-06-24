### compare beverage undercounting timing and ML rollout

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)

### install and load packages ----
library(tidyverse)

### clean product data, get beverages with vague description and labeled 0 calories ----
bev <- read.csv("data/menu-matching/matched-results/PRODUCT_CALORIE_DIM.csv",
                col.names = c("dw_product","dw_group","desc","fulldesc","calorie",
                              "total_fat","sat","trans","chol","sodium","pot",
                              "carb","fiber","sugar","protein"),
                colClasses = c(rep(NA,5),rep("NULL",10)))
bev <- bev %>% filter(dw_group %in% c(15,16,17) & !is.na(calorie)) #filter to beverages only
  #merge with sales data from top selling items data
  #merge with restaurant data to identify treated and comp restaurants in CA
bev$rename <- gsub("[0-9]+", "", bev$full)
bev$rename <- gsub("CENT| OZ|OZ |SMALL|MEDIUM|EXTRA LARGE|REGULAR|GALLON|MEGA JUG|
                      LITER|LARGE", "", bev$rename)
bev$rename <- trimws(bev$rename, "both")
bev$rename <- gsub("UP", "7UP", bev$rename)
bev$rename <- gsub("7UPSELL", "UPSELL", bev$rename)
bev$rename <- gsub("UPSELL", "", bev$rename)
bev <- bev[!grepl("ONION|NACHOS", bev$rename), ]

#identify the beverages that are mislabeled to 0 calories
# read in categorication data and merge to master beverage data
category <- read.csv("data/menu-matching/unique-drinks.csv")
bev <- merge(bev,category,by="rename")
bev <- bev[,c("dw_product","dw_group" ,"desc","fulldesc","category","fountain","category2")]
rm(category)

#identify beverage size
bev$size <- ifelse(grepl("SMALL|12 OZ|16 OZ|9 OZ|14 OZ", bev$fulldesc), "small",
                      ifelse(grepl("MEDIUM|20 OZ|24 OZ|REGULAR|18 OZ", bev$fulldesc), "medium",
                             ifelse(grepl("EXTRA LARGE|MEGA JUG|40 OZ|44 OZ|42 OZ|GALLON|2 LITER", bev$fulldesc), "xl",
                                    ifelse(grepl("LARGE|30 OZ|32 OZ", bev$fulldesc), "large",
                                           ifelse(grepl("ADD |ADDITIVE", bev$fulldesc), "additive", "unclear")))))

### load transaction data ----
sales <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        if(i==2007 & j==1) {
          tmp <- read.csv("data/from-bigpurple/top-selling-items/top-selling-food-item_restid_occasion_2007_Q1.csv")
          tmp <- tmp %>% filter(DW_OCCASION==1|DW_OCCASION==3) %>% 
            dplyr::select(DW_MONTH,DW_RESTID,DW_PRODUCTDETAIL,qty) %>% 
            mutate(qty=qty/2) %>% 
            group_by(DW_MONTH,DW_RESTID,DW_PRODUCTDETAIL) %>% summarise(qty=sum(qty))
          sales <- rbind(sales,tmp)
          print("data loaded for 2007Q1")
        } else {
          tmp <- read.csv(paste0("data/from-bigpurple/top-selling-items/top-selling-food-item_restid_occasion_",i,"_Q",j,".csv"))
          tmp <- tmp %>% filter(DW_OCCASION==1|DW_OCCASION==3) %>% 
            dplyr::select(DW_MONTH,DW_RESTID,DW_PRODUCTDETAIL,qty) %>% 
            group_by(DW_MONTH,DW_RESTID,DW_PRODUCTDETAIL) %>% summarise(qty=sum(qty))
          sales <- rbind(sales,tmp)
          print(paste0("data loaded for ",i,"Q",j))
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
sales <- sales %>% group_by(DW_MONTH,DW_RESTID,DW_PRODUCTDETAIL) %>% summarise(qty=sum(qty))
rm(tmp,i,j)

# merge to beverage data
bev <- merge(bev,sales,by.x="dw_product",by.y="DW_PRODUCTDETAIL")

### merage sales data to analytical restaurants ----
restaurants <- read.csv("data/restaurants/analytic_restaurants.csv",
                        colClasses=c(rep(NA,2),rep("NULL",7),NA,rep("NULL",3),NA,rep("NULL",15),NA))
restaurants$ownership <- ifelse(restaurants$ownership=="COMPANY",1,0)
restaurants$concept <- ifelse(restaurants$concept=="TBC",1,0)

ca <- read.csv("data/matched_ca_restaurants_in_store_prematched_cbps.csv")
ca <- ca %>% dplyr::select(treat,address:concept) %>% 
  mutate(tract_num=gsub(pattern='"',x=tract_num,replacement='')) %>% 
  mutate(tract_num=ifelse(nchar(tract_num)==10,paste0("0",tract_num),tract_num)) %>% 
  mutate(tract_num=paste0('"',tract_num,'"'))
restaurants <- merge(restaurants,ca,by=c("address","tract_num","ownership","concept"))

bev <- merge(bev,restaurants,by.x="DW_RESTID",by.y="restid")
rm(ca,restaurants)

### look at beverage sales over time, by treatment status ----
sales <- bev %>% dplyr::select(treat,category2,qty,DW_MONTH) %>% 
  rename(month=DW_MONTH) %>% 
  filter(!grepl("Additive",category2)) %>% 
  mutate(cat=ifelse(grepl("Diet|Low",category2),"Low-calorie",ifelse(grepl("Vague",category2),"Vague",ifelse(grepl("Freeze",category2),"Freeze","SSB")))) %>% 
  group_by(treat,cat,month) %>% summarise(qty=sum(qty)) %>%
  group_by(treat,month) %>% mutate(total_qty=sum(qty)) %>% mutate(category_pct=qty/total_qty) 
  
ggplot(data=sales,aes(x=month, y=category_pct,color=as.factor(cat))) + 
  geom_line(aes(linetype=as.factor(treat))) +
  geom_vline(xintercept=253,color="grey",linetype="longdash") +
  geom_vline(xintercept=256,color="grey",linetype="longdash") +
  geom_vline(xintercept=265,color="grey",linetype="longdash") +
  geom_vline(xintercept=283,color="grey",linetype="longdash") +
  ggplot2::annotate(geom="label", x=248, y=0.5, label="ML starts", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=267, y=0.9, label="One year", size=3) + 
  ggplot2::annotate(geom="label", x=285, y=0.9, label="End of \nstudy period", size=3) + 
  ggplot2::annotate(geom="text",x=204:309,y=-0.015,label=c(NA,rep(c(1,NA,NA,4,NA,NA,7,NA,NA,10,NA,NA),8),c(1,NA,NA,4,NA,NA,7,NA,NA)),size = 3) + #month
  ggplot2::annotate(geom="text",x=c(210+12*c(0:8)),y=-0.05,label=2007:2015,size=3.5) + #year
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0,1)) + 
  scale_y_continuous(limits=c(-0.2,1),breaks=seq(-0.2,1,0.1),labels=scales::percent) +
  scale_x_continuous(breaks=c(1:35)) + #select which months to display
  labs(title="In-store beverage sales over time in California", x="", y="",
       caption="Note: the analysis excludes additives such as coffee creamer and sugar packets for tea.") + 
  scale_color_manual(name="Beverage type",labels=c("Freeze","Low-calorie","SSB","Vague"),values=c("#228B22","skyblue","orange","hotpink")) +
  scale_linetype_manual(name="Menu label",labels=c("No","Yes"),values=c("dashed", "solid")) +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/beverage-undercount-ml-rollout/bev-sales-over-time-ca.jpeg", dpi="retina")

### look at monthno=277 (2 yrs after ML rollout in CA in-store) ----
# what's driving the sales of ssb and freeze up
# what's driving the difference in sales between CA and its comp restaurants
table(bev$category2)

sales_after <- bev %>%  rename(month=DW_MONTH,product=dw_product) %>% 
  dplyr::select(treat,category2,qty,month,product,fulldesc) %>% 
  filter(!grepl("Additive",category2) & month==282) %>% 
  mutate(cat=ifelse(grepl("Diet|Low",category2),"Low-calorie",ifelse(grepl("Vague",category2),"Vague",ifelse(grepl("Freeze",category2),"Freeze","SSB")))) %>% 
  group_by(treat,fulldesc,month,cat) %>% summarise(qty_item=sum(qty)) %>%
  group_by(treat,month) %>% mutate(qty_total=sum(qty_item)) %>% mutate(item_pct=qty_item/qty_total) %>% 
  arrange(treat,month,desc(item_pct))

sales_before <- bev %>%  rename(month=DW_MONTH,product=dw_product) %>% 
  dplyr::select(treat,category2,qty,month,product,fulldesc) %>% 
  filter(!grepl("Additive",category2) & month==277) %>% 
  mutate(cat=ifelse(grepl("Diet|Low",category2),"Low-calorie",ifelse(grepl("Vague",category2),"Vague",ifelse(grepl("Freeze",category2),"Freeze","SSB")))) %>% 
  group_by(treat,fulldesc,month,cat) %>% summarise(qty_item=sum(qty)) %>%
  group_by(treat,month) %>% mutate(qty_total=sum(qty_item)) %>% mutate(item_pct=qty_item/qty_total) %>% 
  arrange(treat,month,desc(item_pct))