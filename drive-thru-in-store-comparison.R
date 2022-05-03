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

### mean number of drinks ----

### size difference in beverage orders ----

sales_all <- NULL
for (i in 2007:2015) {
  for (j in 1:4) {
    tryCatch(
      if(i==2015 & j==4) {stop("file doesn't exist")} else
      {
        sales <- read.csv(paste0("data/from-bigpurple/top-selling-items/top-selling-food-item_restid_occasion_",
                               i,"_Q",j,".csv"), 
                          col.names = c("month","restid","occasion","detail","cal","desc","qty"),
                          colClasses = c(rep(NA,3),rep("NULL",2),rep(NA,2)))
        print(paste0(i,"Q",j," works"))
        sales_all <- rbind(sales,sales_all)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
  }
}
rm(sales,i,j)

