### calorie trend, summary statistics

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)

### read data ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-month-overall/mean-calorie_2007_Q1.csv",
                   stringsAsFactors = FALSE,
                   col.names = c("year", "month", "calorie","fat", "sat_fat", 
                                 "carb", "protein", "sodium", "count"))
sapply(sample07q1, class)
sample07q1[, c(3:8)] <- sample07q1[, c(3:8)]/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-month-overall/mean-calorie_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("year", "month", "calorie","fat", "sat_fat", 
                                                       "carb", "protein", "sodium", "count"))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

### merge time information ----
# time information: month, year
time <- read.csv("data/from-bigpurple/time_day_dim.csv", stringsAsFactors = FALSE)
names(time)
time <- time[, c(4, 7, 17, 38)]
colnames(time) <- c("year", "month", "yearno", "monthno")
sapply(time, class)
time$yearno <- as.integer(substr(time$yearno, 2, 5))
time$monthno <- as.integer(substr(time$monthno, 6, 7))
time <- time[!duplicated(time) & time$yearno>=2006, ]

calorie <- merge(calorie, time, by=c("year", "month"))
calorie <- calorie[, -c(1:2)]
colnames(calorie)[8:9] <- c("year", "month")
#rm(time)
calorie <- aggregate(data=calorie, .~year+month, sum) #fiscal month doesn't align with calendar month
calorie <- calorie[order(calorie$year, calorie$month), ]
calorie[, c(3:8)] <- calorie[, c(3:8)]/calorie$count
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month.csv", row.names = FALSE)
summary(calorie$calorie)
sd(calorie$calorie)

### visualization, change in mean calorie per order ----
ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=1)) +
      geom_point(color="hotpink") +
      geom_line(size=0.5, linetype="dashed", color="hotpink") +
      geom_smooth(method='lm') +
      ggplot2::annotate(geom="text", x=1:106, y=980, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=950, label=unique(calorie$year), size=4) + #year
      coord_cartesian(ylim=c(1000, 2000), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(1000, 2000, 100)) +
      labs(title="Mean calories per order", x="Time", y="Calories",
           caption="Note: items without calorie information are consider 0 calories.") +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/w-mod/mean-calorie-overall.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~year+month)
summary(mod1) #no significance on year or month
rm(mod1)

#histogram of calorie
hist(calorie$calorie, breaks=50,
     xlab="Calories", main="Histogram of mean calories per order, 2007-2015")

### by occasion ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-occasion/mean-calorie_occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE, 
                       col.names = c("year", "month", "occasion", "calorie", "fat",
                                     "sat_fat", "carb", "protein", "sodium", "count"))
#sapply(sample07q1, class)
sample07q1[, c(4:9)] <- sample07q1[, c(4:9)]/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-occasion/mean-calorie_occasion_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("year", "month", "occasion", "calorie", "fat",
                                                       "sat_fat", "carb", "protein", "sodium", "count"))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat(paste0("ERROR :", i, "Q", j),conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

calorie <- calorie[!is.na(calorie$occasion), ]
calorie <- merge(calorie, time, by=c("year", "month"))
calorie <- calorie[, -c(1:2)]
colnames(calorie)[9:10] <- c("year", "month")
calorie <- aggregate(data=calorie, .~year+month+occasion, sum) 
calorie <- calorie[order(calorie$year, calorie$month), ]
calorie[, c(4:9)] <- calorie[, c(4:9)]/calorie$count
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month-occasion.csv", row.names = FALSE)
summary(calorie$calorie)

ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                         group=as.character(occasion), color=as.character(occasion))) +
      geom_point() +
      geom_line(size=0.5, linetype="dashed") +
      geom_smooth(method='lm') +
      ggplot2::annotate(geom="text", x=1:106, y=980, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=950, label=unique(calorie$year), size=4) + #year
      coord_cartesian(ylim=c(1000, 2000), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(1000, 2000, 100)) +
      labs(title="Mean calories per order, by order type", x="Time", y="Calories",
           caption="Note: items without calorie information are consider 0 calories.") +
      scale_color_discrete(name="Order type",
                           labels=c("Eat-in", "Drive-through", "Takeout")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
ggsave("tables/analytic-model/mean-calorie-per-order/w-mod/mean-calorie-overall-by-occasion.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~year+month+as.character(occasion))
summary(mod1) #no significance on year or month
rm(mod1)

tapply(calorie$calorie, calorie$occasion, summary)
tapply(calorie$calorie, calorie$occasion, sd)

### by daypart ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-daypart/mean-calorie_daypart_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("year", "month", "time", "calorie","fat", 
                                     "sat_fat", "carb", "protein", "sodium", "count"))
#sapply(sample07q1, class)
sample07q1$calorie <- sample07q1$calorie/2

calorie <- NULL
for (i in 2007:2015) {
   for (j in 1:4) {
      tryCatch(
         if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else if (i<=2012|(i==2013&(j==1|j==3)))
            {
               sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-daypart/mean-calorie_daypart_",
                                         i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                                  col.names=c("year", "month", "time", "calorie",
                                              "fat", "sat_fat", "carb", "protein",
                                              "sodium", "count"))
               calorie <- rbind(calorie, sample)
            } else {
               sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-month-overall-daypart/mean-calorie_daypart_",
                                         i,"_Q",j,".csv"), stringsAsFactors = FALSE,
                                  col.names=c("year", "month", "time", "calorie",
                                              "fat", "sat_fat", "carb", "protein",
                                              "sodium", "count", "dollar"))
               sample$dollar <- NULL
               calorie <- rbind(calorie, sample)
            }, error=function(e){cat("ERROR :",paste0("year", i, " quarter", j),conditionMessage(e), "\n")}
      )
   }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

calorie <- merge(calorie, time, by=c("year", "month"))
colnames(calorie)[c(1:2, 11:12)] <- c("yearno", "monthno", "year", "month")
calorie <- aggregate(data=calorie, .~year+month+time+yearno+monthno, sum) #fiscal month doesnt align with calendar month
calorie[, 6:11] <- calorie[, 6:11]/calorie$count
calorie <- calorie[order(calorie$year, calorie$month), ]
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month-daypart.csv", row.names = FALSE)
summary(calorie$calorie)

ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                         group=as.character(time), color=as.character(time))) +
      geom_point() +
      geom_line(size=0.5, linetype="dashed") +
      geom_smooth(method='lm') +
      ggplot2::annotate(geom="text", x=1:106, y=880, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=850, label=unique(calorie$year), size=4) + #year
      coord_cartesian(ylim=c(900, 1900), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(900, 1900, 100)) +
      labs(title="Mean calories per order, by meal time", x="Time", y="Calories",
           caption="Note: calories are not adjusted for modifications to individual items.") +
      scale_color_manual(name="Meal time",
                           labels=c("Late night", "Breakfast", "Lunch",
                                    "Afternoon", "Dinner", "Evening"),
                           values=c("orange", "blueviolet", "red", "aquamarine3", "deepskyblue", "hot pink")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/w-mod/mean-calorie-overall-by-daypart.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~monthno+as.character(time))
summary(mod1) #no significance on year or month
rm(mod1)

tapply(calorie$calorie, calorie$time, summary)
tapply(calorie$calorie, calorie$time, sd)






### changes in california specifically ----
# read restaurant data
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv", stringsAsFactors = FALSE)
names(restaurant)
restaurant <- restaurant[, c(1:7, 23:24)]
area <- restaurant %>%
      filter((state=="NY"&(county=="New York"|county=="Kings"|county=="Bronx"|county=="Queens"|county=="Richmond"))|
                   (state=="WA"&county=="King")|
                   (state=="NY"&county=="Albany")|
                   (state=="PA"&city=="Philadelphia")|
                   state=="CA")

sample07q1 <- read.csv("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-overall/mean-calorie_restid_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("restid", "year", "month", "calorie", "fat",
                                     "sat_fat", "carb", "protein", "sodium", "count", "dollar"))
sapply(sample07q1, class)
sample07q1[, c(4:9, 11)] <- sample07q1[, c(4:9, 11)]/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie-w-mod/by-restaurant-overall/mean-calorie_restid_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("restid", "year", "month",
                                                       "calorie", "fat", "sat_fat",
                                                       "carb", "protein", "sodium",
                                                       "count", "dollar"))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

# merge with restaurant data
area <- merge(calorie, area, by="restid")
area <- merge(area, time, by=c("year", "month"))
colnames(area)[c(1:2, 20:21)] <- c("yearno", "monthno", "year", "month")
area <- area %>% #aggregate by address, year, month
      group_by(address, year, month, state, open, close, city, county, yearno, monthno) %>%
      summarise_at(vars(calorie, fat, sat_fat, carb, protein, sodium, dollar, count), .funs=sum)
area <- area[order(area$state, area$county, area$year, area$month), ]
area <- area[!(area$calorie==0&area$count<10), ] #remove implausible entries
area[, 11:17] <- area[, 11:17]/area$count

area$ml <- ifelse(area$state=="NY"&(area$county=="New York"|area$county=="Kings"|area$county=="Bronx"|area$county=="Queens"|area$county=="Richmond")&
                        ((area$year>=2008&area$month>=5)|area$year>=2009), 1,
                  ifelse(area$state=="WA"&area$county=="King"&area$year>=2009, 1,
                  ifelse(area$state=="NY"&area$county=="Albany"&
                               ((area$year>=2010&area$month>=2)|area$year>=2011), 1,
                  ifelse(area$state=="PA"&area$city=="Philadelphia"&area$year>=2010, 1, 
                  ifelse(area$state=="CA"&area$year>=2011, 1, 0)))))
area$ml2 <- ifelse(area$state=="CA"&((area$year==2011&area$month>=3)|area$year>=2012), 2,
                   ifelse(area$state=="CA"&(area$year<=2009|(area$year==2010&area$month<=10)), 0, 1))

# scatter plot
ggplot(data=subset(area, state=="CA"), aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                         group=as.character(ml2), color=as.character(ml2))) +
      geom_point(size=2, color="#F4EDCA") + #make calorie points bigger so it's not ugly
      stat_summary(aes(y=calorie,group=1), fun.y=mean, colour="orange", geom="point",group=1) + #insert monthly mean as scatter plots
      geom_vline(xintercept = 47.5, color="grey", linetype="dashed", size=1) + #2 month before ML
      geom_vline(xintercept = 51.5, color="grey", linetype="dashed", size=1) + #2 months after ML
      geom_smooth(method='lm') + #add best fitted lines
      ggplot2::annotate(geom="label", x=22, y=1750, label="Monthly decrease \n of 0.13 kcal", size=3) + #add pre-ML trend label
      ggplot2::annotate(geom="label", x=82, y=1750, label="Monthly decrease \n of 1.21 kcal***", size=3) + #add post-ML trend label
      ggplot2::annotate(geom="text", x=1:106, y=-50, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=-150, label=unique(area$year), size=4) + #year
      coord_cartesian(ylim=c(0, 2500), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(0, 2500, 250)) +
      labs(title="Calories per order trend, California", x="Time", y="Calories",
           caption="Note: items without calorie information are considered 0 calorie.") +
      scale_color_discrete(name="Menu Lableing", labels=c("No", "Adjust", "Yes")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=10),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/california-pre-post-ml.jpeg", dpi="retina")

# find out slope of 3 fitted lines
summary(lm(data=subset(area, state=="CA"&ml2==0), calorie~monthno)) #beta=-0.13, not significiant
summary(lm(data=subset(area, state=="CA"&ml2==1), calorie~monthno)) #beta=26, ***
summary(lm(data=subset(area, state=="CA"&ml2==2), calorie~monthno)) #beta=-1.21, ***




