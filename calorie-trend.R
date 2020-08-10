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
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie/by-month-overall/mean-calorie_2007_Q1.csv",
                   stringsAsFactors = FALSE,
                   col.names = c("year", "month", "calorie", "sat_fat", "carb", "protein"))
sapply(sample07q1, class)
sample07q1$calorie <- sample07q1$calorie/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie/by-month-overall/mean-calorie_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("year", "month", "calorie", "sat_fat", "carb", "protein"))
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
colnames(calorie)[5:6] <- c("year", "month")
#rm(time)
calorie <- aggregate(data=calorie, .~year+month, mean) #fiscal month doesnt align with calendar month
calorie <- calorie[order(calorie$year, calorie$month), ]
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month.csv", row.names = FALSE)
summary(calorie$calorie)

# merge with restaurant information: state, city
#restaurant <- read.csv("data/restaurants/analytic_restaurants.csv", stringsAsFactors = FALSE)
#names(restaurant)
#restaurant <- restaurant[, -c(12:13, 15:22, 25:27)]
#calorie <- merge(calorie, restaurant, by="restid")
#calorie <- calorie[order(calorie$state, calorie$address, calorie$year, calorie$month), ]

#count 
#calorie <- calorie %>%
#      group_by(restid) %>%
#      mutate(count=n()) %>%
#      mutate(rank = seq(1, count[1], 1))
#table(calorie$count)

### visualization, change in mean calorie per order ----
ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=1)) +
      geom_point() +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:106, y=960, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=900, label=unique(calorie$year), size=4) + #year
      coord_cartesian(ylim=c(1000, 2000), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(1000, 2000, 100)) +
      labs(title="Mean calories per order", x="Time", y="Calories",
           caption="Note: calories are not adjusted for modifications to individual items.") +
      #scale_color_discrete(name="Match",
      #                     labels=c("Best match, MenuStat, from yes list (n=496)", "Internet (n=36)",
      #                              "Best match, MenuStat, correct mistake by RA, maybe list (n=96)",
      #                              "Non-best match, MenuStat (n=110)", "No match (n=2,744)", "Proxy (n=35)")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/mean-calorie-overall.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~year+month)
summary(mod1) #no significance on year or month
rm(mod1)

#histogram of calorie
hist(calorie$calorie, breaks=50,
     xlab="Calories", main="Histogram of mean calories per order, 2007-2015")

### by occasion ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie/by-month-overall-occassion/mean-calorie-occasion_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("year", "month", "occasion", "calorie", "sat_fat", "carb", "protein"))
sapply(sample07q1, class)
sample07q1$calorie <- sample07q1$calorie/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie/by-month-overall-occassion/mean-calorie-occasion_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("year", "month", "occasion", "calorie", "sat_fat", "carb", "protein"))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

calorie <- calorie[!is.na(calorie$occasion), ]
calorie <- merge(calorie, time, by=c("year", "month"))
calorie <- calorie[, -c(1:2)]
colnames(calorie)[6:7] <- c("year", "month")
calorie <- aggregate(data=calorie, .~year+month+occasion, mean) #fiscal month doesnt align with calendar month
calorie <- calorie[order(calorie$year, calorie$month), ]
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month-occasion.csv", row.names = FALSE)
summary(calorie$calorie)

ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                         group=as.character(occasion), color=as.character(occasion))) +
      geom_point() +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:106, y=960, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=900, label=unique(calorie$year), size=4) + #year
      coord_cartesian(ylim=c(1000, 2000), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(1000, 2000, 100)) +
      labs(title="Mean calories per order, by order type", x="Time", y="Calories",
           caption="Note: calories are not adjusted for modifications to individual items.") +
      scale_color_discrete(name="Order type",
                           labels=c("Eat-in", "Drive-through", "Takeout")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust=-7, size = 12), #vjust to adjust position of x-axis
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14),
            plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
#ggsave("tables/analytic-model/mean-calorie-per-order/mean-calorie-overall-by-occasion.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~year+month+as.character(occasion))
summary(mod1) #no significance on year or month
rm(mod1)

tapply(calorie$calorie, calorie$occasion, summary)
tapply(calorie$calorie, calorie$occasion, sd)

### by daypart ----
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie/by-month-overall-daypart/mean-calorie-daypart_2007_Q1.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("year", "month", "time", "calorie", "sat_fat", "carb", "protein"))
sapply(sample07q1, class)
sample07q1$calorie <- sample07q1$calorie/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie/by-month-overall-daypart/mean-calorie-daypart_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names=c("year", "month", "time", "calorie", "sat_fat", "carb", "protein"))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

calorie <- merge(calorie, time, by=c("year", "month"))
calorie <- calorie[, -c(1:2)]
colnames(calorie)[6:7] <- c("year", "month")
calorie <- aggregate(data=calorie, .~year+month+time, mean) #fiscal month doesnt align with calendar month
calorie <- calorie[order(calorie$year, calorie$month), ]
#write.csv(calorie, "data/calorie-descriptive-data/mean-calorie-by-month-daypart.csv", row.names = FALSE)
summary(calorie$calorie)

ggplot(data=calorie, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                         group=as.character(time), color=as.character(time))) +
      geom_point() +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:106, y=860, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=800, label=unique(calorie$year), size=4) + #year
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
ggsave("tables/analytic-model/mean-calorie-per-order/mean-calorie-overall-by-daypart.jpeg", dpi="retina")

mod1 <- lm(data=calorie, calorie~year+month+as.character(time))
summary(mod1) #no significance on year or month
rm(mod1)

tapply(calorie$calorie, calorie$time, summary)
tapply(calorie$calorie, calorie$time, sd)





