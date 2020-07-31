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
sample07q1 <- read.csv("data/from-bigpurple/mean-calorie/by-restid-calorie_2007_Q1.csv",
                   stringsAsFactors = FALSE, col.names = c("x", "restid", "year", "month", "calorie"),
                   colClasses=c("NULL", NA, NA, NA, NA))
sapply(sample07q1, class)
sample07q1$calorie <- sample07q1$calorie/2

calorie <- NULL
for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if((i==2007 & j==1)|(i==2015 & j==4)) {stop("file doesn't exist")} else
                  {
                        sample <- read.csv(paste0("data/from-bigpurple/mean-calorie/by-restid-calorie_",
                                                  i,"_Q",j,".csv"),
                                           stringsAsFactors = FALSE,
                                           col.names = c("x", "restid", "year", "month", "calorie"),
                                           colClasses=c("NULL", NA, NA, NA, NA))
                        calorie <- rbind(calorie, sample)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
calorie <- rbind(calorie, sample07q1)
rm(sample, sample07q1, i, j)

### merge time and restaurant information ----
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
colnames(calorie)[3:4] <- c("year", "month")
rm(time)
calorie <- aggregate(data=calorie, calorie~year+month+restid, mean) #fiscal month doesnt align with calendar month

# merge with restaurant information: state, city
restaurant <- read.csv("data/restaurants/analytic_restaurants.csv", stringsAsFactors = FALSE)
names(restaurant)
restaurant <- restaurant[, -c(12:13, 15:22, 25:27)]
calorie <- merge(calorie, restaurant, by="restid")
calorie <- calorie[order(calorie$state, calorie$address, calorie$year, calorie$month), ]

#count 
calorie <- calorie %>%
      group_by(restid) %>%
      mutate(count=n()) %>%
      mutate(rank = seq(1, count[1], 1))
table(calorie$count)

### test figures ----
test <- calorie %>%
      filter(county=="New York"|county=="Kings"|county=="Queens"|county=="Bronx"|county=="Richmond")
ggplot(data=test, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                      group=as.character(restid), color=as.character(restid))) +
      geom_point() +
      geom_line() +
      ggplot2::annotate(geom="text", x=1:106, y=740, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=720, label=unique(fig1$year), size=4) + #year
      coord_cartesian(expand = FALSE, clip = "off") + 
      #scale_y_continuous(breaks=seq(750, 1750, 250)) +
      labs(title="Mean calories per order", x="Time", y="Calories") +
      #scale_color_discrete(name="Match",
      #                     labels=c("Best match, MenuStat, from yes list (n=496)", "Internet (n=36)",
      #                              "Best match, MenuStat, correct mistake by RA, maybe list (n=96)",
      #                              "Non-best match, MenuStat (n=110)", "No match (n=2,744)", "Proxy (n=35)")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust = -15, size = 12),
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14))


### visualization, change in mean calorie per order ----
fig1 <- aggregate(data=calorie, calorie~year+month, mean)
fig1 <- fig1[order(fig1$year, fig1$month), ]
ggplot(data=fig1, aes(x=interaction(year, month, lex.order = TRUE), y=calorie,
                           group=1)) +
      geom_point() +
      geom_line(size=0.5) +
      ggplot2::annotate(geom="text", x=1:106, y=720, label=c(12, rep(c(1:12),8), c(1:9)), size = 2) + #month
      ggplot2::annotate(geom="text", x=c(1, seq(7.5, 7.5+12*7, 12), 102), y=680, label=unique(fig1$year), size=4) + #year
      coord_cartesian(ylim=c(750, 1750), expand = FALSE, clip = "off") + 
      scale_y_continuous(breaks=seq(750, 1750, 250)) +
      labs(title="Mean calories per order", x="Time", y="Calories", caption=) +
      #scale_color_discrete(name="Match",
      #                     labels=c("Best match, MenuStat, from yes list (n=496)", "Internet (n=36)",
      #                              "Best match, MenuStat, correct mistake by RA, maybe list (n=96)",
      #                              "Non-best match, MenuStat (n=110)", "No match (n=2,744)", "Proxy (n=35)")) +
      theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(vjust = -15, size = 12),
            axis.text.x = element_blank(), #turn off default x axis label
            axis.title.y = element_text(size = 12),
            legend.text=element_text(size=14))
ggsave("tables/analytic-model/mean-calorie-per-order/mean-calorie-overall.jpeg", dpi="retina")

