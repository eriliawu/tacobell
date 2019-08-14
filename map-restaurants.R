# map restaurants

getwd()
setwd("C:/Users/wue04/Box Sync/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

#install.packages("ggplot2")
library(ggplot2)

### read data ----
#read clean restaurant data
restaurants <- read.csv("raw-output/restaurants-clean.csv",
                        stringsAsFactors = FALSE)
names(restaurants)
restaurants <- restaurants[, -c(18:24)]
sapply(restaurants, class)

# keep only the ones that operated at least for a day in 2007
restaurants <- restaurants[(restaurants$close>="2007-01-01"|is.na(restaurants$close)), ]

# number of geocoded restaurants
length(restaurants$restid[!is.na(restaurants$lon) & restaurants$status!="planned"]) #10889, 98%


### temp closing ----
table(restaurants$status)

# average temp close time
summary(restaurants$tempclose_time)
sd(restaurants$tempclose_time, na.rm=TRUE)
hist(restaurants$tempclose_time, breaks=200,
     xlim=c(0, 500), ylim=c(0, 500),
     main="Histogram of Temp Close Time", xlab="Days", ylab="")
abline(v = mean(restaurants$tempclose_time, na.rm=TRUE), col = "blue", lwd = 2)
length(restaurants$tempclose_time[(restaurants$tempclose_time>500 & !is.na(restaurants$tempclose_time))]) #11
length(restaurants$tempclose_time[(restaurants$tempclose_time<=30 & !is.na(restaurants$tempclose_time))]) #774
length(restaurants$tempclose_time[(restaurants$tempclose_time>30 & restaurants$tempclose_time<=90 & !is.na(restaurants$tempclose_time))]) #774
length(restaurants$tempclose_time[(restaurants$tempclose_time>90 & restaurants$tempclose_time<=180 & !is.na(restaurants$tempclose_time))]) #774
length(restaurants$tempclose_time[(restaurants$tempclose_time>180 & restaurants$tempclose_time<=365 & !is.na(restaurants$tempclose_time))]) #774
length(restaurants$tempclose_time[(restaurants$tempclose_time>265 & !is.na(restaurants$tempclose_time))]) #11

### map ----
# plot lon/lat coordinates
plot(x=restaurants$lon, y=restaurants$lat)

map <- restaurants[!is.na(restaurants$lon), c("restid", "lon", "lat", "status")]
write.csv(map, "raw-output/map.csv")
rm(map)

### number of TB restaurants, by state ----
table(restaurants$state)
tb_state <-aggregate(restaurants, by=list(restaurants$state, restaurants$state_num),
                    FUN=length)
names(tb_state)
tb_state <- tb_state[, c(1:3)]
colnames(tb_state)[1:3] <- c("state", "state_num", "tb")

# clean up income data
income <- read.csv("data/ACS_15_5YR_S1901_income/ACS_15_5YR_S1901_with_ann.csv", stringsAsFactors = FALSE)
income <- income[-1, c("GEO.id2","GEO.display.label", "HC01_EST_VC15")]
colnames(income)[1:3] <- c("state_num", "state_name", "income")
income$state_num <- as.numeric(income$state_num)
tb_state <- merge(tb_state, income, by="state_num", all=TRUE)
rm(income)

# clean up obesity data
obesity <- read.csv("data/obesity.csv", stringsAsFactors = FALSE)
tb_state <- merge(tb_state, obesity, by="state_name", x.all=TRUE)
rm(obesity)

# clean up total population
pop <- read.csv("data/ACS_15_5YR_B01003_population/ACS_15_5YR_B01003_with_ann.csv", stringsAsFactors = FALSE)
head(pop)
pop <- pop[-1, 3:4]
colnames(pop)[1:2] <- c("state_name", "pop")
tb_state <- merge(tb_state, pop, by="state_name", all=TRUE)
tb_state$pop <- as.numeric(tb_state$pop)/1000000

names(tb_state)
sapply(tb_state, class)
tb_state <- tb_state[, c("state", "state_name", "state_num", "tb", "income", "obesity_rate")]
tb_state$income <- as.numeric(tb_state$income)

# income quintile
quartile <- quantile(tb_state$income)
tb_state$income_cat[tb_state$income<quartile[2]] <- 1
tb_state$income_cat[tb_state$income<quartile[3] & tb_state$income>=quartile[2]] <- 2
tb_state$income_cat[tb_state$income<quartile[4] & tb_state$income>=quartile[3]] <- 3
tb_state$income_cat[tb_state$income>=quartile[4]] <- 4
rm(quartile)

# plot
qplot(tb_state$income, tb_state$tb,
     xlim=c(min(tb_state$income, na.rm=TRUE), max(tb_state$income, na.rm=TRUE)),
     main="Number of TB restaurants and household income",
     xlab="Household mean income", ylab="Number of TB restaurants")

income <- factor(tb_state$income_cat)
qplot(tb_state$obesity_rate, tb_state$tb, 
      color=income,
      xlim=c(min(tb_state$obesity_rate, na.rm=TRUE), max(tb_state$obesity_rate, na.rm=TRUE)),
      #main="Number of TB restaurants and obesity rate",
      ylab="Number of TB restaurants", xlab="Obesity rate")

qplot(tb_state$pop, tb_state$tb, 
      color=income,
      xlim=c(min(tb_state$pop, na.rm=TRUE), max(tb_state$pop, na.rm=TRUE)),
      #main="Number of TB restaurants and obesity rate",
      xlab="State population (million)", ylab="Number of TB restaurants")

obesity_plot <- ggplot(data=tb_state, aes(x=obesity_rate, y=tb, color=factor(tb_state$income_cat))) + 
                  geom_point() + 
                  labs(title="Obesity rate and number of TB restaurants",
                       x="Obesity rate", y="Number of TB restaurants",
                       caption="Data source: CDC and Census Bureau, 2015") +
                  theme(plot.title=element_text(hjust=0.5, size=18),
                        plot.caption=element_text(hjust=0, face="italic")) +
                  scale_color_discrete(name="Income quartile",
                                       breaks=c("1", "2", "3", "4"),
                                       labels=c("Bottom", "Mid-lower", "Mid-top", "Top")) +
                  scale_x_continuous(limits=c(min(tb_state$obesity_rate, na.rm=TRUE),
                                              max(tb_state$obesity_rate, na.rm=TRUE)))

pop_plot <- ggplot(data=tb_state, aes(x=pop, y=tb, color=factor(tb_state$income_cat))) + 
      geom_point() + 
      labs(title="State population and number of TB restaurants",
           x="State population (million)", y="Number of TB restaurants",
           caption="Data source: Census Bureau, 2015") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic")) +
      scale_color_discrete(name="Income quartile",
                           breaks=c("1", "2", "3", "4"),
                           labels=c("Bottom", "Mid-lower", "Mid-top", "Top")) +
      scale_x_continuous(limits=c(min(tb_state$pop, na.rm=TRUE),
                                  max(tb_state$pop, na.rm=TRUE)))


