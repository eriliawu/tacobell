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
restaurants <- read.csv("data/restaurants-clean.csv",
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

# clean up urban/rural data
urban <- read.csv("data/2010census_PctUrbanRural_State.csv", stringsAsFactors = FALSE)
names(urban)
urban <- urban[, c(1, 6)]
colnames(urban)[1:2] <- c("state_num", "urban_pct")
tb_state <- merge(tb_state, urban, by="state_num", x.all=TRUE)
rm(urban)

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
                                       labels=c("Q1", "Q2", "Q3", "Q4")) +
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
                           labels=c("Q1", "Q2", "Q3", "Q4")) +
      scale_x_continuous(limits=c(min(tb_state$pop, na.rm=TRUE),
                                  max(tb_state$pop, na.rm=TRUE)))

urban_plot <- ggplot(data=tb_state, aes(x=urban_pct, y=tb, color=factor(tb_state$income_cat))) + 
      geom_point() + 
      labs(title="Urbanicity and number of TB restaurants",
           x="% population as urban", y="Number of TB restaurants",
           caption="Data source: Census Bureau ACS 2015 and 2010 Census") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic")) +
      scale_color_discrete(name="Income quartile",
                           breaks=c("1", "2", "3", "4"),
                           labels=c("Q1", "Q2", "Q3", "Q4")) +
      scale_x_continuous(limits=c(min(tb_state$urban_pct, na.rm=TRUE),
                                  max(tb_state$urban_pct, na.rm=TRUE)))

### number of TB restaurants, by census tract ----
# model figures based on cdc nation wide survey
# clean tract level obesity data from 50 city health dash board
tract <- read.csv("C:/Users/wue04/Box Sync/tacobell/data/city-health-dash-board/500 Cities v7.0 - released September 5, 2019/CHDB_data_tract_all v7_0.csv", stringsAsFactors = FALSE)
unique(tract$metric_name)
tract <- subset(tract, tract$metric_name=="Diabetes"|tract$metric_name=="High blood pressure"|tract$metric_name=="Limited access to healthy foods"|tract$metric_name=="Obesity"|tract$metric_name=="Children in Poverty"|tract$metric_name=="Physical inactivity")
table(tract$data_yr_type)
table(tract$state_abbr)
tract <- tract[, -c(18:20)]
names(tract)
tract <- tract[, c(1:8, 14:16)]
colnames(tract)[c(1:3, 5)] <- c("state", "state_num", "county_num", "tract")
sapply(tract, class)

# re-load restaurants data
count <- restaurants[, c("restid", "state", "state_num", "county_num", "tract")]
count$n <- 1
count <- subset(count, !is.na(count$tract))
count <- aggregate(data=count, n~state_num+county_num+tract, FUN=sum)

tract <- merge(tract, count, by=c("state_num", "county_num", "tract"))
names(tract)
summary(tract$n)
rm(count)
tract <- tract[, -c(7, 10:11)]
names(tract)
colnames(tract)[6] <- "tract_fips"
tract <- reshape(tract, timevar = "metric_name",
                idvar = c("state_num", "county_num", "tract", "state", "county_name", "tract_fips", "n"),
                direction="wide")
names(tract)
colnames(tract)[8:13] <- c("hypertension", "obesity", "child_poverty",
                           "diabetes", "inactivity", "limited_healthy_food")

# obesity and children in poverty
ggplot(data=tract, aes(x=obesity, y=child_poverty,
                       group=as.factor(n), col=as.factor(n))) +
      geom_point(size=0.5) +
      labs(title="Census tract level rates of obesity and children in poverty",
           x="Obesity rate", y="Rate of children in poverty", col="Number of TB restaurants",
           caption="Data source: City Health Dashboard") +
      scale_color_brewer(palette="Set2") +
      theme(plot.title=element_text(hjust=0.5, size=16),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/number-of-tb-restaurants-correlations/tract-obesity-poverty.jpeg", width=20, height=10, unit="cm")

# obesity and physical inactivity
ggplot(data=tract, aes(x=obesity, y=inactivity,
                       group=as.factor(n), col=as.factor(n))) +
      geom_point(size=0.5) +
      labs(title="Census tract level rates of obesity and physical inactivity",
           x="Obesity rate", y="Rate of physical inactivity", col="Number of TB restaurants",
           caption="Data source: City Health Dashboard") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=16),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/number-of-tb-restaurants-correlations/tract-obesity-inactivity.jpeg", width=20, height=10, unit="cm")

### restaurants, analytics ----
names(restaurants)
length(restaurants$restid[restaurants$status=="open" & restaurants$open<="2007-01-01"]) #3496
length(restaurants$restid[restaurants$status=="open" & restaurants$open>"2007-01-01"]) #3458
length(restaurants$restid[restaurants$status=="closed" & restaurants$open<="2007-01-01" & !is.na(restaurants$open)]) #2953
length(restaurants$restid[restaurants$status=="closed" & restaurants$open>"2007-01-01" & !is.na(restaurants$open)]) #704
length(restaurants$restid[!is.na(restaurants$close)&is.na(restaurants$open)])#490

hist(restaurants$tempclose_time[restaurants$status=="open" & restaurants$open<="2007-01-01"],
     main="Temp closing of continuously operated restaurants", xlab="Days", ylab="",
     breaks=300, xlim=c(0, 600), ylim=c(0, 200))
abline(v = mean(restaurants$tempclose_time[restaurants$status=="open" & restaurants$open<="2007-01-01"],
                na.rm=TRUE), col = "blue", lwd = 2)
