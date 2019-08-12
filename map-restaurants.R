# map restaurants

getwd()
setwd("C:/Users/wue04/Box Sync/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### read data ----
#read clean restaurant data
restaurants <- read.csv("raw-output/restaurants-clean.csv",
                        stringsAsFactors = FALSE)
names(restaurants)
restaurants <- restaurants[, -c(18:24)]
sapply(restaurants, class)

# keep only the ones that operated at least for a day in 2007
restaurants <- restaurants[(restaurants$close>="2007-01-01"|is.na(restaurants$close)), ]

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