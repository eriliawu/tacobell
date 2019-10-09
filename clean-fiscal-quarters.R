getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)

sample <- read.csv("data/from-bigpurple/fiscal_dates.csv", sep = ";",
                        header = FALSE, quote = "", nrow=10,
                        stringsAsFactors=FALSE)
head(sample)
rm(sample)

quarter <- read.csv("data/from-bigpurple/fiscal_dates.csv",
                  sep = ";", header = FALSE, quote = "",
                  stringsAsFactors=FALSE,
                  col.names = c("quarter", "start", "end"))
head(quarter)
sapply(quarter, class)
quarter$year <- as.integer(substr(quarter$quarter, 3, 6))
quarter$quarter <- as.integer(substr(quarter$quarter, 10, 10))
quarter$start <- as.Date(substr(quarter$start, 2, 11))
quarter$end <- as.Date(substr(quarter$end, 2, 11))
quarter$days <- quarter$end - quarter$start + 1
quarter$weeks <- quarter$days/7

write.csv(quarter, "data/fiscal-quarters/fiscal-quarters_clean.csv",
          row.names = FALSE)
