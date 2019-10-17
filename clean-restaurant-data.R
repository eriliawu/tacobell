### clean up raw restaurant data exported from big purple

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### read data ----
sample <- read.csv("data/from-bigpurple/restaurants.csv",
                   sep = ";", header = FALSE, nrow=10, quote = "")
head(sample)
sapply(sample, class)
rm(sample)

restaurants <- read.csv("data/from-bigpurple/restaurants.csv",
                  sep = ";", header = FALSE, quote = "\"'", stringsAsFactors = FALSE,
                  col.names = c("restid", "status_desc", "ownership", "kfc_ownership",
                                "tbc_ownership", "phi_ownership", "ljs_onwership",
                                "awr_ownership", "address1", "address2", "address3",
                                "city", "county", "state", "zip", "open", "temp_close",
                                "reopen", "close", "lat", "lon", "cencept",
                                "concept_begin", "concept_end", "drive_thru",
                                "drive_thru_type"))
sapply(restaurants, class)

# convert dates, drop restaurants that closed before 2007-01-01
convert_to_date <- function(x) {
      # replace 0000-00-00 dates as NA
      x[x=="0000-00-00"] <- NA
      # convert characters to dates
      x <- as.Date(x)
      return(x)
      print(class(x))
}
restaurants[, c(16:19, 23:24)] <- lapply(restaurants[, c(16:19, 23:24)], convert_to_date)
rm(convert_to_date)
restaurants <- restaurants[restaurants$close>"2007-01-01"|is.na(restaurants$close), ]












