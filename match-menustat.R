# match menu stat nutritional info to sample transactions

getwd()
setwd("C:/Users/wue04/Box Sync/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### read and clean data ----
#read clean restaurant data
menu <- read.csv("data/menustat_tacobell_nutrition_data.csv", stringsAsFactors = FALSE)
names(menu)
menu <- menu[, -c(1:2)]
colnames(menu)[1] <- "category"
sapply(menu, class)

# separate years into different files
# re-combine them into panel-data style file
menu_all <- NULL
for (i in c(2008, 2010, 2012:2018)) {
      # keep only respective years
      new_menu <- menu[, grepl(as.character(i), names(menu))|grepl("category", names(menu))]
      
      #clean columns names
      new_menu <- new_menu[, !grepl("text", names(new_menu))]
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern=paste("\\_", i, ".", sep=""), replacement="", x=names(new_menu))
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern=paste("\\_", i, sep=""), replacement="", x=names(new_menu))
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern="X.", replacement="", x=names(new_menu))
      
      #change column names to lower case
      colnames(new_menu)[1:dim(new_menu)[2]] <- tolower(colnames(new_menu)[1:dim(new_menu)[2]])
      
      # keep only columns/rows with content
      new_menu <- new_menu[!is.na(new_menu$calories), ]
      new_menu$serving_size_household <- NULL
      
      # export to single year files
      write.csv(new_menu, paste("data/menustat/nutrition_info_", i, ".csv", sep=""),
                row.names = FALSE)
      
      print(dim(new_menu))
      
      # prepare to make master file, panel data
      new_menu$year <- i
      menu_all <- rbind(menu_all, new_menu)
}
dim(menu_all)
menu_all <- menu_all[, c(1:2, 21, 3:20)]
write.csv(menu_all, "data/menustat/nutrition_info_all.csv", row.names = FALSE)
rm(menu, new_menu, i, menu_all)

# link menustat data to transactions ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
table(menu$category)
table(menu$year)

sample <- menu[grepl("Bean Burrito", menu$item_name), ]
