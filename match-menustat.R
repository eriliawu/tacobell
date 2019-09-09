# match menu stat nutritional info to sample transactions

getwd()
setwd("C:/Users/wue04/Box Sync/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("wesanderson")
library(wesanderson)

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

### analyze data from menu stat ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
table(menu$category)
table(menu$year)

# menu items available, by year, by category
trend <- count(menu[menu$year<=2015, ], year, category)
cat <- factor(trend$category, levels=c("Sandwiches", "Entrees", "Pizza", "Burgers",
                                       "Appetizers & Sides", "Fried Potatoes",
                                       "Salads", "Toppings & Ingredients",
                                       "Baked Goods", "Beverages", "Desserts"))
ggplot(data=trend, aes(x=as.character(year), y=n, group=cat, fill=cat)) +
      geom_area(size=0.5, color="white") +
      labs(title="Changes in TB menu items over time",
           x="Year", y="Number of menu items", fill="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/menu-items.jpeg", width=20, height=10, unit="cm")

# menu item calorie changes, do items overall become more caloric
ggplot(data=menu, aes(x=calories, color=category)) +
      geom_histogram(fill=category)

### link menu stat to transaction data ----
sample <- menu[menu$item_name=="Bean Burrito", ]

