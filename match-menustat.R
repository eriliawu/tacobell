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

### read in new menu data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
table(menu$category)
table(menu$year)

### collect trend data, in serving size, unit, calories ----
trend <- count(menu[menu$year<=2015, ], year, category)
serving_size <- aggregate(data=menu[menu$year<=2015, ],
                          serving_size~year+category, FUN=mean, na.rm=TRUE)

serving_size_unit <- menu[menu$year<=2015, c(1, 3, 6)]
serving_size_unit$serving_size_unit <- sub(patter="\\*",
                                           x=serving_size_unit$serving_size_unit,
                                           replacement = "")
serving_size_unit <- serving_size_unit %>% distinct()
serving_size_unit[57, 3] <- "g"

calories <- aggregate(data=menu[menu$year<=2015, ],
                      calories~year+category, FUN=mean, na.rm=TRUE)

menu$sfat_pct <- menu$saturated_fat*10/menu$calories*100
sfat_pct <- aggregate(data=menu[menu$year<=2015, ],
                          sfat_pct~year+category, FUN=mean, na.rm=TRUE)
menu$fat_pct <- menu$total_fat*10/menu$calories*100
fat_pct <- aggregate(data=menu[menu$year<=2015, ],
                      fat_pct~year+category, FUN=mean, na.rm=TRUE)

trend <- merge(trend, serving_size, by=c("year", "category"))
trend <- merge(trend, calories, by=c("year", "category"))
trend <- merge(trend, serving_size_unit, by=c("year", "category"))
trend <- merge(trend, fat_pct, by=c("year", "category"))
trend <- merge(trend, sfat_pct, by=c("year", "category"))

rm(serving_size, calories, serving_size_unit, fat_pct, sfat_pct)

### use trend data to plot figures ----
# menu items available, by year, by category
cat <- factor(trend$category, levels=c("Sandwiches", "Entrees", "Pizza", "Burgers",
                                       "Appetizers & Sides", "Fried Potatoes",
                                       "Salads", "Toppings & Ingredients",
                                       "Baked Goods", "Beverages", "Desserts"))
ggplot(data=trend, aes(x=as.character(year), y=n, group=cat, fill=cat)) +
      geom_area(size=0.5, color="white") +
      labs(title="Change in number of menu items over time",
           x="Year", y="Number of menu items", fill="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/num-of-menu-items.jpeg", width=20, height=10, unit="cm")

# menu item calorie changes, do items overall become more caloric
Year <- factor(menu$year[menu$year<=2015],
               levels=c("2008", "2010", "2012", "2013", "2014", "2015"))
ggplot(data=menu[menu$year<=2015, ], aes(x=calories, group=Year, fill=Year)) +
      geom_histogram(bins=200) +
      labs(title="Change in menu item calories over time",
           x="Calories", y="Number of menu items", fill="Year",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/menu-calorie-change.jpeg", width=20, height=10, unit="cm")

# change in menu item serving size, over time, do items overall get bigger?
# take mean serving size by category
ggplot(data=trend, aes(x=as.character(year), y=serving_size,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Change in serving sizes over time",
           x="Year", y="Serving size", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/mean-serving-size.jpeg", width=20, height=10, unit="cm")

# take mean calories by category, do items get more caloric over time?
ggplot(data=trend, aes(x=as.character(year), y=calories,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Change in calories over time",
           x="Year", y="Calories", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/mean-calories.jpeg", width=20, height=10, unit="cm")

# take mean total fat as %, by year
ggplot(data=trend, aes(x=as.character(year), y=sfat_pct,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Change in fat as % in total calories",
           x="Year", y="Percentage", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/saturated-fat-in-calories.jpeg", width=20, height=10, unit="cm")




### link menu stat to transaction data ----
sample <- menu[menu$item_name=="Bean Burrito", ]

