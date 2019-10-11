# match menu stat nutritional info to sample transactions

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("gridExtra")
library(gridExtra)

### read and clean data ----
#read clean restaurant data
menu <- read.csv("data/menustat_tacobell_nutrition_data.csv", stringsAsFactors = FALSE)
names(menu)
menu$Restaurant <- NULL
colnames(menu)[2] <- "category"
sapply(menu, class)

# separate years into different files
# re-combine them into panel-data style file
menu_all <- NULL
for (i in c(2008, 2010, 2012:2018)) {
      # keep only respective years
      new_menu <- menu[, grepl(as.character(i), names(menu))|grepl("category", names(menu))|grepl("ID", names(menu))]
      
      #clean columns names
      new_menu <- new_menu[, !grepl("text", names(new_menu))]
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern=paste("\\_", i, ".", sep=""), replacement="", x=names(new_menu))
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern=paste("\\_", i, sep=""), replacement="", x=names(new_menu))
      colnames(new_menu)[1:dim(new_menu)[2]] <- sub(pattern="X.", replacement="", x=names(new_menu))
      colnames(new_menu)[1] <- "id"
      
      #change column names to lower case
      colnames(new_menu)[1:dim(new_menu)[2]] <- tolower(colnames(new_menu)[1:dim(new_menu)[2]])
      
      # keep only columns/rows with content
      new_menu <- new_menu[!is.na(new_menu$calories), ]
      new_menu$serving_size_household <- NULL
      
      # export to single year files
      #write.csv(new_menu, paste("data/menustat/nutrition_info_", i, ".csv", sep=""),
       #         row.names = FALSE)
      
      print(dim(new_menu))
      
      # prepare to make master file, panel data
      new_menu$year <- i
      menu_all <- rbind(menu_all, new_menu)
}
dim(menu_all)
names(menu_all)
menu_all <- menu_all[, c(1:2, 22, 3:21)]
write.csv(menu_all, "data/menustat/nutrition_info_all.csv", row.names = FALSE)
rm(menu, new_menu, i, menu_all)

### read in new menu data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
table(menu$category)
table(menu$year)

# re-categorize
# baked goods --> desserts
# burgers --> sandwiches
# fried potatoes --> appetizers
menu$category[menu$category=="Baked Goods"] <- "Desserts"
menu$category[menu$category=="Burgers"] <- "Sandwiches"
menu$category[menu$category=="Fried Potatoes"] <- "Appetizers & Sides"
menu$category[menu$category=="Pizza"] <- "Entrees"
table(menu$category)

### collect trend data, in serving size, unit, calories ----
trend <- count(menu[menu$year<=2015, ], year, category)
serving_size <- aggregate(data=menu[menu$year<=2015, ],
                          serving_size~year+category, FUN=mean, na.rm=TRUE)

serving_size_unit <- menu[menu$year<=2015, c(2, 3, 7)]
serving_size_unit$serving_size_unit <- sub(patter="\\*",
                                           x=serving_size_unit$serving_size_unit,
                                           replacement = "")
serving_size_unit <- serving_size_unit %>% distinct()
serving_size_unit <- serving_size_unit[-42, ]

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
cat <- factor(trend$category, levels=c("Sandwiches", "Entrees", 
                                       "Appetizers & Sides", 
                                       "Salads", "Toppings & Ingredients",
                                       "Beverages", "Desserts"))
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
           x="Mean calories", y="Number of menu items", fill="Year",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_fill_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/menu-calorie-change.jpeg", width=20, height=10, unit="cm")

# change in menu item serving size, over time, do items overall get bigger?
# take mean serving size by category
unit <- factor(paste(trend$category, " (", trend$serving_size_unit, ")",sep=""),
               levels=c("Sandwiches (g)", "Entrees (g)", "Appetizers & Sides (g)",
                        "Salads (g)", "Toppings & Ingredients (g)",
                        "Beverages (fl oz)", "Desserts (g)"))
ggplot(data=trend, aes(x=as.character(year), y=serving_size,
                       group=unit, col=unit)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Change in serving sizes over time",
           x="Year", y="Mean serving size", col="Category",
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
           x="Year", y="Mean calories", col="Category",
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
      labs(title="Change in saturated fat as % in total calories",
           x="Year", y="Mean percentage", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/saturated-fat-in-calories.jpeg", width=20, height=10, unit="cm")

rm(unit, Year, cat)

### look at how calories change for burritos and such ----
# collect specific labelling data on:
# burritos, tacos, chalupa, tostada, taquito, quesadilla, pizza
burrito <- subset(menu,
            (menu$category=="Sandwiches"|menu$category=="Entrees")&grepl("Burrito", menu$item_name),
            select=c("item_name","year", "serving_size", "calories", "saturated_fat"))
quesadilla <- subset(menu,
            (menu$category=="Sandwiches"|menu$category=="Entrees")&(grepl("Quesadilla", menu$item_name)|grepl("Pizza", menu$item_name)),
            select=c("item_name","year", "serving_size", "calories", "saturated_fat"))
taco <- subset(menu,
            (menu$category=="Sandwiches"|menu$category=="Entrees")&(grepl("Taco", menu$item_name)|grepl("Chalupa", menu$item_name)),
            select=c("item_name","year", "serving_size", "calories", "saturated_fat"))
other <- subset(menu,
            (menu$category=="Sandwiches"|menu$category=="Entrees")&(grepl("Gordita", menu$item_name)|grepl("Taquito", menu$item_name)),
            select=c("item_name","year", "serving_size", "calories", "saturated_fat"))
burrito$cat <- "burrito"
quesadilla$cat <- "quesadilla"
taco$cat <- "taco"
other$cat <- "other"
main <- rbind(burrito, quesadilla, taco, other)
main$fat_pct <- main$saturated_fat*10/main$calories*100
rm(burrito, quesadilla, taco, other)

trend <- count(main[main$year<=2015, ], year, cat)
serving_size <- aggregate(data=main[main$year<=2015, ],
                  serving_size~year+cat, FUN=mean, na.rm=TRUE)
calories <- aggregate(data=main[main$year<=2015, ],
                  calories~year+cat, FUN=mean, na.rm=TRUE)
sat_fat <- aggregate(data=main[main$year<=2015, ],
                      fat_pct~year+cat, FUN=mean, na.rm=TRUE)
trend <- merge(trend, serving_size, by=c("cat", "year"))
trend <- merge(trend, calories, by=c("cat", "year"))
trend <- merge(trend, sat_fat, by=c("cat", "year"))
rm(serving_size, calories, sat_fat)

trend$cat <- paste0(toupper(substr(trend$cat, 1, 1)), 
                    substr(trend$cat, 2, nchar(trend$cat)))
rm(main)

### plot specific menu item data ----
cat <- factor(trend$cat,
              levels=c("Burrito", "Quesadilla", "Taco", "Other"))

ggplot(data=trend, aes(x=as.character(year), y=n,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Number of menu items over time",
           x="Year", y="Number of items", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/num_menu_item_sandwiches.jpeg", width=20, height=10, unit="cm")

# serving size
unit <- factor(paste(trend$cat, " (g)", sep = ""),
               levels=c("Burrito (g)", "Quesadilla (g)",
                        "Taco (g)", "Other (g)"))
ggplot(data=trend, aes(x=as.character(year), y=serving_size,
                       group=unit, col=unit)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean serving sizes over time",
           x="Year", y="Mean serving size (g)", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/serving-size_sandwiches.jpeg", width=20, height=10, unit="cm")

# calories
ggplot(data=trend, aes(x=as.character(year), y=calories,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean calories over time",
           x="Year", y="Mean calories", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/mean-calories_sandwiches.jpeg", width=20, height=10, unit="cm")

# saturated fat
ggplot(data=trend, aes(x=as.character(year), y=fat_pct,
                       group=cat, col=cat)) +
      geom_point() +
      geom_line(size=1) +
      labs(title="Mean saturated fat as % of total calories over time",
           x="Year", y="%", col="Category",
           caption="Data source: MenuStat, http://menustat.org") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/tb-menu/sat-fat_sandwiches.jpeg", width=20, height=10, unit="cm")

burrito <- subset(menu,
                  (menu$category=="Sandwiches"|menu$category=="Entrees")&grepl("Burrito", menu$item_name)&menu$year<=2015,
                  select=c("id","item_name","year", "serving_size", "calories", "saturated_fat"))
burrito <- burrito[order(burrito$id, burrito$year), ]
burrito <- burrito[, c("id", "item_name", "year")]

Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
}
name <- aggregate(data=burrito,
      item_name~id, FUN=Mode)
colnames(name)[2] <- "name"
burrito <- merge(burrito, name, by="id")
burrito <- burrito[order(burrito$id, burrito$year), ]
rm(name)
burrito$id <- NULL
burrito <- reshape(burrito, direction = "wide",
                   idvar = "name",
                   timevar = "year")
names(burrito)
colnames(burrito)[2:7] <- c("2008", "2010", "2012", "2013", "2014", "2015")
burrito[, 2:7][!is.na(burrito[, 2:7])] <- "1"
burrito[, 2:7][is.na(burrito[, 2:7])] <- "0"
burrito[2:7] <- sapply(burrito[2:7], as.integer)
sapply(burrito, class)





### link menu stat to transaction data ----
sample <- menu[menu$item_name=="Bean Burrito", ]

