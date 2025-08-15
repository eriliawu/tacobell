### check possible errors in menu stat

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
library(dplyr)
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)

### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
names(menu)
menu$item_name <- toupper(menu$item_name)
length(unique(menu$id)) #700

### read corrected string file ----
strings <- read.csv("data/menu-matching/product-names_unique_substrings_bow_menustat_corrected.csv",
                    stringsAsFactors = FALSE)
sapply(strings, class)
strings <- strings[, -c(2:3)]

# fix numbers that excel automatically converted to dates
strings$original[strings$original=="02-Jan"] <- "1/2"

# replace abbreviations with full spelling
# first, break product names into separate substrings in their own columns
# second, merge each column with the replacement strings
menu <- menu %>%
      separate(item_name, c("item_name1", "item_name2", "item_name3", "item_name4",
                            "item_name5", "item_name6", "item_name7", "item_name8",
                            "item_name9", "item_name10", "item_name11", "item_name12",
                            "item_name13"), " ")

for (i in c(1:13)) {
      menu <- merge(menu, strings, by.x=paste0("item_name", i), by.y="original", sort=FALSE, all.x = TRUE)
      colnames(menu)[i+34] <- paste0("full", i)
}
rm(i, strings)

# paste all substrings, but leave out the NAs
menu$item_name <- apply(cbind(menu$item_name1, menu$item_name2, menu$item_name3,
                              menu$item_name4, menu$item_name5, menu$item_name6,
                              menu$item_name7, menu$item_name8, menu$item_name9,
                              menu$item_name10, menu$item_name11, menu$item_name12,
                              menu$item_name13),
                        1, function(x) paste(x[!is.na(x)], collapse = " "))
menu$full <- apply(cbind(menu$full1, menu$full2, menu$full3, menu$full4,
                         menu$full5, menu$full6, menu$full7, menu$full8,
                         menu$full9, menu$full10, menu$full11, menu$full12, menu$full13),
                   1, function(x) paste(x[!is.na(x)], collapse = " "))

names(menu)
menu <- menu[, c(14, 49, 15:34, 48)]


### remove unnecessary text/stop words, punctuation, whites pace ----
menu$rename <- sapply(strsplit(menu$full, "FOR "), "[", 1)

stop <- c("CENT", "CENTS", "FOR 2", "VERSION", "COUPON", "HNN", "DENVER",
          "SANTA FE", "6 TO 1", "SOS", "FOR 4", "SMT", "SCHOOL LUNCH", "UPSELL",
          "MADISON OKC", "OMA", "BUT", "IF", "BETWEEN", "INTO", "THROUGH",
          "DURING", "BEFORE", "AFTER", "AT", "BY", "FOR", "WITH", "ABOUT",
          "OR", "BECAUSE", "AS", "UNTIL", "WHILE", "OF", "AGAINST", "ABOVE",
          "BELOW", "TO", "FROM", "UP", "DOWN", "IN", "OUT", "ON", "OFF", "OVER",
          "UNDER", "AGAIN", "FURTHER", "THEN", "ONCE", "HERE", "THERE", "ALL",
          "ANY", "BOTH", "EACH", "MORE", "OTHER", "SOME", "NOR", "NOT", "ONLY",
          "OWN", "SAME", "SO", "THAN", "TOO", "VERY", "ADD", "TB", "CRAVINGS",
          "WHY PAY MORE VALUE MENU", "STYLE", "REGIONAL", "CALLED", "ALSO",
          "15 CALORIE", "BUCK BOX", "USDA", "SELECT", "LAS", "VEGAS")
menu$rename <- removeWords(menu$rename, stop)
rm(stop)

menu$rename <- removePunctuation(menu$rename)
menu$rename <- stripWhitespace(menu$rename)

#lemmatization
menu$rename <- toupper(lemmatize_strings(menu$rename))

# fix numbers: 1/2, 1/3, etc
menu$rename <- gsub(" / ", "/", menu$rename)

# fix words lemmatization didnt address
menu$rename <- gsub("7LAYER", "7 LAYERO", menu$rename)
menu$rename <- gsub("5LAYER", "5 LAYERO", menu$rename)
menu$rename <- gsub("TACOS", "TACO", menu$rename)
menu$rename <- gsub("BURGERS", "BURGER", menu$rename)
menu$rename <- gsub("NACHOS", "NACHO", menu$rename)

menu <- menu[, c(1,24,2:22,2,23)]

# drop "regular" and "extra" from menu names
menu$rename <- gsub(" REGULAR", "", menu$rename)
menu$rename <- ifelse(grepl("EXTRA", menu$rename)&!grepl("EXTRA LARGE|EXTRA EXTRA LARGE", menu$rename),
                      gsub("EXTRA", "", menu$rename), menu$rename)

### by-year analysis, repeat menu items ----
# select 2 items from each category, track calorie by year
table(menu$category)
menu$item_name <- tolower(menu$item_name)
menu$cat <- ifelse(grepl("salad", menu$item_name), "salad",
            ifelse(grepl("sauce|dressing|salsa", menu$item_name), "sauce",
            ifelse(grepl("taco|chalupa|gordita|tostada", menu$item_name), "taco",
            ifelse(grepl("burrito|quesarito|griller|crunchwrap|enchirito|taquito", menu$item_name), "burrito",
            ifelse(grepl("quesadilla|flatbread|pizza|doubledilla|meximelt", menu$item_name), "quesadilla",
            ifelse(grepl("nacho", menu$item_name), "nacho",
            ifelse(grepl("cinnamon|cinnabon|cookie|brownie|churro|caramel apple empanada", menu$item_name), "dessert",
            ifelse(menu$category=="Beverages"|grepl("beverages", menu$item_name), "drink",
            ifelse(grepl("bowl", menu$item_name), "bowl", "other")))))))))
table(menu$cat)
table(menu$rename[menu$cat=="quesadilla"], menu$year[menu$cat=="quesadilla"])

sample <- menu[menu$rename=="FIESTA TACO SALAD CHICKEN"|
                     menu$rename=="FIESTA TACO SALAD BEEF"|
                     menu$rename=="NACHO SUPREME"|
                     menu$rename=="TRIPLE LAYER NACHO"|
                     menu$rename=="NACHO BELLGRANDE"|
                     menu$rename=="BURRITO SUPREME CHICKEN"|
                     menu$rename=="BEAN BURRITO"|
                     menu$rename=="SOFT TACO SUPREME BEEF"|
                     menu$rename=="GORDITA SUPREME CHICKEN"|
                     menu$rename=="MEXICAN PIZZA"|
                     menu$rename=="MEXIMELT", ]
sample <- sample[order(sample$id, sample$year), ]
sample <- sample[sample$id!=2708, ]

# visualize calories
ggplot(data=sample,
       aes(x=as.character(year), y=calories,
           group=as.factor(rename),
           col=as.factor(rename))) +
      geom_point() +
      geom_line() +
      labs(title="Calorie change, select items",
           x="Year", y="Calories", col="Item",
           caption="Note: taco salads available 2012-2018, triple layer nacho available 2008, 2010, 2015-2018.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/menustat-menu-item-analysis/select-item-calorie-changes.jpeg", width=20, height=10, unit="cm")

ggplot(data=sample,
       aes(x=as.character(year), y=serving_size,
           group=as.factor(rename),
           col=as.factor(rename))) +
      geom_point() +
      geom_line() +
      labs(title="Serving size change, select items",
           x="Year", y="Calories", col="Item",
           caption="Note: taco salads available 2012-2018, triple layer nacho available 2008, 2010, 2015-2018.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("tables/menustat-menu-item-analysis/select-item-serving-size-changes.jpeg", width=20, height=10, unit="cm")

### by-year analysis, items with limited availability ----
# calculate the number of years an item is available
menu <- menu %>%
      group_by(id) %>%
      mutate(count=n())
summary(menu$count)
hist(x=menu$count, main="Number of years an item is available",
     xlab="Number of years", ylab = "Frequency")
table(menu$year[menu$count==1])

# make the same mean calorie plot, drop items only available for a year
sample <- menu[menu$count>=2 & menu$cat!="other", ]
sample <- aggregate(data=sample, calories~cat+year, mean)
ggplot(data=sample,
       aes(x=as.character(year), y=calories,
           group=as.factor(cat), col=as.factor(cat))) +
      geom_point() +
      geom_line() +
      labs(title="Mean calories, by category",
           x="Year", y="Calories", col="Category",
           caption="Note: data exclude items that were only available in one year.") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/menustat-menu-item-analysis/mean-calorie-by-cat-drop1year-only.jpeg", width=20, height=10, unit="cm")

# count number of items available each year, by cat
sample <- menu[menu$cat!="other", ]
sample$count <- 1
sample <- aggregate(data=sample, count~cat+year, sum)
ggplot(data=sample,
       aes(x=as.character(year), y=count,
           group=as.factor(cat), col=as.factor(cat))) +
      geom_point() +
      geom_line() +
      labs(title="Number of items available, by category",
           x="Year", y="N", col="Category",
           caption='Note: data exclude "other" category.') +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))
ggsave("tables/menustat-menu-item-analysis/num-items-available-by-year.jpeg", width=20, height=10, unit="cm")
