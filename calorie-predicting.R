### using key words to predict calorie info
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
#install.packages(c("dplyr", "tidyr", "stringr", "ggplot2", "tidyverse", "textstem", "tm", "glmnet))
#install.packages(c("SnowballC", "wordcloud", "RcolorBrewer"))
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(glmnet)
library(textstem)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
#install.packages("plotmo")
library(plotmo)

### read menu stat data ----
menu <- read.csv("data/menustat/nutrition_info_all.csv", stringsAsFactors = FALSE)
names(menu)
menu$item_name <- toupper(menu$item_name)
length(unique(menu$id))

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
menu <- menu[, -c(1:13, 35:47)]

### remove unnecessary text/stop words, punctuation, whites pace ----
menu$full <- sapply(strsplit(menu$full, "FOR "), "[", 1)
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
menu$full <- removeWords(menu$full, stop)
rm(stop)

menu$full <- removePunctuation(menu$full)
menu$full <- stripWhitespace(menu$full)

#lemmatization
menu$full <- toupper(lemmatize_strings(menu$full))

# fix numbers: 1/2, 1/3, etc
menu$full <- gsub(" / ", "/", menu$full)
menu$full <- gsub("43832", "1/2", menu$full)

# fix words lemmatization didnt address
menu$full <- gsub("7LAYER", "7 LAYERO", menu$full)
menu$full <- gsub("5LAYER", "5 LAYERO", menu$full)
menu$full <- gsub("TACOS", "TACO", menu$full)
menu$full <- gsub("BURGERS", "BURGER", menu$full)
menu$full <- gsub("NACHOS", "NACHO", menu$full)
length(unique(menu$full)) #759
names(menu)
menu$full <- tolower(menu$full)
#menu$full <- ifelse(grepl("AM|A.M.", menu$item_description),
#                    gsub("be", "am", menu$full), menu$full)

### re-categorize food ----
menu$cat <- ifelse(grepl("salad", menu$full), "salad",
                   ifelse(grepl("sauce|dressing|salsa", menu$full), "sauce",
                          ifelse(grepl("taco|chalupa|gordita|tostada", menu$full), "taco",
                                 ifelse(grepl("burrito|quesarito|griller|crunchwrap|enchirito|taquito", menu$full), "burrito",
                                        ifelse(grepl("quesadilla|flatbread|pizza|doubledilla|meximelt", menu$full), "quesadilla",
                                               ifelse(grepl("nacho", menu$full), "nacho",
                                                      ifelse(grepl("cinnamon|cinnabon|cookie|brownie|churro|caramel apple empanada", menu$full), "dessert",
                                                             ifelse(menu$category=="Beverages"|grepl("beverages", menu$full), "drink",
                                                                    ifelse(grepl("bowl", menu$full), "bowl", "other")))))))))
table(menu$cat)

names(menu)
menu <- menu[, c(3, 7, 23:24)]
menu <- subset(menu, cat!="drink"&cat!="other"&cat!="dessert")

menu$salad <- ifelse(grepl("salad", menu$full), 1, 0)
menu$sauce <- ifelse(grepl("sauce", menu$full), 1, 0)
menu$dressing <- ifelse(grepl("dressing", menu$full), 1, 0)
menu$salsa <- ifelse(grepl("salsa", menu$full), 1, 0)
menu$taco <- ifelse(grepl("taco", menu$full), 1, 0)
menu$chalupa <- ifelse(grepl("chalupa", menu$full), 1, 0)
menu$gordita <- ifelse(grepl("gordita", menu$full), 1, 0)
menu$tostada <- ifelse(grepl("tostada", menu$full), 1, 0)
menu$burrito <- ifelse(grepl("burrito", menu$full), 1, 0)
menu$quesarito <- ifelse(grepl("quesarito", menu$full), 1, 0)
menu$grill <- ifelse(grepl("grill", menu$full), 1, 0)
menu$crunchwrap <- ifelse(grepl("crunchwrap", menu$full), 1, 0)
menu$enchirito <- ifelse(grepl("enchirito", menu$full), 1, 0)
menu$taquito <- ifelse(grepl("taquito", menu$full), 1, 0)
menu$quesadilla <- ifelse(grepl("quesadilla", menu$full), 1, 0)
menu$flatbread <- ifelse(grepl("flatbread", menu$full), 1, 0)
menu$pizza <- ifelse(grepl("pizza", menu$full), 1, 0)
menu$doubledilla <- ifelse(grepl("doubledilla", menu$full), 1, 0)
menu$meximelt <- ifelse(grepl("meximelt", menu$full), 1, 0)
menu$bowl <- ifelse(grepl("bowl", menu$full), 1, 0)
menu$chicken <- ifelse(grepl("chicken", menu$full), 1, 0)
menu$beef <- ifelse(grepl("beef", menu$full), 1, 0)
menu$steak <- ifelse(grepl("steak", menu$full), 1, 0)
menu$rice <- ifelse(grepl("rice", menu$full), 1, 0)
menu$bean <- ifelse(grepl("bean", menu$full), 1, 0)
menu$cheese <- ifelse(grepl("chees", menu$full), 1, 0)
menu$large <- ifelse(grepl("large", menu$full), 1, 0)
menu$extra <- ifelse(grepl("extra", menu$full), 1, 0)
menu$nacho <- ifelse(grepl("nacho", menu$full), 1, 0)
menu$shell <- ifelse(grepl("shell", menu$full), 1, 0)
menu$bacon <- ifelse(grepl("bacon", menu$full), 1, 0)
menu$egg <- ifelse(grepl("egg", menu$full), 1, 0)
menu$veggie <- ifelse(grepl("veg", menu$full), 1, 0)
menu$chili <- ifelse(grepl("chili", menu$full), 1, 0)
menu$cantina <- ifelse(grepl("cantina", menu$full), 1, 0)
menu$fresco <- ifelse(grepl("fresco", menu$full), 1, 0)
menu$`3` <- ifelse(grepl("3", menu$full), 1, 0)
menu$`5` <- ifelse(grepl("5", menu$full), 1, 0)
menu$supreme <- ifelse(grepl("supreme", menu$full), 1, 0)
menu$fiesta <- ifelse(grepl("fiesta", menu$full), 1, 0)
menu$stuft <- ifelse(grepl("stuft", menu$full), 1, 0)
menu$layer <- ifelse(grepl("layer", menu$full), 1, 0)
menu$crunch <- ifelse(grepl("crunch", menu$full), 1, 0)
menu$double <- ifelse(grepl("double", menu$full), 1, 0)
menu$triple <- ifelse(grepl("triple", menu$full), 1, 0)
menu$deck <- ifelse(grepl("deck", menu$full), 1, 0)
menu$breakfast <- ifelse(grepl("breakfast", menu$full), 1, 0)
menu$baja <- ifelse(grepl("baja", menu$full), 1, 0)
menu$sausage <- ifelse(grepl("sausage", menu$full), 1, 0)
menu$lb <- ifelse(grepl("1/2 lb", menu$full), 1, 0)
menu$soft <- ifelse(grepl("soft", menu$full), 1, 0)
menu$bellgrande <- ifelse(grepl("bellgrande", menu$full), 1, 0)
menu$chip <- ifelse(grepl("chip", menu$full), 1, 0)
menu$cream <- ifelse(grepl("cream", menu$full), 1, 0)
menu$potato <- ifelse(grepl("potato", menu$full), 1, 0)
menu$mini <- ifelse(grepl("mini", menu$full), 1, 0)
menu$grande <- ifelse(grepl("grande", menu$full), 1, 0)

### separate training and testing data ----
# randomly selected 80% of the data as training
set.seed(5)
menu$select <- sample.int(length(menu$full), length(menu$full))
summary(menu$select)
menu <- menu[order(menu$select), ]
training <- menu[c(1:as.integer(length(menu$full)*0.8)), -c(3:4, 62)]
testing <- menu[c(1:(length(menu$full)-as.integer(length(menu$full)*0.8))), -c(3:4, 62)]

### make word cloud ----
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=2, max.words=200) { 
      if(type[1]=="file") text <- readLines(x)
      else if(type[1]=="url") text <- html_to_text(x)
      else if(type[1]=="text") text <- x
      
      # Load the text as a corpus
      docs <- Corpus(VectorSource(text))
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove stopwords for the language 
      docs <- tm_map(docs, removeWords, stopwords(lang))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      # Remove your own stopwords
      if(!is.null(excludeWords)) 
            docs <- tm_map(docs, removeWords, excludeWords) 
      # Text stemming
      if(textStemming) docs <- tm_map(docs, stemDocument)
      # Create term-document matrix
      tdm <- TermDocumentMatrix(docs)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      # check the color palette name 
      if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
      else colors = brewer.pal(8, colorPalette) 
      # Plot the word cloud
      set.seed(1234)
      wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
                random.order=FALSE, rot.per=0.35, 
                use.r.layout=FALSE, colors=colors)
      
      invisible(list(tdm=tdm, freqTable = d))
}

cloud_menustat <- rquery.wordcloud(x=menu$full, type="text", lang="english",
                             min.freq = 2, max.words = 200)
freq <- cloud_menustat$freqTable
ggplot()


rm(rquery.wordcloud, cloud_menustat, cloud_tb, freq)


### lasso regression ----
x <- model.matrix(calories~., training)[, -1]
y <- training$calories

cv.lasso <- cv.glmnet(x, y, alpha=1, family="gaussian")
plot(cv.lasso)
cv.lasso$lambda.min
coef(cv.lasso, cv.lasso$lambda.min)

lasso.model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min, family="gaussian")

### testing
x.test <- model.matrix(calories~., testing)[, -1]
pred.calorie <- lasso.model %>% predict(newx = x.test)
obs.calorie <- testing$calories
plot(x=pred.calorie, y=obs.calorie, main="Predicted calorie v. Observed in testing data (N=198)",
     xlab="Fitted calories", ylab="Observed calories",
     xlim=c(-200, 1200), ylim=c(-200, 1200))
abline(a=0, b=1, color="red")
plot(x=pred.calorie - obs.calorie, y=obs.calorie, main="Residaul plot of testing data (N=198)",
     xlab="Residuals", ylab="Observed calories")

print(lasso.model$dev.ratio)

