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
library(plotmo) #visualize summary plot after lasso regression
#install.packages("splitstackshape")
library(splitstackshape) #transform strings into binary predictors

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
      dplyr::separate(item_name, c("item_name1", "item_name2", "item_name3", "item_name4",
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

### re-categorize food, add indicator for drinks ----
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
menu$drink <- ifelse(menu$category=="Beverages", 1, 0)

### separate training and testing data ----
# randomly selected 80% of the data as training
set.seed(5)
menu$select <- sample.int(length(menu$full), length(menu$full))
summary(menu$select)
menu <- menu[order(menu$select), ]

whole_set <- menu[, c(3, 23, 25:26, 7)]
whole_set <- concat.split.expanded(data = whole_set, split.col = "full", sep = " ",
                                   fill=0, type="character")

training <- whole_set[whole_set$select <= length(whole_set$select)*0.8,]
testing <- whole_set[whole_set$select > length(whole_set$select)*0.8,]

# drop the full ite name itself from training and testing data
training$full <- NULL
testing$full <- NULL

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
#make bar plot of top 20 most frequent words
#ggplot()

rm(rquery.wordcloud, cloud_menustat, cloud_tb, freq)

### lasso regression ----
# training
x <- model.matrix(calories~., training)[, -1]
y <- training$calories

cv.lasso <- cv.glmnet(x, y, alpha=1, family="gaussian")
plot(cv.lasso)
cv.lasso$lambda.min
coef(cv.lasso, cv.lasso$lambda.min)

lasso.model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min, family="gaussian")

# testing
x.test <- model.matrix(calories~., testing)[, -1]
pred.calorie <- lasso.model %>% predict(newx = x.test)
obs.calorie <- testing$calories

# summary stats
plot(x=pred.calorie, y=obs.calorie, main="Predicted calorie v. Observed in testing data (N=366)",
     xlab="Fitted calories", ylab="Observed calories",
     xlim=c(-210, 1150), ylim=c(-200, 2020))
abline(a=0, b=1, color="red")
plot(x=pred.calorie - obs.calorie, y=obs.calorie, main="Residaul plot of testing data (N=366)",
     xlab="Residuals", ylab="Observed calories")

print(lasso.model$dev.ratio)
mean(abs(pred.calorie - obs.calorie)) / mean(obs.calorie) #16.67%

# look at predicted calories by food category, visualize
testing <- menu[menu$select > length(menu$select)*0.8, ]
testing <- cbind(testing, pred.calorie)
names(testing)
colnames(testing)[27] <- "pred.calorie"

# fitted v observed
ggplot(data=testing, aes(x=pred.calorie, y=calories,
           group=as.factor(cat), col=as.factor(cat))) +
      geom_point(size=2) +
      labs(title="Predicted calories v. observed",
           x="Predicted", y="Observed", col="Category",
           caption="Note: not yet") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))

# residual plot
ggplot(data=testing, aes(x=pred.calorie, y=pred.calorie - calories,
                         group=as.factor(cat), col=as.factor(cat))) +
      geom_point(size=2) +
      labs(title="Predicted calories v. observed",
           x="Predicted", y="Residuals", col="Category",
           caption="Note: not yet") +
      scale_color_brewer(palette="Set3") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"))

# calculate mean residuals by category
mean(abs(testing$calories[testing$cat=="bowl"] - testing$pred.calorie[testing$cat=="bowl"])) /
      mean(testing$calories[testing$cat=="bowl"]) #0.1589
mean(abs(testing$calories[testing$cat=="burrito"] - testing$pred.calorie[testing$cat=="burrito"])) /
      mean(testing$calories[testing$cat=="burrito"]) #0.0853
mean(abs(testing$calories[testing$cat=="dessert"] - testing$pred.calorie[testing$cat=="dessert"])) /
      mean(testing$calories[testing$cat=="dessert"]) #0.2187
mean(abs(testing$calories[testing$cat=="drink"] - testing$pred.calorie[testing$cat=="drink"])) /
      mean(testing$calories[testing$cat=="drink"]) #0.1752
mean(abs(testing$calories[testing$cat=="nacho"] - testing$pred.calorie[testing$cat=="nacho"])) /
      mean(testing$calories[testing$cat=="nacho"]) #0.0818
mean(abs(testing$calories[testing$cat=="other"] - testing$pred.calorie[testing$cat=="other"])) /
      mean(testing$calories[testing$cat=="other"]) #0.4648
mean(abs(testing$calories[testing$cat=="quesadilla"] - testing$pred.calorie[testing$cat=="quesadilla"])) /
      mean(testing$calories[testing$cat=="quesadilla"])  #0.07828156
mean(abs(testing$calories[testing$cat=="salad"] - testing$pred.calorie[testing$cat=="salad"])) /
      mean(testing$calories[testing$cat=="salad"]) #0.1298
mean(abs(testing$calories[testing$cat=="sauce"] - testing$pred.calorie[testing$cat=="sauce"])) /
      mean(testing$calories[testing$cat=="sauce"]) #1.475421
mean(abs(testing$calories[testing$cat=="taco"] - testing$pred.calorie[testing$cat=="taco"])) /
      mean(testing$calories[testing$cat=="taco"]) #0.1675399



# try AIC, BIC
# try interactions with binary predictors


