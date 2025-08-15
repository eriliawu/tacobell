### assess human raters

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### load libraries ----
#install.packages("irr")
library(irr)

### compare turk inter-rater reliability ----
turk <- read.csv("data/menu-matching/manual-match/mturk/pilot/pilot200-results-simple.csv",
                 stringsAsFactors = FALSE)
names(turk)
#turk$case <- NULL
turk$worker <- rep(c(1:2), 200)
turk <- reshape(turk, direction="wide", idvar = c("task", "case"), timevar = "worker")
names(turk)

# re-construct character answers to numeric
turk$answer.1 <- as.numeric(substr(turk$answer.1, 1, 1))
turk$answer.2 <- as.numeric(substr(turk$answer.2, 1, 1))
table(turk$answer.1, turk$answer.2)

## if we consider answer.1 and answer.2 were rated by the same set of raters
# unweighted kappa
kappa2(turk[1:98, 3:4], "unweighted") #0.53
kappa2(turk[, 3:4], "unweighted") #0.70 

# squared and linear weights
kappa2(turk[, 3:4], "squared")#0.82
kappa2(turk[, 3:4], "equal")#0.77

kappa2(turk[1:98, 3:4], "squared")#0.65
kappa2(turk[1:98, 3:4], "equal")#0.59

## ICC, if raters are randomly selected for each set of pairs
icc(turk[, 3:4], model="oneway", type="agreement", unit="average")#0.9
icc(turk[1:98, 3:4], model="oneway", type="agreement", unit="average")#0.793

### RAs reliability, 1:1 pilot ----
ra <- read.csv("data/menu-matching/manual-match/RA/pilot/manual-matching-best-match_pilot100-results.csv",
               stringsAsFactors = FALSE)
names(ra)
sapply(ra, class)
ra$dm <- ifelse(ra$DM==1&ra$certainDM==1, 1,
                ifelse((ra$DM==1&ra$certainDM==0)|(ra$DM==0&ra$certainDM==1), 2, 3))
ra$ar <- ifelse(ra$AR==1&ra$certainAR==1, 1,
                ifelse((ra$AR==1&ra$certainAR==0)|(ra$AR==0&ra$certainAR==1), 2, 3))
ra$cs <- ifelse(ra$CS==1&ra$certainCS==1, 1,
                ifelse((ra$CS==1&ra$certainCS==0)|(ra$CS==0&ra$certainCS==1), 2, 3))
ra$ek <- ifelse(ra$EK==1&ra$certainEK==1, 1,
                ifelse((ra$EK==1&ra$certainEK==0)|(ra$EK==0&ra$certainEK==1), 2, 3))
ra$kn <- ifelse(ra$KN==1&ra$certainKN==1, 1,
                ifelse((ra$KN==1&ra$certainKN==0)|(ra$KN==0&ra$certainKN==1), 2, 3))
ra <- ra[, c(1:3, 14:18)]
names(ra)

## unweighted kappa
kappam.fleiss(ra[, 3:8]) #0.29

## fleiss's kappa, for each pair of RA
# cols 4:8
kappa2(ra[, c(4,5)], "squared")#0.58
kappa2(ra[, c(4,6)], "squared")#0.76
kappa2(ra[, c(4,7)], "squared")#0.55
kappa2(ra[, c(4,8)], "squared")#0.76
kappa2(ra[, c(5,6)], "squared")#0.71
kappa2(ra[, c(5,7)], "squared")#0.82
kappa2(ra[, c(5,8)], "squared")#0.85
kappa2(ra[, c(6,7)], "squared")#0.66
kappa2(ra[, c(6,8)], "squared")#0.86
kappa2(ra[, c(7,8)], "squared")#0.83

## ICC
icc(ra[, 4:8], model="twoway", type="agreement", unit="average")#0.935

### compare turk, 2nd round, 1v5 pilot ----
turk2 <- read.csv("data/menu-matching/manual-match/mturk/pilot/pilot200-1v5-results-simple.csv",
                 stringsAsFactors = FALSE)
names(turk2)
#turk2 <- turk2[, -c(4:7)]

# see how many workers did more than 1 matching
turk2 <- turk2 %>%
      group_by(worker) %>%
      mutate(count=n()) %>%
      mutate(rank=seq(1, count[1], 1))
table(turk2$count)
turk2 <- as.data.frame(turk2[, -c(5:6)])

# convert answers back to original ordering
# item2: 3, item3: 1, item4: 4, item 5: 2
table(turk2$answer)
turk2$answer <- ifelse(turk2$answer=="Item 2", "3",
                  ifelse(turk2$answer=="Item 3", "1",
                  ifelse(turk2$answer=="Item 4", "4",
                  ifelse(turk2$answer=="Item 5", "2", "5"))))
turk2$answer <- as.integer(turk2$answer)

# reshape to wide
colnames(turk2)[3] <- "task"
turk2$worker <- rep(c(1:3))
turk2 <- reshape(turk2, direction="wide",
                 idvar = c("task", "case", "item1", "item2", "item3", "item4"),
                 timevar = "worker")
turk2$case <- NULL
names(turk2)
#write.csv(turk2[turk2$answer.1==turk2$answer.2&turk2$answer.1==turk2$answer.3, ],
#          "data/menu-matching/manual-match/mturk/pilot/full-agreement_pilot2.csv",
#          row.names = FALSE)

# compare cohen's kappa, weighted
kappa2(turk2[, 2:3], "squared")#0.619
kappa2(turk2[, c(2, 4)], "squared")#0.505
kappa2(turk2[, 3:4], "squared")#0.586

# fleiss's kappa
kappam.fleiss(turk2[, 2:4]) #0.511

# ICC
icc(turk2[, 2:4], model="oneway", type="agreement", unit="average") #0.8  

# kendall's w
kendall(turk2[, 2:4], correct = TRUE) #0.725

# looking at specific numbers
# both 1 and 5 are reasonable numbers
# reasonable combo choices: both 1, both 5, either 1 or 5
table(turk2$answer.1, turk2$answer.2) #0.609
table(turk2$answer.1, turk2$answer.3) #0.645
table(turk2$answer.2, turk2$answer.3) #0.669

# any one of the raters answered neither 1 nor 5
length(turk2$task[(turk2$answer.1!=1&turk2$answer.1!=5)|
                        (turk2$answer.2!=1&turk2$answer.2!=5)|
                        (turk2$answer.3!=1&turk2$answer.3!=5)]) #74

# all raters gave answers other than 1 or 5
length(turk2$task[(turk2$answer.1!=1&turk2$answer.1!=5)&
                        (turk2$answer.2!=1&turk2$answer.2!=5)&
                        (turk2$answer.3!=1&turk2$answer.3!=5)]) #19

# % of all 1, all 5, majority 1, majority 5
length(turk2$task[turk2$answer.1==1&turk2$answer.2==1&turk2$answer.3==1]) #52
length(turk2$task[turk2$answer.1==5&turk2$answer.2==5&turk2$answer.3==5]) #22
length(turk2$task[(turk2$answer.1==1&turk2$answer.2==1&turk2$answer.3!=1)|
                        (turk2$answer.1==1&turk2$answer.3==1&turk2$answer.2!=1)|
                        (turk2$answer.3==1&turk2$answer.2==1&turk2$answer.1!=1)]) #29
length(turk2$task[(turk2$answer.1==5&turk2$answer.2==5&turk2$answer.3!=5)|
                        (turk2$answer.1==5&turk2$answer.3==5&turk2$answer.2!=5)|
                        (turk2$answer.3==5&turk2$answer.2==5&turk2$answer.1!=5)]) #25

### RA reliability, full data launch, 2 rounds ----
ra <- read.csv("data/menu-matching/manual-match/RA/manual-matching-best-match-2rounds.csv",
               stringsAsFactors = FALSE)
names(ra)
ra$X <- NULL

# reliability
kappa2(ra[99:5666, c(3, 5)], "squared")#0.716

### check sales volume represented by matched items ----
## consolidate yes or no match for all items
table(ra$ra_agreement)
table(ra$wu)

# run the match-menu-item.R file to get join_jaccard table
names(ra)
ra <- ra[, c(1:2, 7:8)]
colnames(ra)[c(1:2, 4)] <- c("full", "item_name", "match")
ra <- ra[!duplicated(paste0(ra$full, ra$item_name)), ]
ra <- merge(ra, join_jaccard, by=c("full", "item_name"))
names(ra)
ra <- ra[ra$ra_agreement!="no", c(3:4, 7)]

# match with tacobell sales data
sales_all_ra <- NULL
sales_all_wu <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
#sapply(detail, class)

for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                        sales <- read.csv(paste0("data/from-bigpurple/sales-vol-by-product/sales_",
                                                 i, "_Q0", j, ".csv"),
                                          sep = ";", header = FALSE, quote = "\"'",
                                          stringsAsFactors = FALSE,
                                          col.names = c("p_detail", "sales", "qty"))
                        sales <- merge(detail, sales, by="p_detail", all=TRUE)

                        # clean house
                        sales <- sales[!is.na(sales$sales), ]
                        sales <- sales[!grepl("AWR", sales$detail_desc)&!grepl("AW ", sales$detail_desc)&
                                             !grepl("BYB", sales$detail_desc)&!grepl("KFC", sales$detail_desc)&
                                             !grepl("LJS", sales$detail_desc)&!grepl("PH", sales$detail_desc)&
                                             !grepl("PIZZA HUT", sales$detail_desc)&!grepl("TCBY", sales$detail_desc)&
                                             !grepl("ICBIY", sales$detail_desc)&!grepl("KRYSTAL", sales$detail_desc), ]
                        sales <- sales[sales$detail_desc!="CFM MANAGER SPECIALS"&
                                             sales$detail_desc!=""&
                                             !grepl("* NEW PRODCT ADDED BY", sales$detail_desc)&
                                             !grepl("COMBO", sales$detail_desc)&
                                             !grepl("FRANCHISE LOCAL MENU", sales$detail_desc)&
                                             sales$detail_desc!="NEW ITEM"&
                                             !grepl("SPECIAL PROMOTION", sales$detail_desc), ]
                        
                        sales <- merge(sales, ra, by.x = "detail_desc", by.y = "product", all = TRUE)
                        sales <- sales[!is.na(sales$p_detail), ]
                        
                        sales <- sales[!duplicated(sales$p_detail), ]
                        sales$ra_agreement[is.na(sales$ra_agreement)] <- "no"
                        
                        # fill in summary stats, aggregated by results from ra
                        sales_ra <- aggregate(data=sales, qty~ra_agreement, sum)
                        sales_ra$year <- i
                        sales_ra$quarter <- j
                        sales_ra$pct <- sales_ra$qty / sum(sales_ra$qty)
                        sales_all_ra <- rbind(sales_all_ra, sales_ra)
                        
                        # fill in summary stats, aggregated by results from ra and me
                        sales_wu <- aggregate(data=sales, qty~match, sum)
                        sales_wu$year <- i
                        sales_wu$quarter <- j
                        sales_wu$pct <- sales_wu$qty / sum(sales_wu$qty)
                        sales_all_wu <- rbind(sales_all_wu, sales_wu)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales_ra, sales_wu)

# visualization, resutls from me
ggplot(data=sales_all_wu, aes(x=paste0(year, "Q",quarter), y=pct,
                           group=as.factor(sales_all_wu$match),
                           color=as.factor(sales_all_wu$match))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      scale_color_manual(name="Match",
                         values=c("red", "green"),
                         labels=c("Not a match", "Match")) +
      labs(title="Sales volume (# of items sold)",
           x="Time", y="Percent",
           caption="Note: 737 items are considered a match.") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items-ra+wu.jpeg", width=20, height=10, unit="cm")

# visualization, resutls from ra
ggplot(data=sales_all_ra, aes(x=paste0(year, "Q",quarter), y=pct,
                              group=as.factor(sales_all_ra$ra_agreement),
                              color=as.factor(sales_all_ra$ra_agreement))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      scale_color_manual(name="RA agreement",
                         values=c("blue", "red", "green"),
                         labels=c("Maybe", "Not a match", "Match")) +
      labs(title="Sales volume (# of items sold)",
           x="Time", y="Percent",
           caption="Note: 626 items - match, 360 items - maybe a match.") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items-ra.jpeg", width=20, height=10, unit="cm")
rm(ra, sales_all_ra, sales_all_wu)

### list of items by matching category, in sales volume, descending order ----
ra <- read.csv("data/menu-matching/manual-match/RA/manual-matching-best-match-2rounds.csv",
               stringsAsFactors = FALSE)
names(ra)
ra$X <- NULL

# run the match-menu-item.R file to get join_jaccard table
names(ra)
ra <- ra[, c(1:2, 7)]
colnames(ra)[c(1:3)] <- c("full", "item_name", "match")
table(ra$match)
ra <- ra[!duplicated(paste0(ra$full, ra$item_name)), ]
ra <- merge(ra, join_jaccard, by=c("full", "item_name"))
names(ra)
ra <- ra[, c(1:3, 6)]
ra <- ra[!duplicated(ra$product), ]

# match with tacobell sales data
sales_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
#sapply(detail, class)

for (i in 2007:2015) {
      for (j in 1:4) {
            tryCatch(
                  if(i==2015 & j==4) {stop("file doesn't exist")} else
                  {
                        sales <- read.csv(paste0("data/from-bigpurple/sales-vol-by-product/sales_",
                                                 i, "_Q0", j, ".csv"),
                                          sep = ";", header = FALSE, quote = "\"'",
                                          stringsAsFactors = FALSE,
                                          col.names = c("p_detail", "sales", "qty"))
                        sales <- merge(detail, sales, by="p_detail", all=TRUE)
                        
                        # clean house
                        sales <- sales[!is.na(sales$sales), ]
                        sales <- sales[!grepl("AWR", sales$detail_desc)&!grepl("AW ", sales$detail_desc)&
                                             !grepl("BYB", sales$detail_desc)&!grepl("KFC", sales$detail_desc)&
                                             !grepl("LJS", sales$detail_desc)&!grepl("PH", sales$detail_desc)&
                                             !grepl("PIZZA HUT", sales$detail_desc)&!grepl("TCBY", sales$detail_desc)&
                                             !grepl("ICBIY", sales$detail_desc)&!grepl("KRYSTAL", sales$detail_desc), ]
                        sales <- sales[sales$detail_desc!="CFM MANAGER SPECIALS"&
                                             sales$detail_desc!=""&
                                             !grepl("* NEW PRODCT ADDED BY", sales$detail_desc)&
                                             !grepl("COMBO", sales$detail_desc)&
                                             !grepl("FRANCHISE LOCAL MENU", sales$detail_desc)&
                                             sales$detail_desc!="NEW ITEM"&
                                             !grepl("SPECIAL PROMOTION", sales$detail_desc), ]
                        
                        sales <- merge(sales, ra, by.x = "detail_desc", by.y = "product", all = TRUE)
                        sales <- sales[!is.na(sales$p_detail), ]
                        
                        # keep only valid sales data
                        sales <- sales[!duplicated(sales$p_detail), ]
                        sales <- sales[!is.na(sales$full)&!is.na(sales$qty), ]

                        # fill in summary stats
                        sales <- aggregate(data=sales, qty~full+item_name+match, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)

sales_all <- aggregate(data=sales_all, qty~full+item_name+match, sum)
#sales_all <-merge(sales_all, ra, by="full", all=TRUE) #many test items never had any sales

# put sales in descending order and export
sales_all <- sales_all[order(sales_all$qty, decreasing = TRUE), ]
sales_all <- sales_all[sales_all$qty>=0, ]

# rank sales by %
sales_all$pct <- sales_all$qty / sum(sales_all$qty)*100
colnames(sales_all)[1:2] <- c("tacobell.name", "menustat.name")
write.csv(sales_all, "data/menu-matching/manual-match/items-by-sales-vol-match-type.csv",
          row.names = FALSE)










