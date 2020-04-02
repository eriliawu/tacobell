### assess human raters
#install.packages("irr")

getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### load libraries ----
library(irr)

### compare turk inter-rater reliability ----
turk <- read.csv("data/menu-matching/manual-match/mturk/pilot/pilot200-results-simple.csv",
                 stringsAsFactors = FALSE)
names(turk)
#turk$case <- NULL
turk$worker <- rep(c(1:2), 200)
turk <- reshape(turk, direction="wide", idvar = c("task", "case"), timevar = "worker")
names(turk)

# re-contruct character answers to mueric
turk$answer.1 <- as.numeric(substr(turk$answer.1, 1, 1))
turk$answer.2 <- as.numeric(substr(turk$answer.2, 1, 1))
table(turk$answer.1, turk$answer.2)

## if we consider answer.1 and answer.2 were rated by the same set of raters
# unweighted kappa
kappa2(turk[1:98, 3:4], "unweighted") #0.53
kappa2(turk[, 3:4], "unweighted") #0.70 

# sqaured and linear weights
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
write.csv(turk2[turk2$answer.1==turk2$answer.2&turk2$answer.1==turk2$answer.3, ],
          "data/menu-matching/manual-match/mturk/pilot/full-agreement_pilot2.csv",
          row.names = FALSE)

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
ra$match <- ifelse(ra$round1==1&ra$round2==1, 1,
                  ifelse(ra$round1==0&ra$round2==0, 0, 2))
table(ra$match)

# run the match-menu-item.R file to get join_jaccard table
names(ra)
ra <- ra[, c(1:2, 7)]
colnames(ra)[1:2] <- c("full", "item_name")
ra <- ra[!duplicated(paste0(ra$full, ra$item_name)), ]
ra <- merge(ra, join_jaccard, by=c("full", "item_name"))
ra <- ra[ra$match!=0, c(3, 6)]

# match with tacobell sales data
sales_all <- NULL
detail <- read.csv("data/from-bigpurple/product_detail.csv",
                   stringsAsFactors = FALSE)
sapply(detail, class)

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
                        
                        # delete duplicated rows
                        sales <- sales[!duplicated(sales$detail_desc), ]
                        sales$match[is.na(sales$match)] <- 0
                        
                        # fill in summary stats
                        sales <- aggregate(data=sales, qty~match, sum)
                        sales$year <- i
                        sales$quarter <- j
                        sales$pct <- sales$qty / sum(sales$qty)
                        sales_all <- rbind(sales_all, sales)
                  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
      }
}
rm(i, j, detail, sales)

# visualization
ggplot(data=sales_all, aes(x=paste0(year, "Q",quarter), y=pct,
                           group=as.factor(sales_all$match),
                           color=as.factor(sales_all$match))) +
      geom_point() +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
      scale_color_manual(name="RA agreement",
                         values=c("red", "green", "blue"),
                         labels=c("Not a match", "Match", "Maybe")) +
      labs(title="Percent of sales, by RA agreement",
           x="Time", y="Percent",
           caption="Note: 626 items - both agree on a match; 360 items - one RA rated a match.") +
      theme(plot.title=element_text(hjust=0.5, size=18),
            plot.caption=element_text(hjust=0, face="italic"),
            axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("tables/product-matching/sales-vol-represented-by-matched-items-ra.jpeg", width=20, height=10, unit="cm")


