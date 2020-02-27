### assess human raters
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

# RAs reliability ----
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

