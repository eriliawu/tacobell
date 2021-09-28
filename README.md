# Menu Labeling project
Looking at the effect of local menu labeling mandates on calories purchased at Taco Bell restaurants. 
Of the scripts listed in this repository, this is a summary of what each script does.

## Clean restaurant data from ALIGN_DIM table
clean-restaurant-data.R

## Examine MenuStat data quality
check-menustat-error.R

## Matching Taco Bell menu items to MenuStat
match-menu-item.R
bag-of-words.R
inter-rater-reliability.R
build-nutrional-table.R
calorie-predicting.R
menu-matching-flowchart.R

## Statistical analysis

### Summary Statistics
calorie-trend.R
diff-pricing-analysis.R

### Matching and weighting restaurants
restaurants-matching.R
restaurants-matching-drive-thru.R

### Aim 1, the effect of labeling on calories
[aim1-diff-in-diff.R](https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff.R) The initial aim 1 analysis looking at the effect of menu labeling on calories purchased. I looked at two possible models: 1) treating relative month as a continuous variable, or 2) treating relative month as a factor, to allow for non-parametric results. The team decided to use factor month.
[aim1-diff-in-diff-match-drive-thru.R](https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff-match-drive-thru.R) This script is very similar to the one above, except the data changed to include drive-through transactions only. Note that in the data loading section of the script, users should use the matched-restaurants-trimmed-drive-thru-correct.csv file.
aim1-diff-in-diff-mealtime-orderType.R
aim1-diff-in-diff-unmatched.R

### Additional analyses
categorize-food.R
look-at-only-regular-items.R
missing-macro-nutrients-menustat.R
top-sales-by-restaurant.R

## For the manuscript
produce-tables-and-figures.R

