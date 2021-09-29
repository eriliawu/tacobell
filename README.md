# Menu Labeling project
Looking at the effect of local menu labeling mandates on calories purchased at Taco Bell restaurants. 
Of the scripts listed in this repository, this is a summary of what each script does.

To understand the use of each .csv file in the data/from-bigpurple folder, refer to each script,
as they are read in from the data/from-bigpurple folder before any analysis was performed.

## Clean restaurant data from ALIGN_DIM table
[clean-restaurant-data.R](https://github.com/eriliawu/tacobell/blob/master/clean-restaurant-data.R)
This script cleans data from ALIGN_DIM table in the database.
The cleaning process is documented in the script itself.
There are also some figures made to show summary statistics, such as number of restaurants by regions and mean spending by region.

## Examine MenuStat data quality
[check-menustat-error.R](https://github.com/eriliawu/tacobell/blob/master/check-menustat-error.R)
This script examines MenuStat data quality.
It applies some bag of words techniques to clean up MenuStat data.
It looks at menu items that were only offered in one year, or the ones offered repeatedly.
It also adds a rudimentary item categorization and look at the calorie information by category.

## Matching Taco Bell menu items to MenuStat
[match-menu-item.R](https://github.com/eriliawu/tacobell/blob/master/match-menu-item.R)
This script matches Taco Bell menu items to MenuStat items.
It applies some cleaning to both sets of data, correct misspelling and fill out abbreviations (this part was manually done by an RA).
It than applies different distance algorithms to measure how similar two text strings are.
I settled on using Jaccard distance with a q=1.
The remainder of the matching was done manually by RAs after I provided them with training.
Refer to the menu-matching-flowchart.R script for a detailed flowchart.
The second half of the script cleans up menu items particularly related to beverages.
Note that even though the DW_PRODUCTGROUP column in the database isn't helpful in determining food categories,
it does identify all the beverages in the "drinks" and "smoothie" category.
The script identifies the volumn of each drink, e.g. small, medium, large, extra large, etc.
It also identifies the type of beverage, such as sugar sweetened fountain beverage, diet, and freezes, etc.
The last part of the script looks like drink sales, by various characteristics.

[inter-rater-reliability.R](https://github.com/eriliawu/tacobell/blob/master/inter-rater-reliability.R)
This script looks at inter-rater reliability of the matching results from the research assistants, as well as the pilots studies through Amazon Mturk.
Both the RAs and the Mturks has multiple rounds of pilot studies. 
Every round of pilot study was compared in this script.

[build-nutrional-table.R](https://github.com/eriliawu/tacobell/blob/master/build-nutrional-table.R)
This script aggregates the matching results submitted by the RAs.
Note that when it comes to drinks specifically, the RAs were told to match on content only, and disregard volume information.
For example, "medium pepsi" would be considered the same as "large pepsi".
This is done deliberately because we were able to identify the size of the beverage based on the product name.
However, several fountain beverages were later found to have incorrect information on total cabohydrates, sugar and sodium (all of which were under-counted).
I have not spent time to investigate which part of this script led to such mistakes, but the nutrition table uploaded to the database is now manually corrected.

[menu-matching-flowchart.R](https://github.com/eriliawu/tacobell/blob/master/menu-matching-flowchart.R)
This script records the menu matching process.
Later users can refer to this chart for future menu matching work.

[calorie-predicting.R](https://github.com/eriliawu/tacobell/blob/master/calorie-predicting.R) and 
[bag-of-words.R](https://github.com/eriliawu/tacobell/blob/master/bag-of-words.R)
These two scripts are experimental scripts where I tried to improve data cleaning for Taco Bell menu items, and predict item calories based on the text.
They are very useful. Users should feel free to disregard entirely.

## Statistical analysis

### Summary Statistics
[calorie-trend.R](https://github.com/eriliawu/tacobell/blob/master/calorie-trend.R)
This script looks at the unadjusted calorie trend in figures.
This is the very first summary statistics I looked at after matching menu items and uploaded the nutrition table to the database.
Transactions were aggregated on HPC to the DW_RESTID-MONTHNO (i.e. restaurant-month) level to mean calories purchased per transaction.

[diff-pricing-analysis.R](https://github.com/eriliawu/tacobell/blob/master/diff-pricing-analysis.R)
This script explores pricing patterns across time and regions.
It examines two of the most popular items: bean burrito and medium Pepsi.
I looked at how the same estaurants price these two items over the years,
as well as how restaurants across different regions price their products in the same year.

### Matching and weighting restaurants
[restaurants-matching.R](https://github.com/eriliawu/tacobell/blob/master/restaurants-matching.R)
This script explores matching treated restaurants to a potential pool of comparison restaurants.
The initial design was to simply do a propensity score matching.
The trickiest part of this process is staggered implementation - different cities and states implemented labeling at different times.
I linked several datasets together: ACS data for community characteristics; restaurant level data to identify treatment and comparison restaurants;
and transaction level data (mean calorie, total number of transactions and mean spending per order, all three of which include both the baseline number at month -3 and the 6-month trend).
A complete list of covarirates used in the matching and weighting process can be found in the covariate balance figure.
I also added treatment time for each treated location.
For the matching, I tried a number of different algorithms, including propensity score, mahalanobis distance, covariate balancing propensity score (CBPS),
optimal matching and subclassification. Overall, propensity score performed the best.
For weighting, I tried Inverse Probability Treatment Weighting (IPTW), CBPS weighting, entrophy balance weighting and optimization-based weighting.
One of the biggest problems with these weighting options is that many comparison restaurants were dropped in the process without a clear explanation.
In the end, it appears a combination of propensity score matching with second-stage IPTW weighting reserved the most restaurants while still producing decent matching results.
Each treated location was matched and weighted locally, and then combined to trim off extremely large weighted comparison restaurants,
every large weighted comparison restaurant was removed from the pool of potential comparisons, and the whole matching and weighting process was re-run,
until no comparison restaurant was weighted more than 5% of the total number of comparison restaurants. This process was done in a while loop.
For the matching and weighting methods I eventually did not use,

[restaurants-matching-drive-thru.R](https://github.com/eriliawu/tacobell/blob/master/restaurants-matching-drive-thru.R)
This script is very similar to the one above, with the only difference being the covariate with regards to the transaction data.
After we decided to use drive-through data as the primary focus, I re-did the matching and weighting using drive-through transactions.
Users will notice the transaction level covariates now include two sets of data: 1) overall characteristics in mean calorie per order, total number of transactions and mean spending per order;
2) these characteristics but limited to drive-through data.
The restaurants used in later analyses are based on the results from this script.

### Aim 1, the effect of labeling on calories
[aim1-diff-in-diff.R](https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff.R)
The initial aim 1 analysis looking at the effect of menu labeling on calories purchased.
I looked at two possible models: 1) treating relative month as a continuous variable, or 2) treating relative month as a factor, to allow for non-parametric results.
The team decided to use factor month.

[aim1-diff-in-diff-match-drive-thru.R](https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff-match-drive-thru.R)
This script is very similar to the one above, except the data changed to include drive-through transactions only.
Note that in the data loading section of the script, users should use the matched-restaurants-trimmed-drive-thru-correct.csv file.

[aim1-diff-in-diff-mealtime-orderType.R](https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff-mealtime-orderType.R)
This script resembles aim1-diff-in-diff.R, but further looks at the results by 1) order type: eat-in, drive-through and takeout;
2) meal time: late night, breakfast, lunch, afternoon, dinner, evening.
Note that this script still uses all transaction data (i.e. not just drive-through data), and as the results from the by order type analysis showes,
the team decided to use drive-through data only for further analysis.

[aim1-diff-in-diff-unmatched.R] (https://github.com/eriliawu/tacobell/blob/master/aim1-diff-in-diff-unmatched.R)
This script mimics the analysis in aim1-diff-in-diff.R, but uses all comparison restaurants.

### Additional analyses
[categorize-food.R](https://github.com/eriliawu/tacobell/blob/master/categorize-food.R)
This script categorizes food items, based on key word searches.
It also performs the same diff-in-diff analyses by categories.

[look-at-only-regular-items.R](https://github.com/eriliawu/tacobell/blob/master/look-at-only-regular-items.R)
This script identifies menu items that were consistently available through all 35 sales quarters in our database.
"Available" is defined as being sold at least once in any quarter.
The later part of the script performs the same diff-in-diff analysis,
as well as a few summary statistics: 1) average number of items per order, by cateogry; 2) % of orders containing items by category.

[missing-macro-nutrients-menustat.R](https://github.com/eriliawu/tacobell/blob/master/missing-macro-nutrients-menustat.R)
This script examines data missingness in nutritional information, besides the calorie information provided by MenuStat.

[top-sales-by-restaurant.R](https://github.com/eriliawu/tacobell/blob/master/top-sales-by-restaurant.R)
This script looks at whether top selling items were systematically different between treated and comparison restaurants.
This was done because in most diff-in-diff analyses, whether overall or by-characteristics,
we could see a persistent sudden decrease in calories between months 12 and 14.
We wanted to see whether treated and comparison restaurants had different popular items during those months.

## For the manuscript
[produce-tables-and-figures.R](https://github.com/eriliawu/tacobell/blob/master/produce-tables-and-figures.R)
This script automated the process of producing tables and figures for the manuscript.
A few figures were produced in a different script, and the script names were noted to help users find them.

