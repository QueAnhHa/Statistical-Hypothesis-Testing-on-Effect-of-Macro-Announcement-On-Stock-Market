# Hypothesis Test on Effect of Macro Announcements on Stock Market

This is my final project of one of my finance data analytic class (FINC 489) I had at Rochester Institute of Technology in 2017.
Based on data files containing macro announcements and S&amp;P 500 future prices to address two hypotheses:
1. Macro announcements affect stock markets
2. Certain macro announcements have a greater effect on stock market

# Prequisites
Link to download R: https://www.r-project.org/
Link to download R Studio Free Version: https://www.r-project.org/
2 Exc Files provided in this repo: ES5min.xlsx and Macro Announcements Dr Robin.xls

# Installing packages required for this project
I used 3 packages for this project: readxl, dplyr and broom.
You can install and load those three packages at one by:

```
packages <- c("readxl", "dplyr", "broom")
install.packages(packages)
lapply(packages, library, character.only = TRUE)
```

# Running the Test and Result Explanation

## Section 1: Showing the correlation between announcements and market movement
This experiment will show, based on past market data, that stock price movement is dependent on announcement
results in the 5, 10, and 30 minutes after an announcement is made. To do so, we structure our
experiment hypotheses as follows:
Null hypothesis: Stock price movement has no correlation with macro announcements. 
Alter hypothesis: Stock price movement does have significant correlation with macro annoucements. 

## Data Cleaning
To prep our data, we will first import it via the readxl library, and then clean up any format inconsistencies,
as well as extraneous columns.
```
# Read data from excel
announce <- as.data.frame(read_excel("Macro-Announcements-Dr-Robin.xls"))
ES5min <- as.data.frame(read_excel("ES5min.xlsx"))

# Time format cleaning from string
announce$Time <- substring(announce$Time, 12)
announce$Date_time <- as.POSIXct(sprintf("%s%s", announce$Date, announce$Time), "%Y-%m-%d%H%M%s")

# Deleting redundant columns
ES5min <- subset(ES5min, select = -c(X__1))
announce <- subset(announce, select = -c(Date, Time))
```
## Feature Engineering pt 1: Effects on Price Over Time
As our hypothesis seeks to prove market price movements after an announcement, we will create 3 features in
our data table: each feature will track what the market price is 5, 10, and 30 minutes from now, respectively.
To accomplish that, we use the lead function, provided by the dplyr package.
```
ES5min$After_five_min_close <- lead(ES5min$Close, 1)
ES5min$After_ten_min_close <- lead(ES5min$Close, 2)
ES5min$After_thirty_min_close <- lead(ES5min$Close, 6)
```
## Feature Engineering pt 2: Announcement and Market Signals
In order to gauge the change in market price as well as announcement numbers (i.e. Unemployment), we
need to create features that accurately express the change as a proportion, as follows:

announcement_signal = (announcement_actual - announcement_survey) / announcement_survey
5_min_market_change = (market_price_t5 - market_price_t0) / market_price_t0

Note that in the code, we also use absolute values as modifiers to our announcements and prices. This is due
to some announcements having positive connotations (construction, GDP), while some would have negative
connotations (unemployment). Using absolute values would allow us to treat all announcements as if they
have the same connotation.
```announce$Announcement_signal <- abs((announce$Actual - announce$Survey) / announce$Survey)
ES5min$Five_min_market_signal <- abs((ES5min$After_five_min_close - ES5min$Close) / ES5min$Close)
ES5min$Ten_min_market_signal <- abs((ES5min$After_ten_min_close - ES5min$Close) / ES5min$Close)
ES5min$Thirty_min_market_signal <- abs((ES5min$After_thirty_min_close - ES5min$Close) / ES5min$Close)
```
## Merging it All Together
We will now merge the announce table with the market price table by Date_time, so that we can couple
them together without looking them up in different tables later on. Note that we are doing an inner join, so
that any market prices that does not correspond with an announcement, and vice versa, is discarded. In this
inner join, we do not lose any price information following an announcement, since we have created leading
features for market signals 5, 10, and 30 min on each record.
```
merged <- merge(x=ES5min, y=announce, by="Date_time")
```
We will now also take this opportunity to get rid of any announcement signals that are infinite, due to those
announcements lacking a corresponding survey.
```
merged <- merged[!is.na(merged$Announcement_signal) & !is.infinite(merged$Announcement_signal),]
```
## Final Step: Summaries
```
summary(lm(merged$Announcement_signal ~ merged$Five_min_market_signal))
```
### Output - 5 minute interval 
```
Call: 
lm(formula = merged$Announcement_signal ~ merged$Five_min_market_signal)
Residuals:
Min 1Q Median 3Q Max
-1.023 -0.484 -0.437 -0.140 33.500

Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) 0.46829 0.03784 12.377 <2e-16 ***
merged$Five_min_market_signal 38.00001 17.76180 2.139 0.0325 *
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.605 on 2896 degrees of freedom
Multiple R-squared: 0.001578, Adjusted R-squared: 0.001233
F-statistic: 4.577 on 1 and 2896 DF, p-value: 0.03248
```

A small r-squared value means that not a large portion of the market price signal is determined by the
announcement signal, but the p-value is definitely low enough to show statistical significance. Let’s see if 10
and 30 minutes yield similar results.
```
summary(lm(merged$Announcement_signal ~ merged$Ten_min_market_signal))
```
### Output - 10 minute interval
```
Call:
lm(formula = merged$Announcement_signal ~ merged$Ten_min_market_signal)
Residuals:
Min 1Q Median 3Q Max
-1.422 -0.476 -0.414 -0.123 33.369

Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) 0.42565 0.03873 10.989 < 2e-16 ***
merged$Ten_min_market_signal 60.74793 16.28027 3.731 0.000194 ***
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.603 on 2896 degrees of freedom
Multiple R-squared: 0.004785, Adjusted R-squared: 0.004441
F-statistic: 13.92 on 1 and 2896 DF, p-value: 0.0001941
```
Funny enough, an annoncement’s impact can be seen even more after 10 minutes, as opposed to 5, as seen
by the larger R-squared value, as well as a smaller p-value. Logically, this doesn’t make sense over a longer
period of time, let’s see. . .
```
summary(lm(merged$Announcement_signal ~ merged$Thirty_min_market_signal))
```
### Output - 30 minute interval
```
Call:
lm(formula = merged$Announcement_signal ~ merged$Thirty_min_market_signal)
Residuals:
Min 1Q Median 3Q Max
-1.010 -0.492 -0.444 -0.138 33.467

Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) 0.48308 0.04017 12.027 <2e-16 ***
merged$Thirty_min_market_signal 18.05687 13.86301 1.303 0.193
---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.606 on 2896 degrees of freedom
Multiple R-squared: 0.0005855, Adjusted R-squared: 0.0002404
F-statistic: 1.697 on 1 and 2896 DF, p-value: 0.1928
```
And it is no longer statistically significant, and also has extremely low r-squared values, which passes sanity
tests.
. . . but what if we want to account for different announcement types and their individual effects on market
prices?

## Newwer, More Granular Hypotheses
Whether certain marcro announcements have a greater effect on stock market or not?
We will now split our announcements into their respective announcement types by their code, like so:
```
announcement_types <- unique(announce$Code)
announcement_types
```
### Output
```
[1] "ADP" "BP" "CAB" "CAPU" "CCL" "CCO" "CCR" "CGO"
[9] "COS" "CPI" "CPM" "CSCI" "DF" "DGO" "ECI" "EHS"
[17] "EMP" "FOMC" "FOR" "GDP" "GDPD" "HMI" "HS" "INCL"
[25] "INP" "INV" "IPI" "ISM" "ISMNM" "ISMPP" "LIN" "MBS"
[33] "MEMP" "NHS" "NYM" "PECA" "PECF" "PECP" "PEI" "PES"
[41] "PF" "PHS" "PPI" "RES" "TB" "UMCOF" "UMCOP" "UNE"
```
For each of these announcement types, we will make a hypothesis as follows: Stock price movement does
have significant correlation with this type of macro annoucement. 

## Absolutely Not!
Since we can now select for each type of macro announcement, the justification of using absolute values for
the price and announcement signals no longer hold. Hence, we will re-engineer and remerge these features in
order to get a more accurate assessment of the price and announcement movements.
```
announce$Announcement_signal <- (announce$Actual - announce$Survey) / announce$Survey

ES5min$Five_min_market_signal <- (ES5min$After_five_min_close - ES5min$Close) / ES5min$Close
ES5min$Ten_min_market_signal <- (ES5min$After_ten_min_close - ES5min$Close) / ES5min$Close
ES5min$Thirty_min_market_signal <- (ES5min$After_thirty_min_close - ES5min$Close) / ES5min$Close

merged <- merge(x=ES5min, y=announce, by="Date_time")
merged <- merged[!is.infinite(merged$Announcement_signal),]
```
## Putting it All on the Table
We will now create a table for our final results with respect to price movements after 5 minutes, as creating a
large amount of models is unfeasible.
```
hypothesis_b_table <- data.frame(event_name= character(), linear_coefficient = double(),
                                r_squared= double(), p_value = double(),
                                stringsAsFactors=FALSE)
```
We will now populate the table by looping over our event type list, creating a linear model for each one, and
storing its relevant results. In addition, we will then sort the table based on its r_squared values, in order to
see which announcement type contributes the largest amount of reaction from the market.
```
for (type in announcement_types) {
current_announce <- merged[!is.na(merged$Announcement_signal) &
!is.infinite(merged$Announcement_signal) &
merged$Code == type,]
current_model <- lm(current_announce$Five_min_market_signal ~ current_announce$Announcement_signal)
current_summary <- summary(current_model)
hypothesis_b_table[nrow(hypothesis_b_table) + 1,]$event_name <- type
hypothesis_b_table[nrow(hypothesis_b_table),]$linear_coefficient <- current_summary$coefficients[2]
hypothesis_b_table[nrow(hypothesis_b_table),]$r_squared <- current_summary$adj.r.squared
hypothesis_b_table[nrow(hypothesis_b_table),]$p_value <- glance(current_model)$p.value
}
hypothesis_b_table <- hypothesis_b_table[order(hypothesis_b_table$r_squared, decreasing = TRUE),]
head(hypothesis_b_table)
```
### Output
```
event_name linear_coefficient r_squared p_value
28 ISM 0.0624619119 0.4174268 4.544401e-08
NA ADP 0.0013039245 0.2868906 1.255331e-05
34 NHS 0.0090137989 0.2421148 8.078163e-05
6 CCO 0.0125749116 0.2382468 8.056598e-05
44 RES 0.0005603672 0.2181707 2.498589e-04
24 INCL -0.0165320724 0.1998819 1.455972e-13
```
## Conlucison: Getting Rid of the Insignificant
For our final table, we will get rid of announcement types that have a p value of >0.05, due to a lack of
evidence that they do indeed shift the market.
```
hypothesis_b_table <- hypothesis_b_table[hypothesis_b_table$p_value < 0.05 & !is.na(hypothesis_b_table$p_value),]
head(hypothesis_b_table)
```
### Output
```
event_name linear_coefficient r_squared p_value
28 ISM 0.0624619119 0.4174268 4.544401e-08
NA ADP 0.0013039245 0.2868906 1.255331e-05
34 NHS 0.0090137989 0.2421148 8.078163e-05
6 CCO 0.0125749116 0.2382468 8.056598e-05
44 RES 0.0005603672 0.2181707 2.498589e-04
24 INCL -0.0165320724 0.1998819 1.455972e-13
```
And there you have it! We have our ISM (ISM Manufacturing) contributing to over 40% of the market’s
price changes in the 5 minutes after the announcements, as well as others topping above the 20% range, all
with irrefutable statistical significance.





