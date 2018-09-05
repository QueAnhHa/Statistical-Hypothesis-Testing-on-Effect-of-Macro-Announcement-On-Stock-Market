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

# Running the test and result Explanation

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

