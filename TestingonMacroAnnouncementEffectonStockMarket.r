
# Title: "Project 489B"
# Author: "Anh Ha - aqh9586@rit.edu"
# Date: 12/9/2017

# DATA CLEANING

# Install and Load Required Packages
packages <- c("readxl", "dplyr", "broom")
install.packages(packages)
lapply(packages, library, character.only = TRUE)

# Read data from excel
announce <- as.data.frame(read_excel("Macro Announcements Dr  Robin.xls"))
ES5min   <- as.data.frame(read_excel("ES5min.xlsx"))

# Time format cleaning from string
announce$Time      <- substring(announce$Time, 12)
announce$Date_time <- as.POSIXct(sprintf("%s%s", announce$Date, announce$Time), "%Y-%m-%d%H%M%s")

# Deleting redundant columns
ES5min   <- subset(ES5min,   select = -c(X__1))
announce <- subset(announce, select = -c(Date, Time))

# Hypothesis Test 1 : Macro Announcements Affect Stock Market ???

# PART 1: Effects on Price Over Time
ES5min$After_five_min_close   <- lead(ES5min$Close, 1)
ES5min$After_ten_min_close    <- lead(ES5min$Close, 2)
ES5min$After_thirty_min_close <- lead(ES5min$Close, 6)

# Part 2: Announcement and Market Signals

announce$Announcement_signal     <- abs((announce$Actual - announce$Survey) / announce$Survey)

ES5min$Five_min_market_signal    <- abs((ES5min$After_five_min_close   - ES5min$Close) / ES5min$Close)
ES5min$Ten_min_market_signal     <- abs((ES5min$After_ten_min_close    - ES5min$Close) / ES5min$Close)
ES5min$Thirty_min_market_signal  <- abs((ES5min$After_thirty_min_close - ES5min$Close) / ES5min$Close)

# Merging it All Together
merged <- merge(x=ES5min, y=announce, by="Date_time")
merged <- merged[!is.na(merged$Announcement_signal) & !is.infinite(merged$Announcement_signal),]

# Summaries
summary(lm(merged$Announcement_signal ~ merged$Five_min_market_signal))
summary(lm(merged$Announcement_signal ~ merged$Ten_min_market_signal))
summary(lm(merged$Announcement_signal ~ merged$Thirty_min_market_signal))

# Hypothesis Test 2: Certain Macro Announcements have a greater effect on stock market ???

# Break announcements into different types
announcement_types <- unique(announce$Code)
announcement_types

announce$Announcement_signal     <- (announce$Actual - announce$Survey) / announce$Survey

ES5min$Five_min_market_signal    <- (ES5min$After_five_min_close   - ES5min$Close) / ES5min$Close
ES5min$Ten_min_market_signal     <- (ES5min$After_ten_min_close    - ES5min$Close) / ES5min$Close
ES5min$Thirty_min_market_signal  <- (ES5min$After_thirty_min_close - ES5min$Close) / ES5min$Close

merged <- merge(x=ES5min, y=announce, by="Date_time")
merged <- merged[!is.infinite(merged$Announcement_signal),]

# Put it all together
# Create a table for final result
hypothesis_b_table <- data.frame(event_name= character(), linear_coefficient = double(), 
                                 r_squared= double(), p_value = double(),
                                 stringsAsFactors=FALSE)

# Create linear model for each announcement type

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

# Finally, get rid of types that have p values > 0.05
hypothesis_b_table <- hypothesis_b_table[hypothesis_b_table$p_value < 0.05 & !is.na(hypothesis_b_table$p_value),]
head(hypothesis_b_table)


