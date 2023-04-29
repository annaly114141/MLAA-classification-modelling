library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)

# Set Working directory
setwd("/Users/anna/Desktop/AT1A\ MLA")

# Load 'Transactions' file
tr_monthly <- read.csv(file = "transactions.csv")

# CLEAN AND ADD VARIABLES AS NEEDED
# 1. a) Do you need to clean the data in any way? Justify what you decide to do (or not do).
# Look at structure of the file
str(tr_monthly)

# Date was imported as a factor - change to date to match data dictionary
tr_monthly$date <- as.Date(tr_monthly$date,
                           format = "%d/%m/%Y")

# Check for any transaction with zero values
sum(tr_monthly$monthly_amount==0)
# [1] 1

tr_monthly<-filter(tr_monthly, monthly_amount!=0 )

# Create Month Number variable
tr_monthly = tr_monthly %>%
  mutate(month_number = format(as.Date(date), "%m"))
tr_monthly$month_number = as.integer(tr_monthly$month_number)

# Create Year variable           
tr_monthly$Year <- tr_monthly$date 
tr_monthly$Year <- format(tr_monthly$Year,"%Y")
tr_monthly$Year = as.integer(tr_monthly$Year)
str(tr_monthly)


#######Exploratory Data Analysis (EDA)

tr_monthly %>% count(location)

hist_count_loc <- ggplot(data = tr_monthly) +geom_histogram(mapping = aes(x = location), binwidth = 0.5) + scale_x_continuous(breaks = c(1:10)) + labs (title = "Count of Transactions by Location")

tr_monthly %>% count(industry)

hist_count_ind <- ggplot(data = tr_monthly) +geom_histogram(mapping = aes(x = industry), binwidth = 0.5) + scale_x_continuous(breaks = c(1:10)) + labs (title = "Count of Transactions by Industry")

# Arrange the histograms side by side [PLOT A]
plot_A <- grid.arrange(hist_count_loc, hist_count_ind, nrow=1)

#Most common frequency are Locations 1 & 2
#Lowest frequency is Location 8
#Plot Total transactions per month by industry [PLOT B]

plot_B <- ggplot(data = tr_monthly, aes(x=date, y=monthly_amount)) +
  geom_point(aes(color=industry)) +
  scale_y_continuous(labels = scales::comma) +
  labs (title = "Total Transactions per Month - per industry",
        x = "Date", y = "Monthly amount") 
plot_B

#Repeat for Location
plot_C <- ggplot(data = tr_monthly, aes(x=date, y=monthly_amount)) +
  geom_point(aes(color=location)) +
  scale_y_continuous(labels = scales::comma) +
  labs (title = "Total Transactions per Month - per location",
        x = "Date", y = "Monthly amount") 
plot_C

grid.arrange(plot_B, plot_C, nrow=1)

#Look at count of entries by month [PLOT D]
plot_D <- ggplot(data = tr_monthly) +
  geom_bar(mapping = aes(x = month_number)) + scale_x_discrete(limits = month.name) + labs (title = "Total Transactions per Month", x = "Month", y = "Count Transactions")
plot_D

#Drop off in December. Why? Incomplete year / month of transaction data? Given there are multiple years of data in there, is it a seasonal trend for December? Office/service closed during that period?
#There is a positive trend in # entries from Jan - Nov
#Bar Chart to see the monthly for each month in three year [PLOT E]

tr_monthly %>%
  ggplot(aes(x = month_number, y = monthly_amount)) +
  geom_bar(stat = "identity", fill = "blue") +
  facet_wrap(~ Year) +
  labs(title = "Monthly transaction amount - By year",
       y = "Monthly Transaction Amount",
       x = "Month") + scale_x_discrete(limits=c(1:12))
plot_E



#This has answered the questions raised above - there is an incomplete year of data for 2016 - with December's transactions missing

#Look at the mean
tr_mean_TF <- tr_monthly %>%
  mutate(monthly_above_mean = ifelse(monthly_amount > mean(monthly_amount), TRUE, FALSE))

plot_F <- ggplot(data = tr_mean_TF, aes(x = date, y = monthly_amount, fill = monthly_above_mean)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_discrete(name = "Above mean T/F") +
  labs (title = "Count transactions by month above and below mean",
        x = "Month", y = "Amount")
plot_F

#Look at Monthly Transactions, by Month - facet wrapped by Industry

plot_1 <-  ggplot (tr_monthly, aes(x = month_number, y = monthly_amount)) + geom_point () + facet_wrap (~industry)+ labs(title="Monthly Total Amount: By Industry", x="Month", y="Mean Monthly Amount") 

plot_1

#High variance in monthly transactions in industry 6 and industry 10
#Industry 6 has lowest number of monthly transactions, but higest variance & highest monthly amount (high value infrequent purchases?)

#Switch Facet so now faceted by location

plot_2 <- ggplot (tr_monthly, aes(x = month_number, y = monthly_amount)) + geom_point () + 
  facet_wrap (~location)  + labs(title="Monthly Total Amount: By Location", x="Month", y="Mean Monthly Amount") + scale_x_discrete(limits=c(1:12))

plot_2

# Location 1 has the highest variance in monthly amount, and the second highest volume of transactions 
# Location 8 has some grouped monthly income (possibly indicating 2 quite different price points / products?

#2. a) iCreate an aggregated data set using the fields date, industry and location, with a mean of monthly_amount.
#Create function to aggregate by date, industry, location & mean monthly amount, and add our desired new columns to the df output

agg.func <- function(df) {
  
  output <- df %>% 
    group_by(date, industry, location) %>% 
    summarise(mean_monthly_amount = mean(monthly_amount, na.rm = TRUE))
  
  # Create Month Number Variable
  output = output %>%
    mutate(month_number = format(as.Date(date), "%m"))
  output$month_number = as.integer(output$month_number)
  
  # Create Year variable           
  output$Year <- output$date 
  output$Year <- format(output$Year,"%Y")
  output$Year = as.integer(output$Year)
  
  return(output)
}

agg_data <- agg.func(tr_monthly)

#2. a) ii Create a line plot of the variable monthly_amount for industry 1 and location 1. Note the seasonality by month in this time series.

agg_1_1 <- agg_data %>% 
  filter(industry == 1, location == 1)
# Total 47 observations in filtered dataset

# Create Time Number Variable
agg_1_1$time_number = c(1:nrow(arrange(agg_1_1, date)))

plot_G <-  ggplot(data = agg_1_1, aes(x = date, y = agg_1_1$mean_monthly_amount, group=1)) + 
  geom_line(color="blue") + geom_point(color = 'magenta') + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Monthly Amount: Industry 1 & Location 1", x="Date", y="Mean Monthly Amount")
plot_G

#Dec was a bit of an anomaly

#2. a) iii For industry = 1 and location = 1, train a linear regression model with monthly_amount as the target.
#As random is not appropriate I won't use set.seed, let's look at the numbers for a 30/70 test train split on this small dataset

# Partition data into test train 70/30
trainset_indices <- createDataPartition(y = agg_1_1$mean_monthly_amount, p = .70, list = FALSE)

# Assign observations to training and testing sets
trainset <- agg_1_1[trainset_indices, ]
testset <- agg_1_1[-trainset_indices, ]

# Rowcounts to check
trainset_size <- nrow(trainset)
testset_size <- nrow(testset)
nrow(agg_1_1)

#Now to run a linear model with mean_monthly_amount as the target variable
#Given that in this subset all items are industry 1 and location 1, they will not be used as predictor variables.To have something to compare to however, I am going to run the first model with all variables included. For the second, I am going to use time_number which we added, as the predictor variable.

# Model 1
all_var_pred.lm <- lm(mean_monthly_amount~., data=trainset)
summary(all_var_pred.lm)
# all_var_pred.lm has Adjusted R-squared:0.5542 , p-value: 0.511
par(mfrow = c(2, 2))
plot(all_var_pred.lm)

# Model 2
time_num_pred.lm <- lm(mean_monthly_amount~time_number, data=trainset)
summary(time_num_pred.lm)
plot(time_num_pred.lm)

# time_num_pred.lm has Adjusted R-squared:  0.5377, p-value: 5.432e-07
# time_num_pred.lm has a higher Adjusted R-squared and a lower p-value so therefore looks to be the better model used on our training data. 

# PLot Model 2
plot_H <- plot(time_num_pred.lm, which = 1)
plot_I <- plot(time_num_pred.lm, which = 2)
plot_J <- plot(time_num_pred.lm, which = 3)
plot_K <- plot(time_num_pred.lm, which = 5)
par(mfrow = c(2, 2))
## 2. a) iv
# Create a prediction for monthly_amount in December 2016. Comment on how reasonable this prediction is. For example, if you were to plot it on the same plot as 2aii, would it sit somewhere reasonable?

# Create a data frame for December 2016
dec_2016 = data.frame(date = "2016/12/01",
                      industry=1,
                      location=1,
                      mean_monthly_amount=0,
                      month_number=12,
                      Year=2016,
                      time_number=(nrow(agg_1_1)+1))

dec_2016$date <- as.Date(dec_2016$date,
                         format = "%Y/%m/%d")
dec_2016$industry <- as.integer(dec_2016$industry)
dec_2016$location <- as.integer(dec_2016$location)
dec_2016$time_number <- as.integer(dec_2016$time_number)

# Use predict function + model we just built and apply to dec_2016 dataframe
dec_2016$mean_monthly_amount <- predict(time_num_pred.lm,dec_2016)

# To add this to the existing dataset (agg_1_1) we use rbind
agg_1_1 <- as.data.frame(agg_1_1)
agg_data_with_dec_pred <- rbind(agg_1_1, dec_2016)

#Plot to check the prediction is reasonable as compared to other monthly mean amounts
plot_L <-  ggplot(data = agg_data_with_dec_pred, aes(x = date, y =mean_monthly_amount, group=1)) + 
  geom_line(color="blue") + geom_point(color = 'magenta') + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Monthly Amount: Industry 1 & Location 1", subtitle =   "December 2016 Prediction Included", x="Date", y="Mean Monthly Amount")
plot_L

#The prediction appears to follow the seasonal trend of the last 4 years (2017 & 2020) showing a downward trend heading into Q1 / January.

#3.a) & 3b Apply the modelling process you built for industry 1 and location 1 to all industries and locations programmatically.
#b) Calculate your evaluation measure for the training data and your testing data, for all models. Identify the two industries and two locations for which your method performs worst.
#i. Ensure your models all make a prediction for December 2016.
#Create a function for running all industries and locations with December 2016 included

calculate_predictions <- function(df, industries, locations) {
  
  output = data.frame()
  for (ind in industries) {
    for (loc in locations) {
      
      temp = df[df$industry == ind & df$location == loc, ]
      
      if (length(unique(temp$date)) >= 36) {
        
        arrange(temp, date)
        
        # Add a number to represent date order
        temp$time_number = c(1:nrow(temp))
        
        # Split test train 70/30
        trainset_indices_all <- createDataPartition(y = df$mean_monthly_amount, p = .70, list = FALSE)
        
        # Assign observations to training and testing sets
        trainset_all <- df[trainset_indices_all, ]
        testset_all <- df[-trainset_indices_all, ]
        
        # Rowcounts to check
        trainset_size_all <- nrow(trainset_all)
        testset_size_all <- nrow(testset_all)
        nrow(df)
        
        # Now to run the earlier linear model with mean_monthly_amount as the target variable
        model_all <- lm(mean_monthly_amount~month_number, data=trainset_all)
        summary(model_all)
        
        # Output prediction based on the dataSet
        training.prediction = predict(model_all, trainset_all)
        testing.prediction = predict(model_all, testset_all)
        
        trainset_all$prediction = training.prediction
        testset_all$prediction = testing.prediction
        
        trainset_all$error = with(trainset_all, prediction-mean_monthly_amount)
        testset_all$error = with(testset_all, prediction-mean_monthly_amount)
        
        trainset_all_rmse = with(trainset_all, sqrt(mean(error^2)))
        testset_all_rmse = with(testset_all, sqrt(mean(error^2)))
        
        # Create a dataframe containing just the December 2016 data
        december_2016 = data.frame(date = "2016-12-01",
                                   industry = ind,
                                   location = loc,
                                   mean_monthly_amount = 0,
                                   month_number = 12,
                                   Year = 2016,
                                   time_number = (nrow(temp)+1))
        
        # Ensure temp is of type data frame
        temp = as.data.frame(temp)
        
        # Add the December 2016 row
        temp = rbind(temp, december_2016)
        
        # Output a prediction based on all rows and add it to the temp data frame
        temp$prediction = predict(model_all, temp)
        
        # Get the last prediction value (which is the Dec 2016 value).
        train_dec_2016_prediction = tail(temp$prediction, 1)
        
        tempOutput = c(ind, loc, trainset_all_rmse, testset_all_rmse, train_dec_2016_prediction)
        
      } else {
        
        tempOutput = c(ind, loc, NA, NA, NA)
      }
      
      output = rbind(output, tempOutput)
    }
  }
  
  colnames(output) <- c("Industry", "Location", "Training RMSE", "Testing RMSE", "December 2016 Prediction")
  
  #Return the output
  return(output)
}

# Using the aggregated dataset from part 2 (agg_data)
# Get the list of unique industries, sorted in numerical order 
industries <- sort(unique(agg_data$industry))

# Get the list of unique locations, sorted in numerical order
locations <- sort(unique(agg_data$location))

# Calculate the predictions for all industry and location combinations
all_pred <- calculate_predictions(agg_data, industries, locations)
View(all_pred)

write.csv(all_pred,"/Users/anna/Desktop/all_pred.csv", row.names = FALSE)


model_all <- lm(mean_monthly_amount~month_number, data=trainset)
summary(model_all)
plot(model_all)


# 3.c)
### What might be causing the models on these two industries and locations to be performing poorly (HINT: Some plots may help here.)? How might you fix this in future?

plot_O <- plot(model_all, which = 1)
plot_P <- plot(model_all, which = 2)
plot_Q <- plot(model_all, which = 3)
plot_R <- plot(model_all, which = 5)









