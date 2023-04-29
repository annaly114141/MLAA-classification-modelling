# Set Working directory
setwd("/Users/anna/Desktop/AT1B\ MLA")

repurchase_training <- read.csv("repurchase_training.csv")
repurchase_validation <- read.csv("repurchase_validation.csv")

library(reshape)
library(ggplot2)
library(tidyverse)
library(ROCR)
library(vip)
library(dplyr)
library(purrr)
library(randomForestSRC)
library(pdp)

### EDA

#need to change ID to be "character" in both datasets, so the IDs are not counted as numbers

eda_data <- repurchase_training

for (i in 3:6) {
  eda_data[,i] = as.character(eda_data[,i]);
}

str(eda_data)

#Find nulls in the data to work out which columns have a lot of empty records
total_nulls <- data.frame(type = names(eda_data[, -1]), 
                          nulls = colSums(eda_data[, -1] == "NULL"),
                          values = colSums(eda_data[, -1] != "NULL")
)

total_nulls <- melt(total_nulls, id.var="type")

# We can see from this that 'age_band' has very few values across the training set
# We can also see that gender only has value in about 50% of rows
# Neither is likely very usable for our model
ggplot(total_nulls, aes(x = type, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip()

#First write a function that will take a characteristic and return a data frame that gives us the count of entries with target = 1 and target = 0
get_counts <- function (attribute_df, count_df) {
  
  output = data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
  attribute_name = colnames(attribute_df)[1]
  names(output) = c(attribute_name, "target", "non_target")
  
  for(i in 1:nrow(attribute_df)) {
    my_attribute = as.character(attribute_df[i, attribute_name])
    
    my_target_count = count_df %>%
      filter(Target == 1) %>%
      filter(.[,2] == my_attribute)
    my_no_target_count = count_df %>%
      filter(Target == 0) %>%
      filter(.[,2] == my_attribute)
    
    if(nrow(my_target_count) > 0) {
      target_count = as.character(my_target_count$count)
    } else {
      target_count = "0"
    }
    
    if(nrow(my_no_target_count) > 0) {
      no_target_count = as.character(my_no_target_count$count)
    } else {
      no_target_count = "0"
    }
    
    temp <- data.frame(my_attribute, target_count, no_target_count)
    names(temp) <- c(attribute_name, "target", "non_target")
    
    output = rbind(output, temp)
    output[,1] <- as.character(output[,1])
    output[,2] <- as.integer(output[,2])
    output[,3] <- as.integer(output[,3])
    
  }
  
  return(output)
}
#melt and plot car models
car_models <- eda_data %>%
  group_by(car_model) %>%
  summarize(count = n())

car_models_counts <- eda_data %>%
  group_by(Target, car_model) %>%
  summarize(count = n())

car_model_data <- get_counts(car_models, car_models_counts)

car_model_data <- melt(car_model_data, id.var="car_model")

ggplot(car_model_data, aes(x = car_model, y = value, fill = variable)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#plot car segments variable
car_segments <- eda_data %>%
  group_by(car_segment) %>%
  summarize(count = n())

car_segments_counts <- eda_data %>%
  group_by(Target, car_segment) %>%
  summarize(count = n())

car_segment_data <- get_counts(car_segments, car_segments_counts)

car_segment_data <- melt(car_segment_data, id.var="car_segment")

ggplot(car_segment_data, aes(x = car_segment, y = value, fill = variable)) +
  geom_bar(stat = "identity")

#plot age of vehicle in years variable
age_of_vehicle_years <- eda_data %>%
  group_by(age_of_vehicle_years) %>%
  summarize(count = n())

age_of_vehicle_years_counts <- eda_data %>%
  group_by(Target, age_of_vehicle_years) %>%
  summarize(count = n())

age_of_vehicle_years_data <- get_counts(age_of_vehicle_years, age_of_vehicle_years_counts)

age_of_vehicle_years_data <- melt(age_of_vehicle_years_data, id.var="age_of_vehicle_years")

ggplot(age_of_vehicle_years_data, aes(x = age_of_vehicle_years, y = value, fill = variable)) +
  geom_bar(stat = "identity")


#Look at some service history characteristics of the cars to see if we can detect any patterns
#NUmber of scheduled service used under warranty
sched_serv_warr_values <- eda_data %>%
  group_by(sched_serv_warr) %>%
  summarize(count = n())

sched_serv_warr_counts <- eda_data %>%
  group_by(Target, sched_serv_warr) %>%
  summarize(count = n())

sched_serv_warr_data <- get_counts(sched_serv_warr_values, sched_serv_warr_counts)

sched_serv_warr_data <- melt(sched_serv_warr_data, id.var="sched_serv_warr")

ggplot(sched_serv_warr_data, aes(x = sched_serv_warr, y = value, fill = variable)) +
  geom_bar(stat = "identity")


#Number of services had at the same dealer where the vehicle was purchased
num_serv_dealer_purchased_values <- eda_data %>%
  group_by(num_serv_dealer_purchased) %>%
  summarize(count = n())

num_serv_dealer_purchased_counts <- eda_data %>%
  group_by(Target, num_serv_dealer_purchased) %>%
  summarize(count = n())

num_serv_dealer_purchased_data <- get_counts(num_serv_dealer_purchased_values, num_serv_dealer_purchased_counts)

num_serv_dealer_purchased_data <- melt(num_serv_dealer_purchased_data, id.var="num_serv_dealer_purchased")

ggplot(num_serv_dealer_purchased_data, aes(x = num_serv_dealer_purchased, y = value, fill = variable)) +
  geom_bar(stat = "identity")

lrm_data <- repurchase_training

#We run a linear regression model on our testset, excluding IDs
glm.model = glm(formula = Target ~ ., family = binomial, data = lrm_data[,-1])


lrm_data$age_band <- substr(lrm_data$age_band, 1, 1)
lrm_data$age_band <- gsub("N", "NULL", lrm_data$age_band)

lrm_data$gender <- gsub("Male", "1", lrm_data$gender)
lrm_data$gender <- gsub("Female", "2", lrm_data$gender)

lrm_data$car_model <- sub('model_', '', lrm_data$car_model)

lrm_data$car_segment <- sub("Other", "1", lrm_data$car_segment)
lrm_data$car_segment <- sub("Small/Medium", "2", lrm_data$car_segment)
lrm_data$car_segment <- sub("Large/SUV", "3", lrm_data$car_segment)
lrm_data$car_segment <- sub("LCV", "4", lrm_data$car_segment)

lrm_data$age_band <- as.integer(lrm_data$age_band)

lrm_data$car_model <- as.integer(lrm_data$car_model)
lrm_data$car_segment <- as.integer(lrm_data$car_segment)

#We want to keep our Target as a Factor
lrm_data[,2] = as.factor(lrm_data[,2]);

lrm_data <- lrm_data %>%
  dplyr::select(ID, Target, car_model, car_segment, age_of_vehicle_years, sched_serv_warr, non_sched_serv_warr, sched_serv_paid, non_sched_serv_paid, total_paid_services, total_services, mth_since_last_serv, annualised_mileage, num_dealers_visited, num_serv_dealer_purchased)

#Create training and test sets of our lrm_data

## 70% of the lrm_data, use floor to round down to nearest integer
lrm_trainset_size <- floor(0.70 * nrow(lrm_data))

set.seed(42)

lrm_trainset_indices <- sample(seq_len(nrow(lrm_data)), size = lrm_trainset_size)

lrm_trainset <- lrm_data[lrm_trainset_indices, ]
lrm_testset <- lrm_data[-lrm_trainset_indices, ]
#So try our linear regression model on our newly cleaned test set
glm.model = glm(formula = Target ~ ., family = binomial, data = lrm_testset[,-1])

summary(glm.model)

# Now to predict probabilities on lrm_testset
glm_prob <- predict.glm(glm.model,lrm_testset[,-1],type="response")

# Create a vector to hold predictions
glm_predict <- rep(0,nrow(lrm_testset[,-1]))
glm_predict[glm_prob>.5] <- 1

#Create a confusion matrix
glm_confusion_matrix <- table(pred=glm_predict,true=lrm_testset$Target)

glm_confusion_matrix

# We'll want to look at evaluation measures regularly, so crteate a function to calculate and return them
get_evaluation_measures <- function(name = NA, tn, fp, fn, tp) {
  
  accuracy = (tp+tn)/(tp+tn+fp+fn)
  
  precision = tp/(tp+fp)
  
  recall = tp/(tp+fn)
  
  F1 = 2 * ((precision * recall)/(precision + recall))
  
  output = data.frame(name, accuracy, precision, recall, F1)
  
  return(output)
  
}

#Convert the confusion matrix to a data frame and output the evaluation measures
glm_confusion_matrix_df <- as.data.frame(glm_confusion_matrix)

lrm_evaluation_measures <- get_evaluation_measures("Linear Regression",
                                                   glm_confusion_matrix_df$Freq[1],
                                                   glm_confusion_matrix_df$Freq[2],
                                                   glm_confusion_matrix_df$Freq[3],
                                                   glm_confusion_matrix_df$Freq[4])

lrm_evaluation_measures

#Now to get AUC. We'll do it again further on in our analysis, so write a function
get_auc <- function(probabilities, targets) {
  
  probs = as.vector(probabilities)
  
  pred = prediction(probs,targets)
  
  perf_AUC = performance(pred, "auc")
  
  AUC = perf_AUC@y.values[[1]]
  
  return(AUC)
  
}

#Get the AUC and add it to our evaluation measures data frame
lrm_evaluation_measures$AUC <- get_auc(glm_prob, lrm_testset$Target)

#Create a data frame for all evaluation measures and use lrm_evaluation_measures as the first row
evaluation_measures <- lrm_evaluation_measures

evaluation_measures

#Now we want to try LASSO to perform grid search to find optimal value of lambda IE regularize the model

#convert training data to matrix format
lrm_x <- model.matrix(Target~.,lrm_trainset[,-1])
lrm_y <- lrm_trainset$Target

#family= binomial => logistic regression, alpha=1 => lasso
library(glmnet)
lrm_cv.out <- cv.glmnet(lrm_x, lrm_y, alpha=1, family="binomial", type.measure="auc")

plot(lrm_cv.out)

#Using lambda of 1se rather than the minimum lambda, see what predictors are discarded

#min value of lambda
lrm_lambda_min <- lrm_cv.out$lambda.min
#best value of lambda
lrm_lambda_1se <- lrm_cv.out$lambda.1se

#regression coefficients
coef(lrm_cv.out,s=lrm_lambda_1se)

#Convert test data to a model matrix
lrm_x_test <- model.matrix(Target~.,lrm_testset[,-1])

#Get prediction probabilities
lasso_prob <- predict(lrm_cv.out, newx = lrm_x_test, s=lrm_lambda_1se, type="response")

#translate probabilities to predictions
lasso_predict <- rep(0,nrow(lrm_testset))

lasso_predict[lasso_prob>.5] <- 1

#confusion matrix
lasso_confusion_matrix <- table(pred=lasso_predict,true=lrm_testset$Target)

lasso_confusion_matrix

#Convert the confusion matrix to a data frame
lasso_confusion_matrix_df <- as.data.frame(lasso_confusion_matrix)

lasso_evaluation_measures <- get_evaluation_measures("Lasso",
                                                     lasso_confusion_matrix_df$Freq[1],
                                                     lasso_confusion_matrix_df$Freq[2],
                                                     lasso_confusion_matrix_df$Freq[3],
                                                     lasso_confusion_matrix_df$Freq[4])

#Get the AUC and add it to our evaluation measures data frame
lasso_evaluation_measures$AUC <- get_auc(lasso_prob, lrm_testset$Target)

lasso_evaluation_measures

#Visually compare the two models' evaluation metrics by placing in a data frame

evaluation_measures <- rbind(evaluation_measures, lasso_evaluation_measures)

evaluation_measures

##Tree based classification

# Remove columns with high NAs - excluding age and gender - and ID
tree_data <- repurchase_training %>%
  dplyr::select(Target, car_model, car_segment, age_of_vehicle_years, sched_serv_warr, non_sched_serv_warr, sched_serv_paid, non_sched_serv_paid, total_paid_services, total_services, mth_since_last_serv, annualised_mileage,num_dealers_visited,num_serv_dealer_purchased)

# Make sure our predictor is a factor
tree_data$Target <- as.factor(tree_data$Target)

#Get index of predicted variable
targetColNum <- grep("Target",names(tree_data))

# Create training and test sets

## 70% of the sample size, use floor to round down to nearest integer
tree_trainset_size <- floor(0.70 * nrow(tree_data))

#set random seed 
set.seed(42)

tree_trainset_indices <- sample(seq_len(nrow(tree_data)), size = tree_trainset_size)
#assign observations to training and testing sets

tree_trainset <- tree_data[tree_trainset_indices, ]
tree_testset <- tree_data[-tree_trainset_indices, ]

#This is a classification problem so set method="class" and exclude ID
library(rpart)
rpart_fit <- rpart(Target~.,data = tree_trainset, method='class', model=TRUE)

#predict on test data
rpart_prob <- predict(rpart_fit,tree_testset[,-targetColNum],type="class")

#Create a confusion matrix
rpart_confusion_matrix <- table(pred=rpart_prob,true=tree_testset[,targetColNum])

rpart_confusion_matrix

#Convert the confusion matrix to a data frame
rpart_confusion_matrix_df <- as.data.frame(rpart_confusion_matrix)

rpart_evaluation_measures <- get_evaluation_measures("RPart unpruned",
                                                     rpart_confusion_matrix_df$Freq[1],
                                                     rpart_confusion_matrix_df$Freq[2],
                                                     rpart_confusion_matrix_df$Freq[3],
                                                     rpart_confusion_matrix_df$Freq[4])

#Can't get AUC
rpart_evaluation_measures$AUC <- NA

evaluation_measures <- rbind(evaluation_measures, rpart_evaluation_measures)

evaluation_measures

#what's the tree size which results in the min cross validated error
rpart_opt <- which.min(rpart_fit$cptable[,"xerror"])

#value of the complexity parameter (alpha) for that gives a tree of that size
rpart_cp <- rpart_fit$cptable[rpart_opt, "CP"]

# "prune" the tree using that value of the complexity parameter
rpart_pruned.model <- prune(rpart_fit,rpart_cp)

#plot pruned tree
library(rpart.plot)
prp(rpart_pruned.model)

#predictions from pruned model
rpart_pruned_predict <- predict(rpart_pruned.model,tree_testset[,-targetColNum],type="class")

#confusion matrix (PRUNED model)
rpart_pruned_confusion_matrix <- table(pred=rpart_pruned_predict,true=tree_testset[,targetColNum])

rpart_pruned_confusion_matrix

#Convert the confusion matrix to a data frame
rpart_pruned_confusion_matrix_df <- as.data.frame(rpart_pruned_confusion_matrix)

rpart_pruned_evaluation_measures <- get_evaluation_measures("RPart pruned",
                                                            rpart_pruned_confusion_matrix_df$Freq[1],
                                                            rpart_pruned_confusion_matrix_df$Freq[2],
                                                            rpart_pruned_confusion_matrix_df$Freq[3],
                                                            rpart_pruned_confusion_matrix_df$Freq[4])

#Can't get AUC
rpart_pruned_evaluation_measures$AUC <- NA


evaluation_measures <- rbind(evaluation_measures, rpart_pruned_evaluation_measures)

evaluation_measures

# Create a vector of our top 5 predictors
top_five_predictors = c("annualised_mileage", "mth_since_last_serv", "age_of_vehicle_years", "num_serv_dealer_purchased", "sched_serv_warr")

#Build random forest model
library(randomForest)
rf.model <- randomForest(Target ~.,
                         data = tree_trainset,
                         importance=TRUE,
                         xtest=tree_testset[,-targetColNum],
                         keep.forest = TRUE,
                         ntree=1000)

#predictions for test set
rf_predictions <- data.frame(tree_testset,rf.model$test$predicted)

#confusion matrix
rf_confusion_matrix <- table(pred=rf.model$test$predicted,true=tree_testset[,targetColNum])

rf_confusion_matrix

#Construct the 5 plots

# Annualised Mileage
p1 <- partial(rf.model, pred.var = top_five_predictors[1], plot = FALSE, rug = TRUE, plot.engine = "ggplot2")
# Months since last service
p2 <- partial(rf.model, pred.var = top_five_predictors[2], plot = FALSE, rug = TRUE, plot.engine = "ggplot2")
#Age of vehicle in years
p3 <- partial(rf.model, pred.var = top_five_predictors[3], plot = FALSE, rug = TRUE, plot.engine = "ggplot2")
#Number of services had at the same dealer where the vehicle was purchased
p4 <- partial(rf.model, pred.var = top_five_predictors[4], plot = FALSE, rug = TRUE, plot.engine = "ggplot2")
#Number of scheduled services used under warranty
p5 <- partial(rf.model, pred.var = top_five_predictors[5], plot = FALSE, rug = TRUE, plot.engine = "ggplot2")

#Let's also do a sixth for total services
p6 <- partial(rf.model, pred.var = "total_services", plot = FALSE, rug = TRUE, plot.engine = "ggplot2")
#Plot 1
autoplot(p1, ylab = "yhat") + theme_light() + ggtitle(paste("Partial dependency plot for", top_five_predictors[1]))


#Plot2
autoplot(p2, ylab = "yhat") + theme_light() + ggtitle(paste("Partial dependency plot for", top_five_predictors[2]))


#Plot 3
autoplot(p3, ylab = "yhat") + theme_light() + ggtitle(paste("Partial dependency plot for", top_five_predictors[3]))


#Plot 4
autoplot(p4, ylab = "yhat") + theme_light() + ggtitle(paste("Partial dependency plot for", top_five_predictors[4]))


#Plot 5
autoplot(p5, ylab = "yhat") + theme_light() + ggtitle(paste("Partial dependency plot for", top_five_predictors[5]))


#Plot 6
autoplot(p6, ylab = "yhat") + theme_light() + ggtitle("Partial dependency plot for total services")

#Now use the Random Forest model to predict for the validation set

#Train set is the full repurchase training set after cleaning for the tree models
trainset <- tree_data

#Car model 19 doesn't exist in the validation data set, so we filter it from the training set
trainset <- trainset %>%
  filter(car_model != "model_19")

#Re-factor car_model to have 18 factors as per validation set
trainset$car_model <- factor(trainset$car_model)

#Test set
testset <- repurchase_validation

#Run the model (don't include ID, age or gender columns in the testset)
validation.model <- randomForest(Target~.,
                                 data = trainset, 
                                 importance = TRUE, 
                                 xtest = testset[,4:ncol(testset)],
                                 ntree = 1000)

#predictions for test set
testset$target_class <- validation.model$test$predicted

testset$target_probability <- validation.model$test$votes[,2]

final_output <- testset %>%
  select(ID, target_probability, target_class)

write.csv(final_output, file="repurchase_validation_12945604.csv", row.names=FALSE)

#predictions for trainset
final_predictions <- data.frame(trainset,validation.model$predicted)

#confusion matrix
final_confusion_matrix <- table(validation.model$predicted,trainset[,targetColNum])

final_confusion_matrix

