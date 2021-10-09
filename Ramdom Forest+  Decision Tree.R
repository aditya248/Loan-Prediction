
## Data Processing
# imports
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(ISLR)
library(glmnet)
library(dplyr)

# Cleanning data
dd <- fread("./Training-Data.csv")
data <- dd
data$Id <- NULL
colnames(data)[4] <- "Marital_Status"

## Make sample dataset-1: Based on original dataset ratio
# Get Risk_Flag original dataset ratio
total <- length(data$Risk_Flag)
true <- length(data$Risk_Flag[data$Risk_Flag==1])
false <- length(data$Risk_Flag[data$Risk_Flag==0])
# Calculate dataset ratio
true_ratio <- true/total
false_ratio <- false/total
# Get sample dataset size for 0 and 1
true_smp_size <- floor(true_ratio * 20000)
false_smp_size <- floor(false_ratio * 20000)
# set seed
set.seed(12345)
# Split dataset for Risk_Flag=1 and Risk_Flag=0
true_ind <- sample(seq_len(nrow(data[data$Risk_Flag==1])), size = true_smp_size)
false_ind <- sample(seq_len(nrow(data[data$Risk_Flag==0])), size = false_smp_size)
# Split the data frames
true_smp <- (data[data$Risk_Flag==1])[true_ind, ]
false_smp <- (data[data$Risk_Flag==0])[false_ind, ]
# Create sample dataset size
sample1 <- rbind(false_smp, true_smp)
# Randomize sample's order
sample1 <- sample1[sample(nrow(sample1)),]

## Split sample dataset to test and train
# Split dataset
set.seed(123)
smp_size <- floor(0.70 * nrow(sample1))
train_ind <- sample(1:nrow(sample1),size = smp_size)

# Split the data frames
risk_train <- sample1[train_ind,]
risk_test <- sample1[-train_ind,]
# Get sample output 
y_train_sample <- risk_train$Risk_Flag
y_test_sample <- risk_test$Risk_Flag
# Check test and train dataset
prop.table(table(risk_train$Risk_Flag))
prop.table(table(risk_test$Risk_Flag))

## Random Forests ----
# random forest with default settings
#install.packages("randomForest")
#install.packages("caret")
library(randomForest)
library(caret)
set.seed(123)
# Convert numerical variable to factor
risk_train$Risk_Flag <- as.factor(risk_train$Risk_Flag)
risk_test$Risk_Flag <- as.factor(risk_test$Risk_Flag)
# Run randomForest model
risk_rf <- randomForest(Risk_Flag ~ ., data = risk_train)
risk_rf_pred <- predict(risk_rf, risk_test)
# Check confusion Matrix
confMat_rf <- confusionMatrix(risk_rf_pred,risk_test$Risk_Flag)
confMat_rf
# Calculate MSE for random forest
mse.forest <- mean(((as.integer(risk_rf_pred)-1) - y_test_sample) ^ 2)
print(mse.forest)


# machine learning - decision tree
#install.packages(c("rpart.plot", "rpart"))
library(rpart)
library(rpart.plot)

tree.model <-rpart(Risk_Flag~Income + Age +
                     Experience + Marital_Status +
                     House_Ownership + Car_Ownership +  CURRENT_JOB_YRS + Profession + CITY + STATE + CURRENT_HOUSE_YRS
, data = risk_train,control=rpart.control(cp=0.003))
tree.model
summary(tree.model)

plot(tree.model,compress=TRUE)
rpart.plot(tree.model)
text(tree.model,pretty = 0 )

