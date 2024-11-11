library(benford.analysis)
library(dplyr)
library(BenfordTests)
library(caTools)
library(caret)
library(randomForest)
library(ROSE)

# Benford's Law Analysis
data("corporate.payment")
bfd_cp <- benford(corporate.payment$Amount, number.of.digits = 2, sign = 'positive')
fur_anl <- getSuspects(bfd_cp, corporate.payment)
plot(bfd_cp)
summary(fur_anl$Amount)
chisq.benftest(corporate.payment$Amount, digits = 2)

# Credit Card Fraud Detection
data <- read.csv("~/Desktop/Other/Advanced Financial Analytics/Credit Card.csv")
split <- sample.split(data$Class, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
train_data_rose <- ROSE(Class ~ ., data = train_data, seed = 123)$data
rf_model <- randomForest(Class ~ ., data = train_data_rose, ntree = 50)
rf_predictions <- predict(rf_model, newdata = test_data)