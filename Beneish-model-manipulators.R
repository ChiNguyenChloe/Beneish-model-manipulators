library(readxl)
complete_data <- read_excel("/Users/chinguyen/Downloads/Complete data.xlsx")
sample_data <- read_excel("/Users/chinguyen/Downloads/Sample data.xlsx")
attach(complete_data)
attach(sample_data)
complete_data$Manipulater <- as.factor(complete_data$Manipulater)
sample_data$Manipulator <-as.factor(sample_data$Manipulator)

#3
library(ROSE)
library(ISLR)
balanced_data1 <- ovun.sample(Manipulator ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI, data = sample_data, method = "both")$data
summary(balanced_data1$Manipulator)
set.seed(123)
indx <- sample(2, nrow(balanced_data1), replace = T, prob = c(0.7,0.3))
train1 <- balanced_data1[indx == 1, ]
test1 <- balanced_data1[indx == 2, ]
logit_model1 <- glm(Manipulator ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI, data = train1, family = "binomial")
summary(logit_model1)

#4
predicted_test <- predict(logit_model1, newdata = test1, type = "response") 
predicted_test_class <- ifelse(predicted > 0.5, "Yes", "No")
predicted_test_class <- as.factor(predicted_test_class)
actual_test <- test1$Manipulator
library(caret)
library(e1071)
confusionMatrix(predicted_test_class, reference = actual_test, positive = "Yes")

predicted_train <- predict(logit_model1, newdata = train1, type = "response") 
predicted_train_class <- ifelse(predicted_train > 0.5, "Yes", "No")
predicted_train_class <- as.factor(predicted__train_class)
actual_train <- train1$Manipulator
confusionMatrix(predicted_train_class, reference = actual_train, positive = "Yes")

#5
predicted <- predict(logit_model1, newdata = test1, type = "response")
actual <- test1$Manipulator
library(ROCR)
pred <- prediction(predicted, actual)
perf <- performance(pred, "tpr", "fpr")
opt.cut <- function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, 
  perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))

#7
library(rpart)
tree_model <- rpart(Manipulator ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI, data = train1)

library(rpart.plot)
rpart.plot(tree_model)
print(tree_model)
summary(tree_model)

tree_predict_class <- predict(tree_model,test1, type = "class")
mean(test1$Manipulator == tree_predict_class)

tree_predict__train_class <- predict(tree_model,train1, type = "class")
mean(train1$Manipulator == tree_predict__train_class)

#8
balanced_data2 <- ovun.sample(Manipulater ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI, data = complete_data, method = "both")$data
summary(balanced_data2$Manipulater)
set.seed(456)
indxx<- sample(2, nrow(balanced_data2), replace = T, prob = c(0.7,0.3))
train2 <- balanced_data2[indxx == 1, ]
test2 <- balanced_data2[indxx == 2, ]
logit_model2 <- glm(Manipulater ~ DSRI + GMI + AQI + SGI + DEPI + SGAI + ACCR + LEVI, data = train2, family = "binomial")
summary(logit_model2)
