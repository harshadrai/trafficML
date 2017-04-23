# Logistic Regression

setwd("C:/Users/kkrishn3/Desktop/Sowers/predictive_model/trafficML")
data <- read.csv("balanced_dataset.csv")
data <- data[,-1]

# Split into train-test
train_ind <- sample(seq_len(nrow(data)), size = 0.8*nrow(data))
train <- read.csv("max_roc_train.csv")#data[train_ind, ]
test <- read.csv("max_roc_test.csv")#data[-train_ind, ]

#Build a logit model
model <- glm(fatality~.,data=train,family="binomial")
predicted <- predict(model, newdata=test, type = "response")
library(pROC)
auc <- roc(test$fatality, predicted)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC for Logistic Regression:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#write.csv(train,"max_roc_train.csv")
#write.csv(test,"max_roc_test.csv")

# Build a random forest model
library(randomForest)
fit <- randomForest(as.factor(fatality)~.,data=train, importance = TRUE, ntree = 1000)
predicted <- predict(fit, test)
test$acc <- ifelse(test$fatality==test$predicted,1,0)
sum(test$acc)/length(test$acc)
# Accuracy = 0.815025










