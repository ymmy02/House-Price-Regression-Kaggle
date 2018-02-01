library(ggplot2)
library(readr)
library(Amelia)
library(e1071)
library(mice)
library(lattice)
library(rpart)
library(xgboost)

train <- read.csv("data/train.csv", stringsAsFactors = F)
test <- read.csv("data/test.csv", stringsAsFactors = F)

# train[is.na(train)] <- "No"
# test[is.na(test)] <- "No"

deal_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x[!is.na(x)])
  } else {
    x[is.na(x)] <- "No"
    x <- as.factor(x)
  }
  return (x)
}

for (i in 1:ncol(train)) {
  train[,i] <- deal_missing(train[,i])
}
for (i in 1:ncol(test)) {
  test[,i] <- deal_missing(test[,i])
}

price <- train[,c("SalePrice")]
train_and_test <- rbind(train[,-81], test)

train <- train_and_test[1:nrow(train),]
train <- cbind(train, price)
stt <- nrow(train)+1
end <- nrow(train_and_test)
test <- train_and_test[stt:end,]
train <- train[,-1]
id <- test[,1]
test <- test[,-1]

model.svm <- svm(price ~., data=train)
result <- predict(model.svm, test)

submission <- data.frame(Id = as.integer(id), SalePrice = result)
write_csv(submission, "svm_result.csv")
