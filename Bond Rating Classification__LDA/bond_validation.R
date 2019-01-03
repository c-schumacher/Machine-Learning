library(MASS)
library(caret)
library(hmeasure)

path <- "PATH"
setwd(path)
df_train <- read.csv("BondRatingTrain.csv", header=T)
df_test <- read.csv("BondRatingValidation.csv", header=T)

bondLDA <- lda(CODERTG ~., data=df_train[3:13])
bondLDA

# Predicting the training target using the trained model
p = predict(bondLDA, newdata=df_train[4:13])$class
p

# Confusion matrix for predicting training set based on the trained model
table(p, df_train$CODERTG)

#cross fold validated
bondLDA2 = lda(CODERTG ~ ., data=df_train[,3:13], CV=T)

### predicting the validation data
p2 = predict(bondLDA, newdata=df_test[,4:13])$class
table(p2, df_test$CODERTG)

