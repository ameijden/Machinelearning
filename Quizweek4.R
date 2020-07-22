## Week 4 Quiz

## Question 1:

library(ElemStatLearn)
library(caret)
library(AppliedPredictiveModeling)
data(vowel.train)

data(vowel.test)

## Set the variable y to be a factor variable in both the training and test set.
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

RNGversion("3.0.0")
set.seed(33833)

fitq1rf <- train(y ~ ., method="rf", data=vowel.train)


fitq1rf$finalModel
fitq1rf$metric
fitq1rf$results

predrf <- predict(fitq1rf, newdata = vowel.test)

cmrf <- confusionMatrix(predrf, vowel.test$y)

Accuracy : 0.6039    


fitq1gbm <- train(y ~ ., method="gbm", data=vowel.train)
predgbm <- predict(fitq1gbm, newdata = vowel.test)
cmgbm <- confusionMatrix(predgbm, vowel.test$y)

Accuracy : 0.526     

## What is the accuracy among the test set samples where the two methods agree?
## See course materials on 025 combiningPredictors 
## Fit a model that combines predictors

predDF <- data.frame(predrf,predgbm,y=vowel.test$y)
combModFit <- train(y ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)

cmcomb <- confusionMatrix(combPred, vowel.test$y)
0.1537  ## So combined this is much worse

## So 
predDF <- data.frame(predrf,predgbm,y=vowel.test$y)

## Where the two methods agree
predrf == predgbm

library(dplyr)
agree1 <- predDF %>%
  filter(predrf == predgbm)

## 318 they agree on of the total of 462 in the testset.
318/462= 0.6883117 ## What they agree on 


## Now, how much is correct?
agreecorrect <- agree1 %>%
  filter(predrf == y)

## 209 are correct of the 462 of the total test set. 
209/462 = 0.452381 

## Or of the part they agree, so even then only about 66% correct. 
209 / 318 = 0.6572327

## Accuracy = percentage of correctly classified instances 
## (TP + TN) / (TP + TN + FP + FN)
## True Positives, True Negatives, False Positives and False Negatives
## Sensitivity = TP / TP + FN
## Specificity = TN / TN + FP
## Precision = TP / TP + FP
## True-Positive Rate = TP / TP + FN
## False-Positive Rate = FP / FP + TN
## True-Negative Rate = TN / TN + FP
## False-Negative Rate = FN / FN + TP

## Q2

library(caret)

library(gbm)
RNGversion("3.0.0")

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)



fitq2rf <- train(diagnosis ~ ., method="rf", data=training)
predrf <- predict(fitq2rf, newdata = testing)
cmrf <- confusionMatrix(predrf,  testing$diagnosis)
Accuracy : 0.7927  

fitq2gbm <- train(diagnosis ~ ., method="gbm", data=training)
predgbm <- predict(fitq2gbm, newdata = testing)
cmgbm <- confusionMatrix(predgbm,  testing$diagnosis)
Accuracy :   0.7805  

fitq2lda <- train(diagnosis ~ ., method="lda", data=training)
predlda <- predict(fitq2lda, newdata = testing)
cmlda <- confusionMatrix(predlda,  testing$diagnosis)
Accuracy : 0.7683      

## Example from course
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
##### 


predDF <- data.frame(predrf ,predgbm,predlda, diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
cmcombi <- confusionMatrix(combPred, testing$diagnosis )
Accuracy : 0.8049 


### Q3
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]


set.seed(233)


fitq3lasso <- train(CompressiveStrength ~., data = training, method="lasso")

names(getModelInfo())

?plot.enet


plot(fitq3lasso$finalModel, xvar=c("penalty"), use.color=TRUE)

## Cemenet

### Q4
## Fit a model using the bats() function in the forecast package to the training time series. 
## Then forecast this model for the remaining time points. 
## For how many of the testing points is the true value within the 95% prediction interval bounds?


library(lubridate) # For year() function below

dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

tstesting = ts(testing$visitsTumblr)

library(forecast)
??forecast
??bats

?forecast

fitq4 <- bats(tstrain)
forq4 <- forecast(tstesting, model=fitq4, level = 95) ## missing number of periods for forecasting

forq4b <- forecast(fitq4, h=nrow(testing)) ## missing confidence level
forq4c <- forecast(fitq4, level=95, nrow(testing))
forq4d <- forecast(fitq4, level=95, h=nrow(testing)) ## Same as c 

forq4$lower ## -
forq4$upper
## lwr and upr: the lower and the upper confidence limits for the expected values, 
## respectively. By default the function produces the 95% confidence limits.

q4clower <- forq4c$lower
q4cupper <-forq4c$upper

q4dlower <- forq4d$lower
q4dupper <-forq4d$upper


table((testing$visitsTumblr>q4clower) & (testing$visitsTumblr<q4cupper))
table((testing$visitsTumblr>q4dlower) & (testing$visitsTumblr<q4dupper))

##FALSE  TRUE 
##    9   226 

1-9/235=0.9617021

### Q5

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

library(e1071)

fitq5 <- svm(CompressiveStrength ~., data = training)
predq5 <- predict(fitq5, newdata=testing)

## Calculate the RMSE
## sqrt(mean(predq5 -testing$CompressiveStrength)^2)
answerq5 <- sqrt(mean((predq5 -testing$CompressiveStrength)^2))
6.715009
error = predq5 - testing$CompressiveStrength

