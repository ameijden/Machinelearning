## Quiz week 3

## Question 1:
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)



data(segmentationOriginal)
library(dplyr)

testdata <- segmentationOriginal %>%
  filter(Case=="Test")

traindata <- segmentationOriginal %>%
  filter(Case=="Train")

RNGversion("3.0.0") ## Otherwise the results might be different, see week 2 quiz
set.seed(125)

## fit a CART model with the rpart method using all predictor variables and default caret settings.

fit <- train(Class ~., data=traindata, method="rpart")

?train

names(getModelInfo())

print(fit$finalModel)

## Cart model is decision tree, the R implementation of the CART algorithm 
## is called RPART (Recursive Partitioning And Regression Trees)

a <- c(TotalIntench2 = 23,000, FiberWidthCh1 = 10, PerimStatusCh1=2)
b <- c(TotalIntench2 = 50,000, FiberWidthCh1 = 10, VarIntenCh4 = 100)
c <- c(TotalIntench2 = 57,000, FiberWidthCh1 = 8, VarIntenCh4 = 100)
d <- c(FiberWidthCh1 = 8, VarIntenCh4 = 100, PerimStatusCh1=2)

predict(fit, newdata = b)

library(ggplot2)
library(rattle)
library(rpart.plot)
rattle::fancyRpartPlot(fit$finalModel)

?rattle

## Answer: PS / WS / PS / WS (Last one is a guess because the tree doesn't have this info) [NOK]
## Answer: PS / WS / PS / not possible to predict

 # now predict w/ a data frame
TotalIntenCh2 <- c(23000,50000,57000,NA)
FiberWidthCh1 <- c(10,10,8,8)
VarIntenCh4 <- c(NA,100,100,100)
PerimStatusCh1 <- c(2,NA,NA,2)
theTestData <- data.frame(TotalIntenCh2, FiberWidthCh1,VarIntenCh4, PerimStatusCh1)
test_data <- data.frame(matrix(data = 0, nrow = 4, ncol = length(names(segmentationOriginal))))
predict(fit,newdata = test_data)

## Question 2: If K is small in a K-fold cross validation is the bias in the estimate of
## out-of-sample (test set) accuracy smaller or bigger? 
## If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger.
## Is K large or small in leave one out cross validation?


## The bias is smaller and the variance is bigger. Under leave one out cross validation K 
## is equal to one.

## The bias is smaller and the variance is smaller. Under leave one out cross validation K 
## is equal to one.

## The bias is larger and the variance is smaller. Under leave one out cross validation K 
## is equal to two.

## The bias is larger and the variance is smaller. Under leave one out cross validation K 
## is equal to the sample size. [OK]

## Cross-validation is a statistical method used to estimate the skill of machine learning models.
## The procedure has a single parameter called k that refers to the number of groups 
## that a given data sample is to be split into

### Three common tactics for choosing a value for k are as follows:
  
##  Representative: The value for k is chosen such that each train/test group of data samples is 
## large enough to be statistically representative of the broader dataset.

## k=10: The value for k is fixed to 10, a value that has been found through experimentation 
## to generally result in a model skill estimate with low bias a modest variance.

## k=n: The value for k is fixed to n, where n is the size of the dataset to give each 
## test sample an opportunity to be used in the hold out dataset. 
## This approach is called leave-one-out cross-validation.

## Question 3
## Load the olive oil data using the commands:

library(pgmm)

data(olive)
olive = olive[,-1]

load(file="olive.rda")
getwd()

fitq3 <- train(Area ~., data=olive, method="rpart")
predict(fitq3, newdata = as.data.frame(t(colMeans(olive))))

rattle::fancyRpartPlot(fitq3$finalModel)

2.783

## Question 4
library(ElemStatLearn)
data(SAheart)
RNGversion("3.0.0")
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

fitq4 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")

## Make chd a factor -> don't do it, this will ruin everything -> ignore the warning
## trainSA$chd <- as.factor(trainSA$chd)
# testSA$chd <- as.factor(trainSA$chd)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

b <- predict(fitq4, newdata = trainSA)
missClass(trainSA$chd, b)
0.2727273

c <- predict(fitq4, newdata = testSA)
missClass(trainSA$chd, c)
0.4761905 ## 0.31

## Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

## Set the variable y to be a factor variable in both the training and test set.
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

RNGversion("3.0.0")
set.seed(33833)

??randomForest

fitq5 <- train(y ~ ., method="rf", data=vowel.train)

plot(fitq5$finalModel)
varImp(fitq5$finalModel)

## x.2, x.1, x.5, x.6

