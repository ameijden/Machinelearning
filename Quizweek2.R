

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

## Which of the following commands will create non-overlapping training and test sets 
## with about 50% of the observations assigned to each?

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)


adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[-trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training2 = adData[-testIndex,]
testing2 = adData[testIndex,] ## This one

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training3 = adData[trainIndex,]
testing3 = adData[trainIndex,]

## 2 
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
## Color by each of the variables in the data set (you may find the cut2() function 
## in the Hmisc package useful for turning continuous covariates into factors).
## What do you notice in these plots?

library(Hmisc)

library(ggplot2)
str(training)

cutCement <- cut2(training$Cement, g=3) ## -> cuts the data in three factors
cutBlastFurnaceSlag <- cut2(training$BlastFurnaceSlag, g=3)
cutFlyAsh <- cut2(training$Flyash, g=4)
cutWater <- cut2(training$Water, g=3)
cutSuperplasticizer <- cut2(training$Superplasticizer, g=3)
cutCoarseAggregate <- cut2(training$CoarseAggregate, g=3)
cutAge <- cut2(training$Age, g=3)

naflyash <- is.na(training$FlyAsh)
table(naflyash)
str(training$FlyAsh)
unique(training$FlyAsh)

qplot(CompressiveStrength, colour=Cement, data=training, geom="density")
qplot(CompressiveStrength, colour=cutCement, data=training, geom="density")
qplot(CompressiveStrength, colour=cutBlastFurnaceSlag, data=training, geom="density")
qplot(CompressiveStrength, colour=cutFlyAsh , data=training, geom="density")
qplot(CompressiveStrength, colour=cutWater, data=training, geom="density")
qplot(CompressiveStrength, colour=cutSuperplasticizer, data=training, geom="density")
qplot(CompressiveStrength, colour=cutCoarseAggregate, data=training, geom="density")
qplot(CompressiveStrength, colour=cutAge, data=training, geom="density")

featurePlot(x=training, y=training$CompressiveStrength, plot = "pairs")


qplot(index, CompressiveStrength, data=training, color=cutCement)
qplot(index, CompressiveStrength, data=training, color=cutBlastFurnaceSlag)
qplot(index, CompressiveStrength, data=training, color=cutFlyAsh)
qplot(index, CompressiveStrength, data=training, color=cutWater)
qplot(index, CompressiveStrength, data=training, color=cutSuperplasticizer)
qplot(index, CompressiveStrength, data=training, color=cutCoarseAggregate)
qplot(index, CompressiveStrength, data=training, color=cutAge)


##  There is a non-random pattern in the plot of the outcome versus index that does not appear 
## to be perfectly explained by any predictor suggesting a variable may be missing.

## Q3: 

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
unique(training$Superplasticizer)

hist(log(training$Superplasticizer))

mean(training$Superplasticizer)
sd(training$Superplasticizer)


## The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer [NOK]

## There are values of zero so when you take the log() transform those values will be -Inf. [OK]



## 4.Question 4
## Load the Alzheimer's disease data using the 

## Q4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]



## Find all the predictor variables in the training set that begin with IL.
## Perform principal components on these variables with the preProcess() function 
## from the caret package. Calculate the number of principal components needed to capture 
## 90% of the variance. How many are there?

trainingonlyIL <- grep("^IL", names(training))
library(dplyr)

traningonlyILdata <- training[, c(names(training)[trainingonlyIL])]
names(traningonlyILdata)

il.pca <- prcomp(traningonlyILdata, center=TRUE, scale. = TRUE,)
summary(il.pca)

## Looking at Proportion of Variance 
0.3533 + 0.1183 + 0.09292 + 0.08134 + 0.06734 + 0.06198 +  0.05589 + 0.04756 + 0.03405 = 0.91268

0.3533 + 0.1183 + 0.09292 + 0.08134 + 0.06734 + 0.06198 +  0.05589 = 0.83107

## Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Create a training data set consisting of only the predictors with variable names
## beginning with IL and the diagnosis. Build two predictive models, one using the predictors 
## as they are and one using PCA with principal components explaining 80% of the variance 
## in the predictors. Use method="glm" in the train function.


trainingonlyIL <- grep("^IL", names(training))
library(dplyr)

traningonlyILdata <- training[, c(names(training)[trainingonlyIL], "diagnosis")]
names(traningonlyILdata)

Model1 <- train(diagnosis ~ ., data = traningonlyILdata, method="glm")
summary(Model1$finalModel)
summary(Model1$results)
Model1$results

Model1ontest <- predict(model1, data=)


## non=pca 0,65