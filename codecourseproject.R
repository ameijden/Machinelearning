## Course Project Practical Machine learning

## One thing that people regularly do is quantify how much of a particular activity they do, 
## but they rarely quantify how well they do it. In this project, your goal will be to use data 
## from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
## They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.
## More information is available from the website here: http://groupware.les.inf.puc-rio.br/har 
## (see the section on the Weight Lifting Exercise Dataset).

#### The goal of your project is to predict the manner in which they did the exercise.
## This is the "classe" variable in the training set.

## You may use any of the other variables to predict with. 
## You should create a report describing how you built your model, 
## how you used cross validation, 
## what you think the expected out of sample error is, 
## and why you made the choices you did. 
## You will also use your prediction model to predict 20 different test cases.


#### Peer Review Portion
## Your submission for the Peer Review portion should consist of a link to a Github repo with your R markdown 
## and compiled HTML file describing your analysis. 

## Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. 
## It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page 
## can be viewed online (and you always want to make it easy on graders :-).


#### Course Project Prediction Quiz Portion

## Apply your machine learning algorithm to the 20 test cases available in the test data above and 
## submit your predictions in appropriate format to the Course Project Prediction Quiz for automated grading.


### 
## Load data
## Eploratory data analysis (Cleaning necessary?)
## Linear Regression with statistics
## Machine learning (Linear Regression / Random forests / etc.)

###

training <- read.csv2("pml-training.csv", sep=",",na.strings = c("NA","NaN","","#DIV/0!"))

testing <- read.csv2("pml-testing.csv", sep="," ,na.strings = c("NA","NaN","","#DIV/0!"))

## Split the training test? 

## 160 variables -> too much?

str(training)

training$classe -> ABCDE

## More info about the data is available at http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har
## Weight Lifting Exercises Dataset
## Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell
## Biceps Curl in five different fashions: 
## exactly according to the specification (Class A), 
## throwing the elbows to the front (Class B),
## lifting the dumbbell only halfway (Class C), 
## lowering the dumbbell only halfway (Class D) 
## and throwing the hips to the front (Class E).

## Class A corresponds to the specified execution of the exercise, 
## while the other 4 classes correspond to common mistakes.

## Also a nice report with all methods used by the scientists is downloaded

library(caret)
fitlm1 <- train(classe ~., data=training52, method="glm")

nas <- (is.na(training))
table(nas)

library(dplyr)

Aclass <- training %>%
  filter(classe=="A")


Aclass2 <- Aclass[,colSums(is.na(Aclass))<nrow(Aclass)*0.1]
## Remove columns with 10% NA's

training4 <- training[ , colSums(is.na(training)) == 0]

training2 <- training[ , colSums(is.na(training)) <nrow(training)*0.5]
## Remove columns which have more than 50% NA's

## Remove all columns with NA's

str(training2)

## Remove first 7 columns

training5 <-  select(training4, -c(1,2,3,4,5,6,7))
str(training5)

plot(training5$total_accel_dumbbell, training5$total_accel_forearm, col=training5$classe)
plot(training5$total_accel_belt, training5$total_accel_dumbbell, col=training5$classe)
legend("topright", pch = c(1), cex = 0.8)
library(ggplot2)
ggplot(data=training5, aes(total_accel_belt,total_accel_dumbbell))+
geom_point(aes(color=classe))

ggplot(data=training5, aes(total_accel_forearm,total_accel_dumbbell))+
  geom_point(aes(color=classe))

ggplot(data=training5, aes(gyros_arm_x,magnet_arm_x))+
  geom_point(aes(color=classe))

str(training5)

A <- sum(training5$classe=="A") - 5580
B <- sum(training5$classe=="B") - 3797
C <- sum(training5$classe=="C") - 3422
D <- sum(training5$classe=="D") - 3216
E <- sum(training5$classe=="E") - 3670

## Plots looks like we have a lot of class E and not much of the others, that is not true

## Create the right plots (see PDF)?

## Or identify the most relevant features as proposed by Hall "Best First", based on backtracking.
## They selected 17 features.
## M. A. Hall. Correlation-based Feature Subset Selection
##  for Machine Learning. PhD thesis, Department of
##  Computer Science, University of Waikato, Hamilton,
##  New Zealand, Apr. 1999.

## preProcess

### caret package
### preObj <- preProcess(training[,-58], method=c[“center”, “scale”] 
### Not variable 58 because that is the variabele we want to predict. 
### Don't forget to do this also for the testset!!
                     
                     
######### Feature selection strategy (https://www.youtube.com/watch?v=ioXKxulmwVQ) with python code
## Remove NA's (high percentage) -> review or visualize these variables
##### training2 <- training[ , colSums(is.na(training)) <nrow(training)*0.5] -> Remove all NA's above 50%

### 50 to 95% drop variabels / < 50% use imputation 
### preObj <- preProcess(training[,-58], method=”knnImpute”) 
#### -> using 10 most likely numbers, take the mean and imputes this in the dataset
##### Carefull when transforming factor variables (usually not done)

## Remove variables with low variation -> VAR(x) = sigma squared -> standardize all variables 

#### Standardize -> use scall function (Do this also in the Test set or use the preObj in the predict function)
### Can also be done in the train function
library(caret)
preObj <- preProcess(training5[,-53], method=c("center", "scale"))
traintotalaccelbelt <- predict(preObj,training5[,-53])$total_accel_belt
var(traintotalaccelbelt)
mean(traintotalaccelbelt)
sd(traintotalaccelbelt)

trainaccelbeltx <- predict(preObj,training5[,-53])$accel_belt_x
var(trainaccelbeltx)
mean(trainaccelbeltx)
sd(trainaccelbeltx)

trainaccelbelty <- predict(preObj,training5[,-53])$accel_belt_y
var(trainaccelbelty)
mean(trainaccelbelty)
sd(trainaccelbelty)

trainaccelbeltz <- predict(preObj,training5[,-53])$accel_belt_z
var(trainaccelbeltz)
mean(trainaccelbeltz)
sd(trainaccelbeltz)

hist(training5$total_accel_belt)
hist(traintotalaccelbelt)

hist(training5$accel_belt_x, col = training5$classe)
hist(trainaccelbeltx, col = training5$classe)
?hist

ggplot(data=training5, aes(accel_belt_x))+
  geom_histogram(aes(fill=classe))


ggplot(data=training5, aes(accel_belt_y))+
  geom_histogram(aes(fill=classe))


ggplot(data=training5, aes(accel_belt_z))+
  geom_histogram(aes(fill=classe))

## Transform using "BoxCox"? t take continuous data, and try to make them look like normal data 
### and they do that by estimating a specific set of parameters using maximum likelihood. 

nearZeroVar(training5[,-53], saveMetrics = TRUE)
## In this case all FALSE so we can't delete a variabele based on zero or near zero variance.
## Variables with the highest number of Unique values roll_dumbbell pitch_dumbbell  yaw_dumbbell 
## Make some plots

library(ggplot2)
ggplot(data=training5, aes(roll_dumbbell ,pitch_dumbbell))+
  geom_point(aes(color=classe))

ggplot(data=training5, aes(yaw_dumbbell ,pitch_dumbbell))+
  geom_point(aes(color=classe))


ggplot(data=training5, aes(pitch_forearm  ,pitch_dumbbell))+
  geom_point(aes(color=classe))




ggplot(data=training2b, aes(pitch_forearm,magnet_belt_y ))+
  geom_point(aes(color=classe)) ## Add later after 

?preProcess

nearZeroVar(preObj) ## Not possible

preObj2 <- preProcess(training5[,-53], method=c("corr"))



cortraining <- cor(training5)

training6 <- as.numeric(levels(training5[,-53]))[training5[,-53]]

Obj3 <- findCorrelation() ## Use a correlation matrix


#### Skewed variables have a much higher sd than the mean (any in this dataset?) -> center and scale this variables

### PCA

library(dplyr)
  training6 <- training5 %>%
      select_if(is.integer)


PCA <- abs(cor(training6))
diag(PCA) <- 0 ## Diagnols always have 1 (because it always correlates with itself)
which(PCA > 0.8, arr.ind=T)

plot(training$accel_belt_y, training$total_accel_belt)

plot(training5$accel_belt_z, training5$total_accel_belt)

plot(training5$accel_belt_x, training5$magnet_belt_x)

training7 <- prcomp(training6, center=TRUE, scale = TRUE) ## Do this only for some variables

abytab <- training6[,c("accel_belt_y", "total_accel_belt")]
prcombabytab <- prcomp(abytab)
plot(prcombabytab$x[,1],prcombabytab$x[,2])
prcombabytab$rotation



library(caret)
preProc <- preProcess(log10(training6[,-53]+1), method = "pca", pcaComp = 2)
PC <- predict(preProc, log10(training6[,-53]+1))
plot(PC[,1],PC[,2])


## If two variables are highly correlated, keeping only one -> which to keep? The one that has a higher
### Correlation matrix with tolerance 0.65
### correlation with the target -> How to know?
## 
## Feature selection (PCA) Principal Compenent Analysis
## Drop variables which have a very low correlation with the target

## Forward selection
### Identify the best variable
### Add the next best variable
### Etc.

## Backward elimination
### Start with all variables
##E Drop the least usefull one (based on the smallest drop in model accuracy)
## Etc

## lasso -> does feature selection for you

## Tree based models -> set a level

## Set up model evaluation first

####  From factor to numeric
training3 <- training2

training3$roll_belt <- levels(training3$roll_belt)[training3$roll_belt]
training3$roll_belt <- as.numeric(training3$roll_belt)

str(training3)

training3 <- training2

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
training3a$roll_belt <- as.numeric.factor(training3a$roll_belt)
### 


########################## Start ############## 


training <- read.csv2("pml-training.csv", sep=",",na.strings = c("NA","NaN","","#DIV/0!"))

testing <- read.csv2("pml-testing.csv", sep="," ,na.strings = c("NA","NaN","","#DIV/0!"))


nas <- (is.na(training))
table(nas)

training2 <- training[ , colSums(is.na(training)) <nrow(training)*0.5]
## Remove columns which have more than 50% NA's

library(dplyr)
training2b <-  select(training2, -c(1,2,3,4,5,6,7))
str(training2)

plot(training2b$total_accel_dumbbell, training2b$total_accel_forearm, col=training2b$classe)
plot(training2b$total_accel_belt, training2b$total_accel_dumbbell, col=training2b$classe)
legend("topright", pch = c(1), cex = 0.8)

library(ggplot2)
ggplot(data=training2b, aes(total_accel_belt,total_accel_dumbbell))+
  geom_point(aes(color=classe))

ggplot(data=training2b, aes(total_accel_forearm,total_accel_dumbbell))+
  geom_point(aes(color=classe))

ggplot(data=training2b, aes(gyros_arm_x,magnet_arm_x))+
  geom_point(aes(color=classe))

ggplot(data=training52, aes(x=pitch_forearm))+
  geom_density(aes(color=classe))

ggplot(data=training52, aes(x=magnet_arm_x))+
  geom_density(aes(color=classe))


ggplot(data=training52, aes(x=total_accel_dumbbell))+
  geom_density(aes(color=classe))

ggplot(data=training2b, aes(x=total_accel_dumbbell))+
  geom_density(aes(color=classe))

ggplot(data=training52, aes(x=pitch_forearm,magnet_belt_y ))+
  geom_point(aes(color=classe))

ggplot(data=training52, aes(pitch_forearm,magnet_belt_y ))+
  geom_point(aes(color=classe))

ggplot(data=training52, aes(x=classe, y=total_accel_dumbbell))+geom_boxplot()
ggplot(data=training52, aes(x=classe, y=pitch_forearm))+geom_boxplot()
ggplot(data=training52, aes(x=classe, y=magnet_belt_y))+geom_boxplot()




str(training52)

library(dplyr)
training2f <- training2b[,-53] %>%
  select_if(is.factor)

library(tidyverse)
training2nf <- training2b %>%
  select_if(negate(is.factor))

## Convert all columns to numeric of a dataset with only factors
training2fn <- lapply(training2f, function(x) as.numeric(levels(x))[x])

## Combine these datasets to get a dataset with all numeric values (except of course classe)
training3 <- cbind(training2nf, training2fn, training2b$classe)

colnames(training3)[53] <- "classe"

library(caret)
nearZeroVar(training3[,-53], saveMetrics = TRUE) ## No Trues
cor(training3[,-53])

PCA <- abs(cor(training3[,-53]))
diag(PCA) <- 0 ## Diagnols always have 1 (because it always correlates with itself)
which(PCA > 0.95, arr.ind=T)

training4 <- training3
training4$classe <- as.numeric(training4$classe)

## Keep only the variable with highest correlation with class
cor(training4$classe, training4$accel_belt_z)
cor(training4$classe, training4$total_accel_belt) ## Has highest with class
cor(training4$classe, training4$roll_belt)

##### Create graph with correlation (https://www.kaggle.com/reisel/how-to-handle-correlated-features)
survived <- training4$classe
feature <- names(training4)

corrSurvived <- data.frame(feature = feature, coef = rep(NA, length(feature)))
for (iFeature in 1:length(feature)){
  corrSurvived$coef[iFeature] <- cor(training4[, iFeature], survived)
}

# sort by correlation coefficient
corrSurvivedOrder <- corrSurvived[order(corrSurvived$coef, decreasing = FALSE), ]

ggplot(corrSurvivedOrder, aes(x = factor(feature, levels = feature), y = coef)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  xlab("Feature") + 
  ylab("Correlation Coefficient")


### Remove the one (of the two correlated features) which scored lowest on the correlation with class

library(dplyr)
training5 <- select(training4, -c(4, 26, 3, 10, 28, 2, 7, 13, 11, 17, 39, 40, 20, 23, 27, 35, 41, 50))
                              
## Next step / Random forests / Ensemble Trees https://thenewstack.io/3-new-techniques-for-data-dimensionality-reduction-in-machine-learning/

### We generate a large set (2,000) of very shallow trees (two levels), and each tree is trained on a 
## small fraction (three columns) of the total number of columns. If a column is often selected as the best split, 
## it is very likely to be an informative column that we should keep. For all columns, we calculate a score as 
## the number of times that the column was selected for the split, divided by the number of times in which 
## it was a candidate. The most predictive columns are those with the highest scores.


library(caret)
?train

training6 <- select(training5, c(1,2,3,35))

modfit1 <- train(classe ~., data = train6, method="rf", prox=TRUE) ## Takes too long
modfit1a <-  train(classe ~., data = training6, method="rpart")
print(modfit1a$finalModel)
plot(modfit1a$finalModel)
library(pgmm)
library(rattle)
rattle::fancyRpartPlot(modfit1a$finalModel) ## Shows a nice tree with only a few predictors 

training7 <- select(training6, -c(4))
training8 <- cbind(training7, training$classe)
colnames(training8)[4] <- "classe"

modfit1b <-  train(classe ~., data = training8, method="rpart")
rattle::fancyRpartPlot(modfit1b$finalModel)

### rpart (caret package http://topepo.github.io/caret/train-models-by-tag.html#Tree_Based_Model)
### CART - tuning parameters: cp (complexity parameter)
### Classification and Regression Trees or CART 
### a model specific variable importance metric is available


## WHat are the options for rf method? 

training51 <- select(training5, -c(35))
training52 <- cbind(training51, training$classe)
colnames(training52)[35] <- "classe"

modfit1c <-  train(classe ~., data = training52, method="rpart") ## Really fast
rattle::fancyRpartPlot(modfit1c$finalModel)
print(modfit1c$finalModel)
 

##  * denotes terminal node
## 1) root 19622 14042 A (0.28 0.19 0.17 0.16 0.18)  
## 2) pitch_forearm< -33.95 1578    10 A (0.99 0.0063 0 0 0) *
##   3) pitch_forearm>=-33.95 18044 14032 A (0.22 0.21 0.19 0.18 0.2)  
## 6) magnet_belt_y>=555.5 16579 12570 A (0.24 0.23 0.21 0.18 0.15)  
## 12) roll_dumbbell< 63.52522 11837  8379 A (0.29 0.19 0.26 0.13 0.13)  
## 24) roll_forearm< 122.5 7561  4475 A (0.41 0.18 0.2 0.11 0.099) *
##   25) roll_forearm>=122.5 4276  2654 C (0.087 0.19 0.38 0.17 0.18)  
## 50) magnet_dumbbell_z>=284.5 836   524 B (0.31 0.37 0.024 0.069 0.23) *
##   51) magnet_dumbbell_z< 284.5 3440  1838 C (0.034 0.15 0.47 0.19 0.16) *
##   13) roll_dumbbell>=63.52522 4742  3157 B (0.12 0.33 0.067 0.29 0.19)  
## 26) total_accel_dumbbell>=6.5 3046  1625 B (0.07 0.47 0.055 0.16 0.25) *
##   27) total_accel_dumbbell< 6.5 1696   805 D (0.2 0.097 0.09 0.53 0.088) *
##   7) magnet_belt_y< 555.5 1465   275 E (0.002 0.0027 0.0014 0.18 0.81) *


## Accuracy of this model is only about 50%
## But we have some great features -> pitch_forearm < -35 -> then it is an A
## And we get 81% E if magnet_belt_y > 556
## All the others are not so good

#### Can I use bagging (treebag) to get more trees?

#######################

modfit1d <-  train(classe ~., data = training2b, method="rpart")  ## Error, can't allocate vector of 11.4 Gb
rattle::fancyRpartPlot(modfit2b$finalModel)
print(modfit2b$finalModel)

#### Can I use bagging (treebag) to get more trees and better accuraacy
library(caret)
set.seed(5634)
trCtrl <- trainControl(method = "cv", number = 10) ## k-fold Cross Validation
modfitbag1 <- train(classe ~., data = training52, method="treebag", trControl=trCtrl, metric="Accurarcy") 
print(modfitbag1$finalModel)

## Accurarcy=98%!! 
## cv is cross validation

#### From the assignment
### You should create a report describing how you built your model, how you used cross validation, 
## what you think the expected out of sample error is, and why you made the choices you did. 


######### First get the test set with the same transformations as the training set
###############################################################################
library(dplyr)
testing2 <- testing[ , colSums(is.na(training)) <nrow(training)*0.5]
testing2b <-  select(testing2, -c(1,2,3,4,5,6,7))

testing2f <- testing2b[,-53] %>%
  (select_if(is.factor))

library(tidyverse)
testing2nf <- testing2b %>%
  select_if(negate(is.factor))

testing2fn <- lapply(testing2f, function(x) as.numeric(levels(x))[x])

## Combine these datasets to get a dataset with all numeric values
testing3 <- cbind(testing2nf, testing2fn)

testing3 <- testing2b

testing5 <- select(testing3, -c(8,9,10,13,22,23,25,35,38,48,1,2,3,18,28,29,31,46))

testing6 <- testing5 %>%
  select_if(is.factor)

testing7 <- testing5  %>%
  select_if(negate(is.factor))

testing8 <- lapply(testing6, function(x) as.numeric(levels(x))[x])

testing9 <- cbind(testing8, testing7)

str(testing2b)
str(training2b)
unique(training$classe)
unique(testing$problem_id)

################## Now we can predict for the 20 observations

PredictClassTest <- predict(modfitbag1, testing9)

results <- data.frame("Participant"=testing$user_name, "Problem_id"=testing$problem_id, 
                      "Class"=PredictClassTest)
print(results)

###########################

## What is the out of sample error? 
## Out of sample error =  RMSE = (not possible for a factor)

modfitbag1$rmse ## NULL
PredictClassTest$rmse ## Error

### I don't have a testset to calculate the out of sample error,
#### the test set does not contain any class information

###############################

#### Boosting

modfitboost1 <- train(class ~ ., method="gbm", data=training53, verbose=FALSE) ## error not a matrix
print(modfitboost1)

training53 <- as.matrix(training52)
str(training53)

##########

head(modfitbag1$pred)



str(testing5)
str(training51)


str(testing3)
str(training3)

str(testing5)
str(training5)

str(testing2b)
str(training2f)

str(testing2)

#### Perform feature selection based on this correlation? Keep 10?

highlycor <- which(PCA > 0.75, arr.ind=T)
write.csv(highlycor, "highcor.csv")

plot(training3$accel_belt_z,training3$total_accel_belt)

rbtab <- training3[,c("accel_belt_z", "total_accel_belt")]
prcombrbtab  <- prcomp(rbtab)
plot(prcombrbtab$x[,1],prcombrbtab$x[,2])
prcombrbtab$rotation

## Total dataset PCA on total dataset
typeColor <- ((training3$classe))
prComptotal <- prcomp(training3[,-53])
prComptotal$rotation
plot(prComptotal$x[,1],prComptotal$x[,2], col=typeColor)
print(prComptotal)

## Using caret doing PCA
preProc <- preProcess(training4[,-53], method = "pca", pcaComp = 20)
caretpca <- predict(preProc, training4[,-53])
plot(caretpca[,1], caretpca[,2], col= typeColor)

modelFit1 <- train(training4$classe ~., method="cforest", data = caretpca) ## Werkt niet

## Or do the above all at once using the caret package
testpca <- predict(caretpca, testing[,-53])
confusionmatrix(testing$classe, predict(modelFit1,testpca)) 

## Check the accuracy to see how well it had performed

## Fit linear model, warning messages

lm1 <- lm(classe ~ , data=training3) 
## Using the parameters of the PCA
lm1 <- lm(classe ~total_accel_belt + accel_belt_x  + accel_belt_y +accel_belt_z +magnet_belt_x +magnet_belt_y+magnet_belt_z+total_accel_arm+accel_arm_x + accel_arm_y + accel_arm_z +magnet_arm_x+magnet_arm_y+magnet_arm_z+total_accel_dumbbell+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x, data=training4)
lm2 <- lm(classe ~total_accel_belt + accel_belt_x  + accel_belt_y +accel_belt_z , data=training4)

summary(lm1)

modFitAll <- train(classe ~ ., data = training4, method="glm")

training4 <- training3


### Rank features by importance -> not possible for this dataset (https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
modelimp <- train(classe~., data=training3, method="lvq", preProcess="scale", trControl=control)
modelimp <- train(classe~total_accel_belt + accel_belt_x  + accel_belt_y +accel_belt_z, data=training3, method="lvq", preProcess="scale", trControl=control)

importance <- varImp(model, scale=FALSE)
plot(importance)

## Remove redundant features

correlationMatrix <- cor(training3[,1:52])
print(correlationMatrix)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
print(highlyCorrelated)

## Generally, you want to remove attributes with an absolute correlation of 0.75 or higher.

## Automatic feature selection using RFE (https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/)
library(mlbench)
library(caret)
control2 <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(training3[,1:52], training3[,53], sizes=c(1:52), rfeControl=control2)

print(results)
predictors(results)
plot(results, type=c("g", "o"))

# summarize the results


### Trees
modFittree <- train(class ~ ., method="rpart", data=training2f)

training4 <- training3
training4$classe <- as.numeric(training4$classe)
unique(training4$classe)
str(training4)
class(training4)