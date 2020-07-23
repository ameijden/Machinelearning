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
fitlm1 <- train(classe ~., data=training5, method="glm")

nas <- (is.na(training))
table(nas)

library(dplyr)

Aclass <- training %>%
  filter(classe=="A")


Aclass2 <- Aclass[,colSums(is.na(Aclass))<nrow(Aclass)*0.1]
## Remove columns with 10% NA's

training4 <- training[ , colSums(is.na(training)) == 0]
## Remove all columns with NA's

str(training4)

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

