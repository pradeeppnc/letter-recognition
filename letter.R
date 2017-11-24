#Load the dataset
letters = read.csv(choose.files())

#add a new variable "isB" to the dataset  
letters$isB = as.factor(letters$letter == "B")

#Split the data into train and test with splitratio as .5
library("caTools")
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = .5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

#Baseline accuracy of the original dataset
table(letters$isB)
2350/(2350+766)

#Create a Cart model and make prediction in testing set for dependant variable "isB"
library(rpart)
library(rpart.plot)
Model1 = rpart(isB ~ . -letter, data = train, method = "class")
prp(Model1)
PredictCart = predict(Model1, newdata = test, type = "class")
table(test$isB, PredictCart)
(1118+340)/(1118+340+43+57)

#Create a randomForest model and make prediction in testing set for dependant variable "isB"
set.seed(1000)
library("randomForest")
Model2 = randomForest(isB ~ . -letter, data = train)
PredictForest = predict(Model2, newdata = test)
table(test$isB, PredictForest)
(1165+374)/(1165+374+10+9)

#Convert letter variable to factor
letters$letter = as.factor( letters$letter )

#Split the data into Train and Test with splitratio as .5
set.seed(2000)
spll = sample.split(letters$letter, SplitRatio = .5)
Train = subset(letters, spll == TRUE)
Test = subset(letters, spll == FALSE)

#Baseline accuracy of the original dataset
table(Test$letter)
(401)/(395+383+401+379)

#Create a Cart model and make prediction in testing set for dependant variable "letter"
library(rpart)
library(rpart.plot)
Model3 = rpart(letter ~ . -isB, data = Train, method = "class")
prp(Model3)
PredictCart2 = predict(Model3, newdata = Test, type = "class")
table(Test$letter, PredictCart2)
(377+299+367+333)/(377+299+367+333+4+14+7+11+66+7+25+2+9+36+1)

#Create a randomForest model and make prediction in testing set for dependant variable "letter"
set.seed(1000)
library("randomForest")
Model4 = randomForest(letter ~ . -isB, data = Train)
PredictForest2 = predict(Model4, newdata = Test)
table(Test$letter, PredictForest2)
(394+375+395+363)/nrow(Test)
