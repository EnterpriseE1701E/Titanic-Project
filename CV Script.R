library(caret)
library(C50)
library(irr)
library(party)
library(wSVM)
set.seed(1701)
folds<-createFolds(FinalTrain$Survived, k=11)

myFunction<-function(x)
  {
  STrain<-FinalTrain[-x,]
  STest<-FinalTrain[x,]
  SModel<-cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID+CabinNumberFinal+CabinLetterFinal,data = STrain, controls=cforest_unbiased(ntree=2540, mtry=3))
  SPrediction<-predict(SModel, STest, OOB=TRUE, type = "response")
  ErrorRate<-Error.rate(SPrediction, STest$Survived)
  return(1-ErrorRate)
}
cv_results<-lapply(folds, myFunction)
cv_results
