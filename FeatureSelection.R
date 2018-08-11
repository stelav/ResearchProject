setwd("~/Project")
library("fscaret")
training <- read.csv("Training.csv", stringsAsFactors = T)
splitIndex <- createDataPartition(training$diabetic, p = .75, list = FALSE, times = 1)
trainDF <- training[ splitIndex,]
testDF  <- training[-splitIndex,]
fsModels <- c("glm", "gbm", "treebag", "rf", "neuralnet") 
myFS<-fscaret(trainDF, testDF, myTimeLimit = 40, preprocessData=TRUE,
Used.funcRegPred = fsModels, with.labels=TRUE,
supress.output=FALSE, no.cores=2)
myFS$VarImp
myFS$PPlabels