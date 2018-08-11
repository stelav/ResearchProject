setwd("~/Project")
library(neuralnet)
library(NeuralNetTools)
library(caTools)
library(pROC)
set.seed(1)
data <- read.csv("ANNTraining.csv", stringsAsFactors = T)
ind<-sample.split(Y=data$Diabetic.Patient, SplitRatio = 0.7)
diabetesTrain<- data[!ind,]
diabetesTest<- data[ind,]
nn = neuralnet(diabetesTrain$Diabetic.Patient~Age+Sex+X2.12+X2.5+X2.2+X1.6+X1.9+X1.7+X2.1, data = diabetesTrain, hidden =c(4), threshold=0.01,err.fct ="ce"
, linear.output = FALSE, stepmax = 1e6)
nn$net.result[[1]]
nn1 <- ifelse(nn$net.result[[1]]>0.5,1,0)
misClassifictionError = mean(diabetesTrain$Diabetic.Patient != nn1)
misClassifictionError
OutPutVsPred = cbind(diabetesTrain$Diabetic.Patient,nn1)
OutPutVsPred
cm <- as.matrix(table(diabetesTrain$Diabetic.Patient, nn1))
cm
plot(roc(as.numeric(diabetesTrain$Diabetic.Patient), as.numeric(nn1)), main = "Neural Network ROC Curve", ylab = "Recall", xlab = "False Positve Rate", col="dark blue", lty=1)
aucNN<-auc(as.numeric(diabetesTrain$Diabetic.Patient), as.numeric(nn1))
aucNN
saveRDS(nn, file = "modelNN.rds")