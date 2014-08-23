
require(caret)

training <- read.csv('pml-training.csv',na.strings = c("NA",""))
testing <- read.csv('pml-testing.csv',na.strings = c("NA",""))

# Remove bad variables
keep <- colSums(training !=0) != 0
keep <- keep[keep == TRUE]

training2<- training[,which(!is.na(names(keep)))]
testing <- testing[,which(!is.na(names(keep)))]

# also remove variables with low variance
nsv <- nearZeroVar(training2,saveMetrics=TRUE)

# Remove cloumns with low frequency in variation
training2 <- training2[,which(nsv$freqRatio > 1.1)]
testing <- testing[,which(nsv$freqRatio > 1.1)]

# Remove columns that are just timestamps
testing <- testing[,-c(1,3,4,5,6)]
training2 <- training2[,-c(1,3,4,5,6)]

#turn names into variables:
#levels(training2$user_name) <- 1:length(levels(training2$user_name))
#levels(testing$user_name) <- 1:length(levels(testing$user_name))

#Get numeric columns for data
numerics <- sapply(training2, is.numeric)

# Split training into sub-training
inTrain= createDataPartition(y=training2$classe,p=0.75,list=FALSE)

training3 <- training2[inTrain,]
subTrain <- training2[-inTrain,]


#preProc <- preProcess(training3[,which(numerics==TRUE)],method="pca")
#trainPC <- predict(preProc,training3[,which(numerics==TRUE)])
modelFit <- train(classe ~ .,method="rf",data=training3)

#Prediction of inner test Set
testPC <- predict(modelFit,subTrain)

# Accuracy of result set
dazed <- confusionMatrix(subTrain$classe,testPC)
testAccuracy = dazed$overall['Accuracy']

#Prediction on test set
testTC <- predict(modelFit,testing)


#citation

#Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.


