Machine Learning Project
========================================================

This is an R Markdown document. Or is it?

The purpose of this porject is to develop a model for predicting a type of activity through supervised learning based on aquired data.

The subject in question is of multiple test subjects performing exercises with gyroscopes and accelerametor sensors attached to them. There are 5 exercises labled A to E that describe the exercise they performed. Using this data it should be possible to predict the exercise type based soley on the data.

However, none of this matters! By using feature selection and random forests, we can build a model that predicts the type of activity without knowing much about the data at all. As long as we have an accuracy of 95% or more, we can be confident that the algorithm works.

First we load the data. We are given a testing set of 19622 rows and a testing set of 20 rows. We will eventually split the training set up into another training and testing set in order to validate our model before applying it.


```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```


```r
training <- read.csv('pml-training.csv',na.strings = c("NA",""))
testing <- read.csv('pml-testing.csv',na.strings = c("NA",""))
```

Next, we need to remove columns where we didn't receive any data. Remember that any transformation we make to the training set, we have to make to the testing set.


```r
keep <- colSums(training !=0) != 0
keep <- keep[keep == TRUE]

training2<- training[,which(!is.na(names(keep)))]
testing <- testing[,which(!is.na(names(keep)))]
```

Then we remove columns with low variance. If a column has little variance, then it is a bad predictor since every activity type will have similar values.


```r
nsv <- nearZeroVar(training2,saveMetrics=TRUE)

training2 <- training2[,which(nsv$freqRatio > 1.1)]
testing <- testing[,which(nsv$freqRatio > 1.1)]
```

There are also columns that relate to timestamps that don't make any sense to use.


```r
testing <- testing[,-c(1,3,4,5,6)]
training2 <- training2[,-c(1,3,4,5,6)]
```

Since we want to know how well our algorithm is performing, we split the training data up again:


```r
inTrain= createDataPartition(y=training2$classe,p=0.75,list=FALSE)

training3 <- training2[inTrain,]
subTrain <- training2[-inTrain,]
```

Finally, we run a random forest algorithm on the training3 dataset to build a model using random forests


```r
modelFit <- train(classe ~ .,method="rf",data=training3)
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

We can then run a prediction on the subTrain dataset using this model and compare the results


```r
testPC <- predict(modelFit,subTrain)

dazed <- confusionMatrix(subTrain$classe,testPC)
testAccuracy = dazed$overall['Accuracy']
```

We find that we have an accuracy of 0.9802 ! That's better than our accepted accuracy of 95%.

All that's left to do is run the prediction on our test set:

```r
testTC <- predict(modelFit,testing)
```

And the answers are:
B, A, B, A, A, E, D, B, A, A, B, C, B, A, E, E, A, B, B, B

