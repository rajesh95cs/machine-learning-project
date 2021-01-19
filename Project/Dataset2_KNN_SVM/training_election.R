#loading libraries
library(tidyr)
library("ggplot2")
library(caTools)
library(class)
library(gmodels)
library(corrplot)
library(caret)
#reading the cleaned data set
election <- read.csv("/home/rajesh/Desktop/DMML_project/Datasets/2/election.csv")
election <- election[-1]
election$candidate <- as.factor(election$candidate)
#normalizing the values in the dataset
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
election[2:13] <- as.data.frame(lapply(election[2:13], normalize))
str(election)
#converting the data set to a matrix for heat map and correlation
election_mat <- data.matrix(election)
#plotting heat map and correlation matrix
heatmap(election_mat,scale = 'column')
corrplot(cor(election_mat),col='black',method="number")
str(election)
election$candidate <- as.numeric(election$candidate)
#sampling the data sets for training and testing
train_election<-election[1:2500, ]
test_election <- election[2501:5509, ]
#assigning dependent variables
election_train_labels <- election[1:2500, 1]
election_test_labels <- election[2501:5509, 1]
#Training the knn model
knnmodel <- knn(train = train_election,test = test_election,cl = election_train_labels,k = 65 )
#calculating the accuracy
ACC <- 100*sum(election_test_labels == knnmodel)/NROW(election_test_labels)
ACC
#getting confusion matrix
confusionMatrix(table(knnmodel,election_test_labels))
                      