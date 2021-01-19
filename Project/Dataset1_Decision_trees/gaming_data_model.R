#trainig Decison tree
setwd('/home/rajesh/Desktop/DMML_project/Code/1/')
library("caTools")
library("FSelector")
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(data.tree)
library(ggplot2)
library(lattice)
library(doParallel)
library(foreach)
library(iterators)
library(parallel)
library(e1071)
library(caTools)
library(corrplot)
#after converting the cleaned data set after converting it into csv
datadf <- read.csv('/home/rajesh/cleaned_gaming.csv')
str(datadf)
#getting the necessary columns to train
datadf <- datadf[c(3,7,16,17,18)]
#converting it into a matrix to know the correlation and for heat map 
datadf_mat <- data.matrix(datadf)
heatmap(datadf_mat,scale = 'column')
corrplot(cor(datadf_mat),col='black',method="number")
str(datadf)
#split dataset
set.seed(123)
# to divide the data set to 80:20 ratio for training and testing
sample <- sample.split(datadf$SWL_T,SplitRatio = 0.8)
#assigning train and test set
train <- subset(datadf,sample==TRUE)
test <- subset(datadf,sample==FALSE)
#training
#dividing the load into 6 cluster to do parallel processing
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
#start time 
start.time<-proc.time() 

tree <- rpart(SWL_T ~.,data = train)
#ending
stop.time<-proc.time() 
#run time of the model
run.time<-stop.time -start.time 

print(run.time) 

stopCluster(cl) 
str(train)
tree.SWL_T.predicted <- predict(tree,test)
#converted to integer to get rid of decimal
tree.SWL_T.predicted <- round(tree.SWL_T.predicted)
tree.SWL_T.predicted
#creating a decision tree
prp(tree)
#calculating Accuracy
ACC <-100*sum(tree.SWL_T.predicted == test$SWL_T)/NROW(test)
ACC

