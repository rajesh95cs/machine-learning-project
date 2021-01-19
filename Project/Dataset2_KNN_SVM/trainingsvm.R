#loading the packages
library(e1071)
library(caret)
library(kernlab)

#election svm train model 
# loading the cleaned dataset
election_svm <- read.csv("/home/rajesh/Desktop/DMML_project/Datasets/2/election.csv")
election_svm$candidate <- as.factor(election_svm$candidate)
election_svm <- election_svm[-1]
str(election_svm)
test_election$candidate <- as.numeric(test_election$candidate)
#normalizing the values in the dataset
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
election_svm[2:13] <- as.data.frame(lapply(election_svm[2:13], normalize))
#splitting the data set as test and train
train_election <- election_svm[1:4000, ]
test_election <- election_svm[4001:5509, ]
#training model
svm_model <- svm(candidate~.,
                 data = train_election,
                 type ='C-classification',
                 kernel = 'linear',
                 scale = FALSE)
#get summary of the model
summary(svm_model)
#show support vectors
test_predict <- predict(svm_model,newdata = test_election)
test_predict
#confusion matrix Printed
confusionMatrix(table(test_predict,test_election$candidate))
str(train_election)
#tune grid for changing the ways to tune the learning
grid <- expand.grid(C = c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_model_linear_grid <- train(candidate~.,
                               data = train_election,
                               method = 'svmLinear',
                               preProcess = c("center","scale"),
                               tuneGrid = grid,
                               tuneLength = 10)

svm_model_linear_grid
#plotting the variation in accuracy with respect to tuning grid
plot(svm_model_linear_grid)

test_predict_grid <- predict(svm_model_linear_grid,newdata = test_election)
#confusion matrix for tuned model
confusionMatrix(table(test_predict,test_election$candidate))
