#random forest 
library(caTools)
library(randomForest)
library(outliers)
library(caret)
genre_df <- genre_data[-c(1)]

str(genre_df)

boxplot(genre_df)
#handling outliers for each columns separately by typing each column and 
#rerunning the code till the outliers are removed
#run this after reading the csv file
outliers <- boxplot(genre_df$,plot = FALSE)$out
genre_df[genre_df$Tempo %in% outliers,'Tempo'] <- NA
genre_df <- na.omit(genre_df)

boxplot(genre_df$Tempo)  
boxplot(genre_df)
genre_df <- genre_df
genre_data <- genre_df
write.csv(genre_df,'/home/rajesh/cleaned_genre.csv')
genre_data <- read.csv('/home/rajesh/cleaned_genre.csv')
str(genre_data)
genre_data <- genre_data[c(2,4,7,8,9)]

summary(genre_data$Energy)
#adding an extra column from the existing data in the data set
values <- c()
for(value in genre_data$Energy) {
  if(value > 0.0 && value < 0.479250 ){
    values <- append(values,"very Low Energy")
  }
  else if (value >= 0.479250 && value < 0.65700 ) {
    values <-append(values,"Low Energy")
  }
  else if (value >= 0.65700 && value < 0.8100) {
    values <-append(values,"High Energy")
  }
  else if (value >= 0.81000 && value < 1.00 ) {
    values <-append(values,"Very High Energy")
    
  }
}
length(values)

genre_data <- genre_data[c(1:62443),]
genre_data$Energy_summary <- values
genre_data <- genre_data[-1]
#normalize the data 
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
genre_data[1:4] <- as.data.frame(lapply(genre_data[1:4], normalize))
str(genre_data)
#train and Test
train <- genre_data[1:10000,]
test <- genre_data[10001:15000,]

#modeling the the training data
train$Energy_summary <- as.factor(train$Energy_summary)
test$Energy_summary <- as.factor(test$Energy_summary)
randfmodel <- randomForest(Energy_summary ~.,data = train, ntree = 2001,importance = TRUE)
#plotting graph
plot(randfmodel)

result <- data.frame(test$Energy,predict(randfmodel,test))
test_predict <- predict(randfmodel,test)


#Accuracy of the model
confusionMatrix(test_predict,test$Energy_summary)
