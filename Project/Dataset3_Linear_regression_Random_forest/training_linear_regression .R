#loading packages
library(caTools)
library(hydroGOF)

genre_data <- read.csv('/home/rajesh/cleaned_genre.csv')
genre_df <- genre_data[-c(1)]
str(genre_df)
#filtering out necessary columns
genre_df <- genre_df[c(1,3,6,7,8)]

#normalizing the values
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
genre_df <- as.data.frame(lapply(genre_df, normalize))

str(genre_df)
sample <- sample.split(genre_df,SplitRatio = 0.7)
#train and Test
train <- subset(genre_df,sample == TRUE)
test <- subset(genre_df,sample==FALSE)
#modeling the the training data
Model <- lm(Energy~.,data = train)
pred_Energy <- predict(Model,test)

#fit
plot(pred_Energy,type = 'l',lty=1.8,col="blue")
plot(test$Energy, type='l',lty=1.8,col="red")
lines(pred_Energy,type = 'l',col="orange")
plot(x = test$Energy,y=pred_Energy,col ='red')
abline(Model,col = 'blue')

#finding accuracy
rmse(pred_Energy,test$Energy)

#to get the summary of the model
summary(Model)

