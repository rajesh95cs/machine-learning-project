#importing libraries
library(Amelia)
library(outliers)
#setting working environment
setwd("/home/rajesh/Desktop/DMML_project/Datasets/1/")
#reading data and saving it into a data frame
gaming_data <- read.csv("GamingStudy_data.csv")
#getting the structure of the data frame
str(gaming_data)
#plotting missing percentage
missmap(gaming_data,main = "missing in gaming data")
#computing the total missing values in each column of the dataframe
sapply(gaming_data, function(x) sum(is.na(x)))
#removing unnecessary columns
gaming_data <- gaming_data[c(1:2,10,16:20,23,41:49,51:55)]
str(gaming_data)
#omitting the rows with null values
gaming_data <- na.omit(gaming_data)
#again removing unnecessary columns
gaming_data <- gaming_data[-c(1,2,3,22,23)]
length(unique(gaming_data))
#adding column to the data frame, creating categorical data from 
#the existing data in the dataframe
values <- c()
for(value in gaming_data$SWL_T) {
  if(value >= 5 && value <= 9){
    values <- append(values,"Extremely Disatisfied")
  }
  else if (value >= 10 && value <= 14 ) {
    values <-append(values,"Disatisfied")
  }
  else if (value >= 15 && value <= 19 ) {
    values <-append(values,"Slightly Disatisfied")
  }
  else if (value == 20) {
    values <-append(values,"Neutral")
  }
  else if (value >= 21 && value <= 25 ) {
    values <-append(values,"Slightly Satisfied")
    
  }
  else if (value >= 26 && value <= 30 ) {
    values <-append(values,"Satisfied")
  }
  else if (value >= 31 && value <= 35 ) {
    values <-append(values,"Extremely Satisfied")
  }
}
values
gaming_data$SWL_summary <- values
length(values)
length(gaming_data$SWL_summary)
unique(gaming_data$SWL_summary)
min(gaming_data$GAD_T)
max(gaming_data$GAD_T)
GAD_values <- c()
#setting again values for GAD as categorical
for(value in gaming_data$GAD_T) {
  if(value >= 0 && value <= 4){
    GAD_values <- append(GAD_values,"Normal")
  }
  else if (value >= 5 && value <= 9 ) {
    GAD_values <-append(GAD_values,"Mild Anxiety")
  }
  else if (value >= 10 && value <= 14 ) {
    GAD_values <-append(GAD_values,"Moderate Anxiety")
  }
  else if (value >= 15 && value <= 21 ) {
    GAD_values <-append(GAD_values,"Severe Anxiety")
    
  }
}
gaming_data$GAD_summary <- GAD_values
min(gaming_data$SPIN_T)
max(gaming_data$SPIN_T)
SPIN_values <- c()
#adding column for SPIN_T as categorical
for(value in gaming_data$SPIN_T) {
  if(value >= 0 && value <= 18){
    SPIN_values <- append(SPIN_values,"Normal")
  }
  else if (value >= 19 && value <= 30 ) {
    SPIN_values <-append(SPIN_values,"Mild Social Phobia")
  }
  else if (value >= 31 && value <= 40 ) {
    SPIN_values <-append(SPIN_values,"Moderate Social Phobia")
  }
  else if (value >= 41 && value <= 49 ) {
    SPIN_values <-append(SPIN_values,"Severe Social Phobia")
  }
  else  if (value >= 50 ){
    SPIN_values <- append(SPIN_values,"Very Severe Social Phobia")
  }
}  

gaming_data$SPIN_summary <- SPIN_values
boxplot(gaming_data$Hours)
summary(gaming_data$Hours)
#handling outliers counting the record with outlier values by substitutng each column
#and repeating the steps until the outliers are removed
count <- 0
for (value in gaming_data$Hours) {
  if (value>46) {
    count <- count + 1
    
  }
}
count
str(gaming_data)
summary(gaming_data$Hours)
#getting inter quartile range
IOR <- 28 - 12
upper_wisker <- 52 + 1.5*IOR
head(gaming_data,10)
IQR(gaming_data$Hours)
outlier(gaming_data$Hours)
boxplot(gaming_data$Hours)
#getting values with no outliers
gaming_data <- subset(gaming_data,gaming_data$Hours <= 44)
max(gaming_data$Hours)
hour_values <- c()
summary(gaming_data$Age)
IQR(gaming_data$Age)
gaming_data$Per_Day_Playing_time <- hour_values
gaming_data$Per_Day_Playing_time <- as.factor(gaming_data$Per_Day_Playing_time)
# writing the cleaned dataset to a file for training
writexl::write_xlsx(gaming_data,"/home/rajesh/cleaned_gaming_data.xlsx")

