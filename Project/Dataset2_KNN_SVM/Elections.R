#set working directory
#loading libraries
library(Amelia)
setwd("/home/rajesh/Desktop/DMML_project/Datasets/2")
election_results <- read.csv("primary_results.csv")
county_facts <- read.csv("county_facts.csv")
#merge two data sets
merged <- merge(election_results,county_facts,by='fips')
#check the structure of data set
str(merged)
#plot of percentage of missing values
missmap(merged,main="missing in merged")
#finding the number of missing values in sapply
sapply(merged, function(x) sum(is.na(x)))
#converting dataframe to csv file
write.csv(merged,"/home/rajesh/merged_cleaned_election_result.csv")
merged_to_cleaning <- read.csv("merged_cleaned_election_result.csv") 
#removing unnecessary columns
merged_to_cleaning <- merged_to_cleaning[c(7:9,12,19:27)]

str(merged_to_cleaning)
merged_to_cleaning <- merged_to_cleaning[(merged_to_cleaning$candidate %in% c("Donald Trump","Hillary Clinton")),]
#converted for knn and svm
write.csv(merged_to_cleaning,"election.csv")
