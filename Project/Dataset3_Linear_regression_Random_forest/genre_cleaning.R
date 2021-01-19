#setting environment
setwd("/home/rajesh/Desktop/DMML_project/Datasets/3/")

#loading Libraries
library(Amelia)
library(corrplot)

#loading dataset
genre_data <- read.table(file='songDb.tsv', sep = '\t', header = TRUE, fill = TRUE)

str(genre_data)

#plotting missing percentage
missmap(genre_data,main = "missing in genre dataset")

#confirming for missing values 
sapply(genre_data, function(x) sum(is.na(x)))

#ommitting null values if any
genre_data <- na.omit(genre_data)

#removing unneccesaary columns
genre_data <- genre_data[-c(13:17)]

#boxplot
boxplot(genre_data[c(2:11,13)])

#heat map and correlation plot
genre_data_mat <- data.matrix(genre_data)
heatmap(genre_data_mat[1:10000,],scale = 'column')
corrplot(cor(genre_data_mat),method = 'pie')

#after handling the outliers
genre_data <- genre_data[-c(13)]
genre_data$Genre <- as.factor(genre_data$Genre)
genre_data$time_signature <- as.factor(genre_data$time_signature)
genre_data$Tempo <- as.numeric(genre_data$Tempo)

#writing it to a csv file
write.csv(genre_data,"/home/rajesh/genre_cleaned_data.csv")


