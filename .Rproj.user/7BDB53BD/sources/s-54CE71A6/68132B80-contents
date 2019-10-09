library("readr")
library(ggplot2)
cars <- read.csv("~/Desktop/UBIQUM/Data Analytics 2/Get Started with R/R Tutorial Data Sets/cars.csv")

# Preprocessing Data
names(cars)<-c("Brand","Speed","Distance")

# Data disribution plots
plot(cars$Speed,cars$Distance,main = "Scatterplot")
cor(cars$Speed,cars$Distance)
qqnorm(cars$Distance)
qqline(cars$Distance)
plot(cars$Distance,cars$Brand)

# % of Data partition for the Model
trainSize<-round(nrow(cars)*0.7)
print(trainSize)
testSize<-nrow(cars)-trainSize
print(testSize)

# creating the test sets
set.seed(123)
training_indices<- sample(seq_len(nrow(cars)),size =trainSize)
print(training_indices)
trainSet<- cars[training_indices, ]
testSet<- cars[-training_indices, ]
print(testSet)

# Linear Regression Model
LRM <- lm(Distance ~ Speed, trainSet)
summary(LRM)
abline(LRM)

# Looking for Outliers
boxplot(cars, main = "All Data Set Boxplot")
boxplot(cars$Distance, main = "Distance Boxplot")
boxplot(cars$Speed, main = "Speed Boxplot")
boxplot(cars$Distance,cars$Speed, main = "Distance&Speed Boxplot")
identify(cars$Brand,cars$Distance)

# Discarding Outliers


# Accessing regression objects
names(LRM)
LRM$fitted
plot(cars$Distance,LRM$fitted)

# Prediction







