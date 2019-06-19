# installing packages
install.packages("naniar") # for missing value

# loading packages
library(naniar)
library(caret)
library(ggplot2)

# Importing data
our.test = read.csv("test.csv")
our.train = read.csv("train.csv")

str(our.test)
str(our.train)

# adding a new column survived to our test data
our.test$Survived = NA

# Combining our test and train data
our.data = rbind(our.train, our.test)

# Check missing value in our data set
missing.df = missing_val(our.data)


# Survived
str(our.data$Survived)
our.data$Survived = as.factor(our.data$Survived)

# PClass
str(our.data$Pclass)

# Check unique value of PClass
unique(our.data$Pclass)

# convert it into factor
our.data$Pclass = as.factor(our.data$Pclass)
our.train$Pclass = as.factor(our.train$Pclass)

# SEX
str(our.data$Sex)
