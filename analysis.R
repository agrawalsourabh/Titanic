# installing packages
install.packages("naniar") # for missing value
install.packages("wordcloud")
install.packages("RColorBrewer")\
install.packages("tm")
install.packages("SnowballC")

# loading packages
library(naniar)
library(caret)
library(ggplot2)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

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

# Name
title = getTitle(our.data$Name)

# add title column to our data
our.data$title = title

# convert to factor
our.data$title = as.factor(our.data$title)
table(our.data$title)

#AGE
str(our.data$Age)
summary(our.data$Age)

missing_age_title.df = aggregate(Age ~ title, data = our.data, function(x){
  sum(is.na(x))
}, na.action = NULL)

# calculate and add percentage column
missing_age_title.df$Perc = round(missing_age_title.df$Age / sum(is.na(our.data$Age)) * 100, 
                                  digits = 2)

missing_age_title.df = missing_age_title.df[which(missing_age_title.df$Age != 0), ]


