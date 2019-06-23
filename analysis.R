# installing packages
install.packages("naniar") # for missing value
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
install.packages("SnowballC")
install.packages("data.table")

# loading packages
library(naniar)
library(caret)
library(ggplot2)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(dplyr)
library(data.table)
library(scales)
library(caret)
library(naivebayes)
library(randomForest)
library(class)


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


# Mean value of each title
mean_age_title = aggregate(Age ~ title, data = our.data, function(x){
  mean(x)
}) 

mean_age_title$title = as.character(mean_age_title$title)

# Imputing missing value - AGE

# Title == " Mr"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Mr'), 
                      mean_age_title[which(mean_age_title$title == ' Mr'), 2], 
                      our.data$Age)

# Title == " Miss"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Miss'), 
                      mean_age_title[which(mean_age_title$title == ' Miss'), 2], 
                      our.data$Age)

# Title == " Mrs"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Mrs'), 
                      mean_age_title[which(mean_age_title$title == ' Mrs'), 2], 
                      our.data$Age)

# Title == " Master"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Master'), 
                      mean_age_title[which(mean_age_title$title == ' Master'), 2], 
                      our.data$Age)

# Title == " Ms"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Ms'), 
                      mean_age_title[which(mean_age_title$title == ' Ms'), 2], 
                      our.data$Age)

# Title == " Dr"
our.data$Age = ifelse((is.na(our.data$Age) & our.data$title == ' Dr'), 
                      mean_age_title[which(mean_age_title$title == ' Dr'), 2], 
                      our.data$Age)

# Sibsp and Parch
# Make them one column family size - (1 + sibsp + parch)

our.data$family.size = 1 + our.data$SibSp + our.data$Parch

# family size
unique(our.data$family.size)

# convert them to categorical variable
our.data$family.size = as.factor(our.data$family.size)

# Survival rate as per family size
sur_rate_f.s = aggregate(Survived ~ family.size, data = our.data, FUN = function(x){
  count(x)
})

family.size_survived_ft = xtabs(~ Survived + family.size, our.data[-(419:1309), ])
family.size_survived_ft.mat = as.matrix(family.size_survived_ft)

family.size_survived_ft.df = data.frame(family.size = levels(our.data$family.size), 
                                        total.passenger = family.size_survived_ft.mat[1, ] + family.size_survived_ft.mat[2, ],
                                        non.survival = family.size_survived_ft.mat[1, ] )  
family.size_survived_ft.df$ns.perc = round(family.size_survived_ft.df$non.survival / family.size_survived_ft.df$total.passenger * 100, 
                                        digits = 2)
# 
# aggregate(cbind(count = (Survived == 0)) ~ family.size, 
#           data = our.data[-(419:1309), ], 
#           FUN = function(x){NROW(x)})

# aggregate(our.data$Survived, by=list(our.data$family.size, our.data$Survived),count)


# Embarked
str(our.data$Embarked)
levels(our.data$Embarked)

#
#   WORKPLACE SAVED
#

# our.data$Embarked = as.character(our.data$Embarked)
x = ifelse(our.data$Embarked == "", "S", our.data$Embarked)
our.data$Embarked = as.factor(x)
table(our.data$Embarked)

# Survival Rate as per Embarked
embarked_survived.ft = xtabs(~Survived + Embarked, data = our.data[-(419:1309), ])
embarked_survived.mat = as.matrix(embarked_survived.ft)
embarked_survived.ft.df = data.frame(Embarked = levels(our.data$Embarked), 
                                     Total.Passenger = embarked_survived.mat[1, ] + embarked_survived.mat[2, ], 
                                     Non.Survival = embarked_survived.mat[1, ])

embarked_survived.ft.df$NS.Perc = round(embarked_survived.ft.df$Non.Survival / embarked_survived.ft.df$Total.Passenger * 100, 
                                        digits = 2)


# Dropping columns - Name, Ticket, Fare and Cabin
our.mod.data = our.data[, -c(4, 7, 8, 9, 10, 11, 13)]
colnames(our.mod.data)

# Dividing the our.mod.data to training data and testing data
our.mod.training = our.mod.data[1:891, ]
our.mod.testing = our.mod.data[-c(1:891), ]

# Sampling the our.mod.training data to trd and tsd
index = createDataPartition(our.mod.training$Survived, times = 1, 
                            p = 0.7, list = F)
trd = our.mod.training[index, ]
tsd = our.mod.training[-index, ]

#
# ================== NAIVE BAYES ===================
#
# Creating classifier - Naive Bayes
classifier_nb = naive_bayes(formula = Survived ~., 
                            data = trd[, -1])


# Predicting tsd value
our.pred_nb = predict(classifier_nb, newdata = tsd[, -1], type = "class")

cm_nb = confusionMatrix(our.pred_nb, tsd$Survived)
cm_nb

#
# ================== RANDOM FOREST ===================
#
# Creating classifier - random forest
classifier_rf = randomForest(formula = Survived ~., 
                            data = trd[, -1], ntree = 20)


# Predicting tsd value
our.pred_rf = predict(classifier_rf, newdata = tsd[, -1], type = "class")

cm_rf = confusionMatrix(our.pred_rf, tsd$Survived)
cm_rf


# training model to our.mod.training set - Naive Bayes
classifier_nb_full = naive_bayes(formula = Survived ~., data = our.mod.training[, -1])

# training model to our.mod.training set - Random Forest
classifier_rf_full = randomForest(formula = Survived ~., data = our.mod.training[, -1], ntree = 20)

# predicting - Naive Bayes
our.pred = predict(classifier_nb_full, newdata = our.mod.testing[, -1], type = "class")
our.pred_nb = predict(classifier_nb, newdata = our.mod.testing[, -1], type = "class")

# predicting - random forest
our.pred_rf = predict(classifier_rf, newdata = our.mod.testing[, -1], type = "class")
our.pred_rf_full = predict(classifier_rf_full, newdata = our.mod.testing[, -1], type = "class")

our.submission = data.frame(PassengerId = our.mod.testing[,1], Survived = our.pred_nb)

write.csv(our.submission, "data/submission_nb.csv", row.names = FALSE)


#
#  ====================== WORKPLACE SAVED ================================================
#

