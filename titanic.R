#titanic script
#data from https://www.kaggle.com/c/titanic/data
library(MASS)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(olsrr)


summary(train)
colSums(is.na(train))

train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)
train$SibSp <- factor(train$SibSp)
train$Parch <- factor(train$Parch)
train$Ticket <- factor(train$Ticket)
train$Embarked <- factor(train$Embarked)
train$Cabin <- factor(train$Cabin)

titanicmodel <- glm(Survived ~ Sex + Pclass + Ticket + SibSp + Parch + Ticket + Fare + Cabin + Embarked + Age, data = train, family = "binomial")
summary(titanicmodel)
titanicmodel1 <- glm(Survived ~ Age, data = train, family = "binomial")

step(titanicmodel, direction = "backward", k=2)
#Survived ~ Sex + Ticket + SibSp + Parch + Fare + Cabin + Age
step(titanicmodel1, direction = "forward", k=2)
#Survived ~ Sex + Pclass + Ticket + SibSp + Parch + Ticket + Fare + Cabin + Embarked + Age
step(titanicmodel, direction = "both", k=2)
#Survived ~ Sex + Ticket + SibSp + Parch + Fare + Cabin + Age
step(titanicmodel)
titanicmodeltrue <- glm(Survived ~ Sex + Ticket + SibSp + Parch + Fare + Cabin + Age, data = train, family = "binomial")
summary(titanicmodeltrue)

#survived ~ sex + sibsp + parch + fare + age, sex: male=1 female=0
#sex: if male, less survival
#sibsp and parch: more in family, less survival; might be due to the data having accounted family members
#fare: more expensive more survival
#age: younger more survival
#ticket: too many at the moment

plot(titanicmodeltrue)


ggplot(data = titanicmodeltrue, aes(x = Sex, fill = Survived, color = Survived)) + geom_bar()

help(step)







