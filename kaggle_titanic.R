
library(tidyverse)
library(ggplot2)

# Let's load the datas (test set is used later)
train_set <- read.csv("train.csv")
test_set <- read.csv("test.csv")

# Now we have a look at it
str(train_set)
summary(train_set) 

#Note: 
# - Survival rate in the train set is at 0.3838
# - Decimal age? (min at 0.42)



# Based on those metrics, let's look at survival rate of certain profils
## Kids and women first, huh?

# Based on sex ...
aggregate(Survived~Sex,data = train_set,mean)

# Based on age .. mmmh need more analysis
table(train_set$Age)
sum(is.na(train_set$Age)) #177 out of 1000 with NA age ...

train_set_age_adj <- train_set[!is.na(train_set$Age),]

ggplot(train_set_age_adj)+
  geom_bar(aes(Age),width = 2)

# Let's do grouping to analyse survival rate first, but i'll keep real age for model
train_set$Age_group <- cut(train_set$Age,breaks=seq(0,80,10))
aggregate(Survived~Age_group,data = train_set,mean)

# Crossing age and sex
group_by(train_set,Sex,Age_group) %>% 
  summarise(survival_Rate = mean(Survived),n=n())

# Now looking at Pclass (it has quite an impact)
summarise(group_by(train_set,Pclass),survival_Rate = mean(Survived),n=n())

# Now looking at Embarked (Port of embarkation: Cherbourg, Queenstown or Southampton)
summarise(group_by(train_set,Embarked),survival_Rate = mean(Survived),n=n())

# SibSp (# of siblings/spouse on the ship)
summarise(group_by(train_set,SibSp),survival_Rate = mean(Survived),n=n())

# parch (# of parents/children on the ship)
summarise(group_by(train_set,Parch),survival_Rate = mean(Survived),n=n())

# Crossing SibSp and Parch
summarise(group_by(train_set,Parch, SibSp),survival_Rate = mean(Survived),n=n())

###############################################
###############################################

# Let's try some small models to see first predictions before feature engineering
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived~Sex+Pclass+Parch,data=train_set,method = "class")

fancyRpartPlot(fit)

# This could be our first model, which does not seem too over fitted!!
Prediction <- predict(fit,test_set,type="class")
submit <- data.frame(PassengerId=test_set$PassengerId,Survived=Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)





