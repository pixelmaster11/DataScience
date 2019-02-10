#Useful for knowing missing data
install.packages("Amelia")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)

#Goal: Predict Survival of passengers onboard titanic (Current accuracy 76.67%)
#Data: https://www.kaggle.com/c/titanic/data

filepath <- "D:\\Git_DataScience_Projects\\DataScience\\Datasets"

titanic_train <- read.csv(paste(filepath , "Kaggle_Titanic\\Data\\train.csv", sep = "\\"))
titanic_test <- read.csv(paste(filepath, "Kaggle_Titanic\\Data\\test.csv", sep = "\\"))

#Lot of missing Age values
missmap(titanic_train, main = "Missing Map", col = c("Yellow", "Black"), legend = FALSE)


ggplot(titanic_train, aes(x = Pclass)) + geom_bar(aes(fill = factor(Pclass)))

ggplot(titanic_train, aes(x = Sex)) + geom_bar(aes(fill = factor(Sex)))

#177 na values removed
ggplot(titanic_train, aes(x = Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = "blue")

ggplot(titanic_train, aes(x = SibSp)) + geom_bar()

ggplot(titanic_train, aes(x = Parch)) + geom_bar()

ggplot(titanic_train, aes(x = Fare)) + geom_histogram(bins = 10, alpha = 0.5, fill = "green", color = "black")


#Plot Age according to Pclass to compute age based on class to fill na values in age
ggplot(titanic_train, aes(x = Pclass, y = Age)) + 
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.4) +
  scale_y_continuous(breaks = seq(min(0), max(80, by = 2))) + theme_bw()


#####
#Calculate avg ages for missing age values based on average age per class
#####
calc_age <- function(age , class) {
  
  out <- age
  
  for(i in 1:length(age)) {
    
    if(is.na(age[i])){
      
      if(class[i] == 1){
        
        out[i] <- 37
        
      } else if(class[i] == 2){
        
        out[i] <- 29
        
      } else{
        
        out[i] <- 24
      }
      
    } else {
      
      out[i] <- age[i]
    }
   
  }
  
  return (out)
}

fixed.ages <- calc_age(titanic_train$Age , titanic_train$Pclass) 
                    
titanic_train$Age <- fixed.ages

missmap(titanic_train, main = "Check", col = c("Yellow", "Black"), legend = F)

#Feature Engineering
#Grab Titles from name, Grab Cabin name letter, etc

titanic_train_mod <- select(titanic_train, -Cabin, -PassengerId, -Ticket, -Cabin, -Name)

titanic_train_mod$Survived <- factor(titanic_train_mod$Survived)
titanic_train_mod$Pclass <- factor(titanic_train_mod$Pclass)
titanic_train_mod$Parch <- factor(titanic_train_mod$Parch)
titanic_train_mod$SibSp <- factor(titanic_train_mod$SibSp)

str(titanic_train_mod)

################# Predict Survival ##################

#Generalized linear model
log.model <- glm(Survived ~ . , family = binomial(link = "logit"), data = titanic_train_mod)
summary(log.model)

#Lot of missing Age values
missmap(titanic_test, main = "Missing Map", col = c("Yellow", "Black"), legend = FALSE)


#Plot Age according to Pclass to compute age based on class to fill na values in age
ggplot(titanic_test, aes(x = Pclass, y = Age)) + 
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.4) +
  scale_y_continuous(breaks = seq(min(0), max(80, by = 2))) + theme_bw()


# class 1 <- 42 , class 2 <- 26 , class 3 <- 24
#####
#Calculate avg ages for missing age values based on average age per class
#####
calc_age_testset <- function(age , class) {
  
  out <- age
  
  for(i in 1:length(age)) {
    
    if(is.na(age[i])){
      
      if(class[i] == 1){
        
        out[i] <- 42
        
      } else if(class[i] == 2){
        
        out[i] <- 26
        
      } else{
        
        out[i] <- 24
      }
      
    } else {
      
      out[i] <- age[i]
    }
    
  }
  
  return (out)
}

fixed.ages <- calc_age_testset(titanic_test$Age, titanic_test$Pclass)
titanic_test$Age <- fixed.ages

#Get mean per passenger class for fares
ggplot(titanic_test, aes(x = Pclass, y = Fare)) + 
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 600, 10)) + theme_bw()

#Class 3 mean is 10 so assign that to the NA value
titanic_test[is.na(titanic_test$Fare), "Fare"] <- mean(titanic_test$Fare)

titanic_test_mod <- select(titanic_test, -Cabin, -PassengerId, -Ticket, -Cabin, -Name)


titanic_test_mod$Pclass <- factor(titanic_test_mod$Pclass)
titanic_test_mod$Parch <- factor(titanic_test_mod$Parch)
titanic_test_mod$SibSp <- factor(titanic_test_mod$SibSp)

#Remove 2 rows with 9 Parch values as it does not match with train dataset
titanic_test_mod[titanic_test_mod$Parch == 9, "Parch"] <- 6


fitted.probablities <- predict(log.model, titanic_test_mod, type = "response")
fitted.results <- ifelse(fitted.probablities > 0.5, 1 , 0)



submission <- cbind(titanic_test$PassengerId, fitted.results)

submission <- as.data.frame(submission)

colnames(submission) <- c("PassengerId", "Survived")

write.csv(submission, file = "mysub1.csv")




#######################
###
# 2. Submission
###
#######################

#######################
###
# Different Feature Engineering
###
#######################




titanic_train <- read.csv(paste(filepath , "titanic_train.csv", sep = "\\"))
titanic_test <- read.csv(paste(filepath, "titanic_test.csv", sep = "\\"))

#Lot of missing Age values
missmap(titanic_train, main = "Missing Map", col = c("Yellow", "Black"), legend = FALSE)


#######################
###
# FIX MISSING AGE VALUES BASED ON PCLASS AVERAGE AGE
###
#######################
fixed.ages <- calc_age(titanic_train$Age , titanic_train$Pclass) 
titanic_train$Age <- fixed.ages


#######################
###
# Age Groups
###
#######################



############################
###
#1. We can see from the following table, a higher preference was given to females overall when choosing to save a passenger
#2. Out of Total 1st class female passenger, only 3% were killed while 97% were saved / survived
#   As compared with that of total 1st class male passengers, 63% were killed while only 37% were saved / survived
#3. Similar trend is seen with both female and male 2nd class passengers
#4. However, an interesting stat is observed with 3rd class female passengers with only 50-50 chances of survival
#   This is interesting as it might indicate that 3rd class female passengers were not treated fairly as compared to that
#   of 1st and 2nd class female passengers who have death rate of only 3% and 7% respectively whereas for 
#   3rd class female passengers the death rate is extremely high of 50% 
###

titanic_train %>% group_by(Pclass, Sex, Survived) %>%  summarise(Total_Passengers = n()) %>% 
  mutate(Percent = Total_Passengers / sum(Total_Passengers) * 100)

#############################




#TODO: 20s, 30s, 40s, etc...

age_groups <- function(age){
  
  if(age < 20){
    
    return("Below 20")
    
  } else if(age >= 20 & age < 30){
    
    return("Twenties")
    
  } else if(age >= 30 & age < 40){
    
    return("Thirties")
    
  } else if(age >= 40 & age < 50){
    
    return("Fourties")
    
  } else if(age >= 50 & age <= 60){
    
    return("Fifties")
    
  } else {
    
    return("Above 60")
  }
  
}


#Lot of missing Age values
missmap(titanic_test, main = "Missing Map", col = c("Yellow", "Black"), legend = FALSE)

##Making same changes to test dataset
fixed.ages <- calc_age_testset(titanic_test$Age, titanic_test$Pclass)
titanic_test$Age <- fixed.ages



titanic_train$AgeGroup <- as.factor(sapply(titanic_train$Age, age_groups))
titanic_test$AgeGroup <- as.factor(sapply(titanic_test$Age, age_groups))

titanic_train <- mutate(titanic_train, FamilySize = SibSp + Parch)
titanic_test <- mutate(titanic_test, FamilySize = SibSp + Parch)

titanic_train$Survived <- factor(titanic_train$Survived)
titanic_train$Pclass <- factor(titanic_train$Pclass)
titanic_train$FamilySize <- factor(titanic_train$FamilySize)


titanic_test$Pclass <- factor(titanic_test$Pclass)
titanic_test$FamilySize <- factor(titanic_test$FamilySize)


titanic_train <- select(titanic_train, -SibSp, -Parch, -Name, -Age, -Ticket, -Cabin, -PassengerId)
titanic_test <- select(titanic_test, -SibSp, -Parch, -Name, -Age, -Ticket, -Cabin, -PassengerId)

log.model <- glm(Survived ~ . , family = binomial(link = "logit"), data = titanic_train)
summary(log.model)



#Null value of fare from test
titanic_test[is.na(titanic_test$Fare), "Fare"] <- 10


fitted.probablities <- predict(log.model, titanic_test, type = "response")
fitted.results <- ifelse(fitted.probablities > 0.5, 1 , 0)

temp <- read.csv(paste(filepath , "titanic_test.csv", sep = "\\"))

submission <- cbind(temp$PassengerId, fitted.results)

submission <- as.data.frame(submission)

colnames(submission) <- c("PassengerId", "Survived")

write.csv(submission, file = "mysub2.csv", row.names = F)

