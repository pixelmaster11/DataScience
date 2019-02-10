install.packages("corrgram")
install.packages("corrplot")
install.packages("caTools")

library(ggplot2)
library(dplyr)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)

#GOAL <- Predict G3 (final grade) score
#https://archive.ics.uci.edu/ml/datasets/Student+Performance


filepath <- "D:\\Tuts\\DataScience\\Datasets\\Student_Performance_UCI"

#Separated by semicolon ;
#Mathematics data set
student_data <- read.csv(paste(filepath, "student-mat.csv", sep = "\\"), sep = ";")

#No Null NA values
any(is.na(student_data))

#Check structure of variables to check if we need to change any into factors
str(student_data)

#Get numeric columns
numeric_cols <- sapply(student_data, is.numeric)

#Get the correlation of these numeric columns
correlation <- cor(student_data[, numeric_cols])

print(correlation)


#Plot the correlation data
#1. We see Grades G1, G2,, G3 are highly correlated which makes sense considering
#   if you get good or bad grades in 1st and 2nd term you will get good or bad final grade (G3)

#2. Similarly failures is also negatively correlated with Grades which makes perfect sense 

#3. Another interesting observation is that of Mother and Father's education are also highly correlated
# Which also kind of makes sense as mostly couples tend to have similar levels of education but not always true

#Only numeric cols supported
print(corrplot(corr = correlation, method = "color"))

#Plot using corrgram (supports the whole data)
print(corrgram(student_data))
print(corrgram(student_data, order = T, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt))


ggplot(student_data, aes(x = G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = "blue")


#As noted from correlation, G1 and G2 scores tend to increase with one another
ggplot(student_data, aes(x = G1, y = G2)) + geom_point() + geom_smooth(color = "red")  

#It seems students who had internet did slightly better in final grade as compared with those who didnt
ggplot(student_data, aes(x = internet, y = G3, fill = factor(health))) + 
  geom_bar(stat = "identity", position = "dodge") 
  
#It seems students who had romantic relationships performed worse than those who did not
#Health did not have any significant impact on the final student grades
ggplot(student_data, aes(x = romantic, y = G3, fill = factor(health))) + 
  geom_bar(stat = "identity", position = "dodge") 


#Male students had more romantic relationships than female students
ggplot(student_data, aes(x = romantic, y = G3, fill = factor(sex))) + 
  geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("blue", "black"))


 
#Linear Regression 
#(caTools)

#Set a Random Seed
set.seed(101)

#Split the dataset into random samples using caTools
sample <- sample.split(student_data$G3, SplitRatio = 0.66)

#Train dataset (66% of whole dataset)
#Test dataset (34% of whole dataset)
train <- subset(student_data, subset = sample == T)
test <- subset(student_data, subset = sample == F)


#Train and build model

#model <- lm(y ~ x1 + x2, data)

#Use all features
#Run Model
model <- lm(G3 ~ ., data = train)

#Interpret the model
summary(model)

#Get and plot residuals
res <- residuals(model)
class(res) 
#Change Numeric to dataframe for plotting
res <- as.data.frame(res)

#Plot residual histogram / good model should have close to normal distribution plot
#We have some -ve residual values meaning -ve test scores predicted by our model
ggplot(res, aes(res)) + geom_histogram(fill = "blue", alpha = 0.5)

#Residual plots
plot(model)


#PREDICTIONS G3 VALUES
G3.predictions <- predict(model, test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c("Predicted", "Actual")
results <- as.data.frame(results)

print(head(results))

#Remove negative values and make them 0 as we don't want negative test scores
to_zero <- function(x) {
  if(x < 0 ) {
    return(0)
  } else{
    return(x)
  }
}

#Remove max values and make them max as we don't want test scores greater than max
to_max <- function(x) {
  if(x > 20 ) {
    return(20)
  } else{
    return(x)
  }
}

#Remove -ve values
results$Predicted <- sapply(results$Predicted, to_zero)
#Remove > max values
results$Predicted <- sapply(results$Predicted, to_max)

#Mean Squared Error
mse <- mean((results$Actual - results$Predicted) ^ 2)

#Root Mean Squared Error
rmse <- sqrt(mse)

#Sum of Squared Errors
sse <- sum( (results$Predicted - results$Actual) ^ 2 )
sst <- sum( (mean(student_data$G3) - results$Actual) ^ 2)

Rsquared <- 1 - (sse / sst)







