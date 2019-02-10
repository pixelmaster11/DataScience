library(ggplot2)
library(ggfortify)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)
library(ggthemes)

theme_update(plot.title = element_text(hjust = 0.5))


#DATA: https://www.kaggle.com/spscientist/students-performance-in-exams

filepath <- "D:\\Git_DataScience_Projects\\DataScience\\Datasets"

student_performance <- read.csv(paste(filepath, "Kaggle_Students_performance_in_exams\\Data\\StudentsPerformance.csv", sep = "\\"))

df <- student_performance %>% group_by(race.ethnicity) %>% summarise(Total_Students = n())

#Number of students per ethnic group
#Group C has highest number of students followed by group D, B, E and A
df %>%
ggplot(aes(x = race.ethnicity, y = Total_Students, fill = race.ethnicity)) + geom_bar(stat = "identity") +
scale_y_continuous(name = "Total Students", limits = c(0,350), breaks = seq(0,350,40)) + 
scale_x_discrete(name = "Ethnic Groups") +
geom_text(aes(label = Total_Students), vjust = -0.3) 

#Percent of students in each ethnic group
df %>% mutate(Percentage = Total_Students * 100 / sum(Total_Students)) %>% 
ggplot(aes(x = race.ethnicity, y = Percentage, fill = race.ethnicity)) + geom_bar(stat = "identity") +
scale_y_continuous(limits = c(0,40), breaks = seq(0,40, 10), labels = percent_format(scale = 1)) + 
scale_x_discrete(name = "Ethnic Groups") +
geom_text(aes(label = paste(Percentage, "%")), vjust = -0.3)  
  

#Percentage Students per ethnic group Pie Chart
df_pct <- df %>% mutate(Percentage = Total_Students * 100 / sum(Total_Students))
  
ggpie(df_pct, x = "Percentage", label =  paste(df_pct$Percentage, "%") , lab.pos = "in", lab.font = list(color = "white"), fill = "race.ethnicity" 
        , color = "white", palette = "jco", ggtheme = theme_foundation(), main = "Percentage Students in Ethnic Groups")



#Math distribution Histogram / Density plots
p1 <- ggplot(student_performance, aes(x = math.score)) + 
geom_histogram(bins = 20, color = "black", fill = "gray") +
geom_vline(aes(xintercept = mean(math.score)), linetype = "dashed", color = "Red") + ggtitle("Maths Score Histogram") 


p2 <- ggplot(student_performance, aes(x = reading.score)) + 
  geom_histogram(bins = 20, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(reading.score)), linetype = "dashed", color = "Red") + ggtitle("Reading Score Histogram") 


p3 <- ggplot(student_performance, aes(x = writing.score)) + 
  geom_histogram(bins = 20, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(writing.score)), linetype = "dashed", color = "Red") + ggtitle("Writing Score Histogram") 


#The density and histogram plots show that the maths scores are left skewed / more data on right with a left tail
#This means more number of students scored higher marks in maths

ggarrange(p1 , p2, p3, ncol = 3, nrow = 1, labels = c("A", "B", "C"))



p4 <- ggplot(student_performance, aes(x = math.score)) + 
  geom_density(aes(y = ..count..), color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(math.score)), linetype = "dashed", color = "Red") + ggtitle("Maths Score Density Distribution") +
  scale_x_continuous(name = "Maths Scores")

p5 <- ggplot(student_performance, aes(x = math.score)) + 
  geom_density(aes(y = ..count..), color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(math.score)), linetype = "dashed", color = "Red") + ggtitle("Reading Score Density Distribution") +
scale_x_continuous(name = "Reading Scores")

p6 <- ggplot(student_performance, aes(x = math.score)) + 
  geom_density(aes(y = ..count..), color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(math.score)), linetype = "dashed", color = "Red") + ggtitle("Writing Score Density Distribution") +
scale_x_continuous(name = "Writing Scores")

ggarrange(p4 , p5, p6, ncol = 1, nrow = 3, labels = c("A", "B", "C"))



#Gender based performance analysis

student_performance$TotalMarks <- student_performance$math.score + student_performance$reading.score + 
                                  student_performance$writing.score

student_performance$Percentage <- round(student_performance$TotalMarks / 3, 2)

student_performance$Grades <- ifelse(student_performance$Percentage >= 80, "A", 
                               ifelse(student_performance$Percentage < 80 & student_performance$Percentage >= 70, "B", 
                                ifelse(student_performance$Percentage < 70 & student_performance$Percentage >= 60, "C", 
                                 ifelse(student_performance$Percentage < 60 & student_performance$Percentage >= 50, "D", 
                                  ifelse(student_performance$Percentage < 50 & student_performance$Percentage >= 40, "E", 
                                   ifelse(student_performance$Percentage < 40, "F", "F"))))))
  
#Most students received B grade and C grades in their examinations 
table(student_performance$Grades)


df_gender <- student_performance %>% group_by(gender) %>% summarise(Total_Students = n())

df_gender$Total_Passed <- c(nrow(filter(student_performance, Grades != "F" & gender == "female")), 
                            nrow(filter(student_performance, Grades != "F" & gender == "male")))
                                                                   
                                                                                        
df_gender$Total_Failed <- c(nrow(filter(student_performance, Grades == "F" & gender == "female")), 
                            nrow(filter(student_performance, Grades == "F" & gender == "male")))

df_gender <- mutate(df_gender, Percent_Passed = Total_Passed * 100 / Total_Students , Percent_Failed = Total_Failed * 100 / Total_Students)

#Grades Count
#We can see there are highest number of students with B and C grades
ggplot(student_performance, aes(x = Grades)) + geom_bar() + 
  scale_y_continuous(name = "Count", limits = c(0, 280), breaks = seq(0,280,40)) 


#Grades Count based on Gender
#We can see 
#1. Females have considerably more A and B grades than males
#2. For grades C, D, E there are more male students than female students
#3. Only slightly higher number of females failed than males with F grade
#4. All above concludes that female students performed much better than male students overall

ggplot(student_performance, aes(x = Grades)) + geom_bar(aes(fill = gender), position = "Dodge") +
  scale_fill_manual(values = c("Blue", "Black")) + 
  scale_y_continuous(name = "Count", limits = c(0, 160), breaks = seq(0,160,20)) + ggtitle("Student Grades")


#Lunch and Gender / Grades analysis
#1. We see both males and females with standard lunch performed better with more students getting A,B,C grades
#2. Both females and males with free/reduced lunch had more number of students failing or getting E grades as compared to 
#   male and female students with standard lunch

ggplot(student_performance, aes(x = Grades)) + geom_bar(aes(fill = lunch), position = "Dodge") +
  scale_fill_manual(values = c("Blue", "Black")) + facet_grid(cols = vars(gender)) +
  scale_y_continuous(name = "Count", limits = c(0, 120), breaks = seq(0,120,20)) + ggtitle("Student Grades") 

#Test Preparation and Gender / Grades analysis
#1. Female students
#Surpising stats show that female students without any test preparation course achieved more A,B,C grades
#Similarly they also achieved more lower grades D,E,F 
#This suggests that test preparation course does not have much effect on achieving higher grades for female studetns,
#But test perparation course had significant impact on female students as only few female students got lower grades

#2. Male students
#Male students achieved more A grades when they completed test preparation course unlike female students where there 
#was no impact on higher grades
#Similar to females, male students who completed test preparation course have significantly less number of lower grades than
# those without.

#This suggests that for both male and female students who completed the test preparation course fared better 
# by avoiding lower grades than those who didnt. 
#This course did not impact females while it helped males achieve higher A grades 
ggplot(student_performance, aes(x = Grades)) + geom_bar(aes(fill = test.preparation.course), position = "Dodge") +
  scale_fill_manual(values = c("Blue", "Black")) + facet_grid(cols = vars(gender)) +
  scale_y_continuous(name = "Count", limits = c(0, 120), breaks = seq(0,120,20)) + ggtitle("Student Grades") 



#Gender and Ethnicity
#We see male students from ethnic group D perform better with highest number of A,B,C grades whereas
# female students from ethnic group C perform better with highest number of A,B,C,D grades
#Ethnic group A students (both male and female) have the worst performance with lowest counts in all grades

###################################
# This plot however is an incorrect view of things as number students differ for each ethnic group
# More accurate plot would be to show percentage of students performance per ethnic group
ggplot(student_performance, aes(x = Grades)) + geom_bar(aes(fill = race.ethnicity), position = "dodge") +
  scale_fill_manual(values = c("Blue", "Black", "darkGreen", "darkred", "orange")) + facet_grid(rows = vars(gender)) +
  scale_y_continuous(name = "Count", limits = c(0, 60), breaks = seq(0,60,10)) + ggtitle("Student Grades") 



df2 <- student_performance %>% group_by(race.ethnicity) %>% summarise(T = n())
df3 <- student_performance %>% group_by(race.ethnicity, Grades) %>% summarise(T = n()) 
 
#df3 <-  spread(df3, Grades, T)
df3 <- spread(df3, race.ethnicity, T)


df3 <- rbind(df3, c("Total", sum(df3$`group A`), sum(df3$`group B`), 
                    sum(df3$`group C`), sum(df3$`group D`), sum(df3$`group E`)))



df_groups.grades.percents <- data.frame(groupA = round(as.numeric(df3$`group A`) * 100 / as.numeric(df3[7,2]), 2), 
                                        groupB = round(as.numeric(df3$`group B`) * 100 / as.numeric(df3[7,3]), 2),
                                        groupC = round(as.numeric(df3$`group C`) * 100 / as.numeric(df3[7,4]), 2),
                                        groupD = round(as.numeric(df3$`group D`) * 100 / as.numeric(df3[7,5]), 2), 
                                        groupE = round(as.numeric(df3$`group E`) * 100 / as.numeric(df3[7,6]), 2))

df_groups.grades.percents$Grades <- c("Grade A", "Grade B", "Grade C", "Grade D", "Grade E", "Grade F", "Total")

df_groups.grades.percents <- df_groups.grades.percents[1:6 , ]

df_groups.grades.percents <- gather(df_groups.grades.percents, groups, percents, groupA:groupE)


#Same as above plot absolute count of students' grades in each ethnic group
pl7 <- ggplot(student_performance, aes(x = Grades)) + geom_bar(aes(fill = race.ethnicity), position = "dodge") +
  scale_fill_manual(values = c("Blue", "Black", "darkGreen", "darkred", "orange")) +
   ggtitle("Student Grades") 


#Percentage of student's grades per ethnic group
#This plot clearly shows that group E had the best performace of all groups with most higher grades and least lower grades
# Similarly group A had the worst performance with least higher grades and most lower grades.
pl8 <- ggplot(df_groups.grades.percents, aes(x = Grades, y = percents)) +
  geom_bar(stat = "identity", aes(fill = groups), position = "dodge") +
  scale_y_continuous(name = "Percentage of Students", limits = c(0, 34), breaks = seq(0,34,2))


ggarrange(pl7 , pl8, ncol = 1, nrow = 2, labels = c("A", "B"))


df5 <- student_performance %>% group_by(parental.level.of.education, Grades) %>% summarise(T = n()) 


df5 <- spread(df5, parental.level.of.education, T)

df5[is.na(df5$`master's degree`), 5] <- 0

df5 <- rbind(df5, c("Total", table(student_performance$parental.level.of.education)))

df_education.grades.percents <- data.frame(AD = round(as.numeric(df5$`associate's degree`) * 100 / as.numeric(df5[7,2]), 2), 
                                        BD = round(as.numeric(df5$`bachelor's degree`) * 100 / as.numeric(df5[7,3]), 2),
                                        HS = round(as.numeric(df5$`high school`) * 100 / as.numeric(df5[7,4]), 2),
                                        MD = round(as.numeric(df5$`master's degree`) * 100 / as.numeric(df5[7,5]), 2), 
                                        SC = round(as.numeric(df5$`some college`) * 100 / as.numeric(df5[7,6]), 2), 
                                        SHS = round(as.numeric(df5$`some high school`) * 100 / as.numeric(df5[7,7]), 2))


df_education.grades.percents <- df_education.grades.percents[1:6 , ]

df_education.grades.percents$Grades <- c("Grade A", "Grade B", "Grade C", "Grade D", "Grade E", "Grade F")

df_education.grades.percents <- gather(df_education.grades.percents, Parental.Education.Level, Percents, AD:SHS )


#PLotting percentage of students' grades per parental level of education
#1. Master's level education have highest percent of students getting A grades with High School being the lowest
# No master's level education student has failed while there is atleast 1 failed with all other
#2. High school level performed worst with significant less good grades and high percentage bad grades

 ggplot(df_education.grades.percents, aes(x = Grades, y = Percents)) +
  geom_bar(stat = "identity", aes(fill = Parental.Education.Level), position = "dodge") +
  scale_y_continuous(name = "Percentage of Students", limits = c(0, 35), breaks = seq(0,35,5))
 
 

 
############## Predicting Maths, Read, Write scores #########################
 library(corrgram)
 library(corrplot)
 library(caTools) 
 
 ggplot(student_performance, aes(x = math.score)) + geom_histogram(bins = 20, alpha = 0.5, fill = "blue")
 
 
 #Set a Random Seed
 set.seed(101)
 
 #Split the dataset into random samples using caTools
 sample <- sample.split(student_performance$math.score, SplitRatio = 0.66)
 
 #Train dataset (66% of whole dataset)
 #Test dataset (34% of whole dataset)
 train <- subset(student_performance, subset = sample == T)
 test <- subset(student_performance, subset = sample == F)
 
 model <- lm(math.score ~ . -TotalMarks -Percentage -Grades, data = train)
 
 
 #Interpret the model
 # Male Gender, Ethnic Group E, Standard Lunch, Test Preparation none, Reading and writing score all have
 #significant effect when predicting maths scores
 #As F-stat is much much greater than 1 with considerable number of features, we can completely dicard null test hypothesis
 #Which means the above features have significant impact while predicting maths scores of students
 summary(model)
 
 res <- residuals(model)
 
 #Change Numeric to dataframe for plotting
 res <- as.data.frame(res)
 
 #Plot residual histogram / good model should have close to normal distribution plot
 #We have some -ve residual values meaning -ve test scores predicted by our model
 ggplot(res, aes(res)) + geom_histogram(fill = "blue", alpha = 0.5)
 
 plot(model)
 
 
 #Remove negative values and make them 0 as we don't want negative test scores
 to_zero <- function(x) {
   if(x < 0 ) {
     return(0)
   } else{
     return(x)
   }
 }
 
 mathscore.predictions <- predict(model , test)
 results <- cbind(mathscore.predictions, test$math.score)
 colnames(results) <- c("Predicted", "Actual")
 results <- as.data.frame(results)
 results$Predicted <- round(results$Predicted, 0)
 
 print(head(results))
 
 #Remove -ve values
 results$Predicted <- sapply(results$Predicted, to_zero)
 
 #Mean Squared Error
 mse <- mean((results$Actual - results$Predicted) ^ 2)
 
 #Root Mean Squared Error
 rmse <- sqrt(mse)
 
 #Sum of Squared Errors / Residual sum of squares #Rss / sse
 sse <- sum( (results$Predicted - results$Actual) ^ 2 )
 
 #Total sum of squares
 sst <- sum( (mean(student_performance$math.score) - results$Actual) ^ 2)
 
 Rsquared <- 1 - (sse / sst)
 

 
 
 #################################################### Extra###################################
 lmPred <- predict(model, test, interval = "prediction", level = 0.95)
 summary(lmPred)
 
 mydata1 <- cbind(test, lmPred)
 
 p <- ggplot(mydata1, aes( fit, math.score)) +
   geom_point() +
   stat_smooth(method = lm)
 
 p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed") +
   xlab("Predicted Scores") + ylab("Actual Scores")
 
 
#Add weights
#student_performance <- student_performance %>% group_by(race.ethnicity) %>% mutate(n = n() / nrow(student_performance))

#ggplot(student_performance, aes( x = writing.score, fill = race.ethnicity)) + 
  #geom_density(aes(weights = n), col = NA, alpha = 0.35)
  #geom_violin()

# p -> probability (cumulative d.f)
# q -> quantile (inverse c.d.f)
# d -> density (density function)
# r -> random


#ggdistribution(dnorm, student_performance$reading.score,mean =  mean(student_performance$reading.score),sd = sd(student_performance$reading.score), colour = "red", fill = "blue") +

              

