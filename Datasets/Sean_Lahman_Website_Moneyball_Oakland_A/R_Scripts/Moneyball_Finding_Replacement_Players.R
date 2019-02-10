library(ggplot2)
library(dplyr)
library(tidyr)
library(ggfortify)

#GOAL: Find 3 replacement players (batters) for OAkland A's during the year 2001 when they lost their 3 players
#Data: http://www.seanlahman.com/baseball-archive/statistics/

filepath <- "D:\\Git_DataScience_Projects\\DataScience\\Datasets"

batting <- read.csv(paste(filepath , "Sean_Lahman_Website_Moneyball_Oakland_A\\Data\\Batting.csv", sep = "\\"))

str(batting)

summary(batting)


#Calculate Batting Average
#Batting AVG = Hits / At Bats

batting$BA <- batting$H / batting$AB
#batting <- mutate(batting, BA = H / AB)
#batting <- transmute(batting, BA = H / AB)


#Calculate On-Base-Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

#Calculate Slugging
#Calculate singles (total hits - double, triples & home runs)
batting$X1B <- batting$H- batting$X2B - batting$X3B - batting$HR
batting$SLG <- (batting$X1B + 2 * batting$X2B + 3 * batting$X3B + 4 * batting$HR) / batting$AB

str(batting)


#Merge Salary data with Batting Data (To find the most undervalued players)

salaries <- read.csv(paste(filepath , "Sean_Lahman_Website_Moneyball_Oakland_A\\Data\\Salaries.csv", sep = "\\"))

summary(salaries)

#Salaries start from year 1985 / Batting data starts from 1871
batting <- subset(batting, subset = yearID >= 1985)
batting_salary <- merge(batting, salaries , by =  c("playerID", "yearID"))

summary(batting_salary)

#Lost players we need to find replacement for
lost_players_names <- c("giambja01", "damonjo01", "saenzol01")

#Grab the data of lost players and only select concerned columns
lost_players <- subset(batting_salary, subset = playerID %in% lost_players_names & yearID == 2001) %>% 
                select(c("playerID", "H", "X2B", "X3B", "HR", "OBP", "SLG", "BA", "AB"))

#Replacement player conditions
# 1. Combined salary of 3 players should not exceed 15 mil. dollars
# 2. Combined number of At Bats (AB) should be equal or greater than that of lost players
# 3. Their mean On-Base percent (OBP) should be equal or greater than mean OBP of lost players

conditions <- summarise(lost_players, mOBP = mean(OBP), sAB = sum(AB))

#We need to find replacement for players lost in the year 2001
batting_salary <- subset(batting_salary, subset = yearID == 2001)


theme_update(plot.title = element_text(hjust = 0.5))

#1. Let us plot salary graph
# We can discard players from Oakland team as we want to find replacement players for Oakland
ggplot(batting_salary, aes(x = teamID.x, y = salary, fill = teamID.x )) + geom_bar(stat = "identity")

#We need combined salary of 3 players not to exceed 15 mil dollars
#So we can discard players with salary of 10mil and above 
ggplot(batting_salary, aes(x = salary)) + geom_histogram(bins = 10)

#We need combined AB of 3 players to be equal or greater than 1469
# So our 3 players should atleast have values around 1469 / 3 = 489. So around > 400 
ggplot(batting_salary, aes(x = AB)) + geom_histogram(bins = 20)


#Get potential players after discarding by above mentioned conditions
potential_players <- subset(batting_salary, subset = teamID.x != "OAK" & salary < 9000000  & AB >= 450 & OBP >= 0.35) %>% 
                      select(c("playerID", "teamID.x", "AB", "OBP", "R", "HR", "salary")) %>% 
                      arrange(-OBP)



y_names <- c("$0", "$1 Mil.", "$2 Mil.", "$3 Mil.", "$4 Mil.", "$5 Mil.", "$6 Mil.", "$7 Mil.", "$8 Mil.", "$9 Mil.")

#Lets plot our potential players graph to find 3 replacement players
#We plot for salary and ABP values to get the 3 most undervalued players within our replacement conditions

ggplot(potential_players, aes(x = reorder(playerID, AB), y = salary)) + geom_bar(aes(fill = factor(AB)), stat = "identity") + 
geom_text(aes(label = factor(AB)), hjust = -0.3) + labs(title = "Replacement Players for Oakland A's", y = "Player Salary (Million Dollars $)", x = "Player Names", fill = "At Bats") + 
coord_flip() + scale_y_continuous(limits = c(0, 9000000), breaks = seq(0, 9000000, 1000000), labels = y_names) 



#Home Runs
ggplot(potential_players, aes(x = reorder(playerID, HR), y = salary)) + geom_bar(aes(fill = factor(HR)), stat = "identity") + 
  geom_text(aes(label = factor(HR)), hjust = -0.3) + labs(title = "Replacement Players for Oakland A's", y = "Player Salary (Million Dollars $)", x = "Player Names", fill = "Home Runs") + 
  coord_flip() + scale_y_continuous(limits = c(0, 9000000), breaks = seq(0, 9000000, 1000000), labels = y_names) 

#6 players were narrowed down as replacement players based on our conditions to find
# The most undervalued players
selected_player_names <- c("aurilri01", "gonzalu01", "boonebr01", "pujolal01" , "berkmla01", "heltoto01")

selected_players <- potential_players[potential_players$playerID %in% selected_player_names, ] %>%
  arrange(-OBP)


ggplot(selected_players, aes(x = reorder(playerID, salary), y = salary)) + 
  geom_bar(stat = "identity", aes(fill = factor(OBP))) + coord_flip()

