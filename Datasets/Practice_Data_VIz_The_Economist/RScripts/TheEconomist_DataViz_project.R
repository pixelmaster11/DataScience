library(ggplot2)
library(dplyr)
library(ggthemes)

#Get the data from the path
path <- "D:\\Tuts\\DataScience\\R\\R_BootCamp\\R-Course-HTML-Notes\\R-Course-HTML-Notes\\R-for-Data-Science-and-Machine-Learning\\Training Exercises\\Capstone and Data Viz Projects\\Data Visualization Project\\Economist_Assignment_Data.csv"

#Read data from csv file and store as data frame and dropping column 1 which is just the index nums
df <- select(read.csv(path), -1)

#print(head(df))

#******************************************************************
#VISUALIZATION
  #DATA & AESTHETICS

pl <- ggplot(df, aes(x = CPI, y = HDI)) + geom_point(aes(color = Region), shape = 1, size = 5) 

countries_to_label <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                         "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                         "India", "Italy", "China", "South Africa", "Spane",
                         "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                         "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                         "New Zealand", "Singapore")

pl2 <- pl + geom_smooth(aes(group = 1),color = "Red", method = lm, formula = y ~ log(x), se = F) + 
            geom_text(data = subset(df, Country %in% countries_to_label), check_overlap = T, aes(label = Country), color = "Black")

pl3 <- pl2 + theme_economist_white() 

pl4 <- pl3 + scale_x_continuous(name = "Corruption Perception Index, 2011 (10 = least corrupt)", limits = c(0.9,10.5), breaks = 1:10)

pl5 <- pl4 + scale_y_continuous(name = "Human Development Index, 2011 (1 = best)", limits = c(0.2, 1), breaks = seq(0.2,1,0.1))

pl6 <- pl5 + ggtitle("Corruption and Human development")

print(pl6)

