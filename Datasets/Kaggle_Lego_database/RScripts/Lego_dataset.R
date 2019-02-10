#Libraries
library(dplyr)
library(ggplot2)

#DATA: https://www.kaggle.com/rtatman/lego-database


#Fiile path for lego dataset
filepath <- "D:\\Git_DataScience_Projects\\DataScience\\Datasets"

#Fetch Datasets

#Get the sets dataset
sets <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\sets.csv", sep = "\\"))

parts <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\parts.csv", sep = "\\"))

colors <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\colors.csv", sep = "\\"))

themes <- read.csv(paste(filepath, "Kaggle_Lego_database\\Data\\themes.csv", sep = "\\"))


part_categories <- parts <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\part_categories.csv", sep = "\\"))

#Get the inventory datasets
inventory <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\inventories.csv", sep = "\\"))

inventory_parts <- read.csv(paste(filepath, "Kaggle_Lego_database\\Data\\inventory_parts.csv", sep = "\\"))

inventory_sets <- read.csv(paste(filepath,"Kaggle_Lego_database\\Data\\inventory_sets.csv", sep = "\\"))


#****************************************COLORS ************************************

#Get total unique colors in dataset
unique_colors <- length(unique(colors$name))

print(paste("Unique colors in Lego sets =", unique_colors))

#Group colors by their transperancy  and calculate number of observations in those groups
colors %>% group_by(is_trans) %>% summarise(c = n()) %>%

#Bar - Plot
ggplot(aes(x = is_trans, y = c, fill = is_trans)) + geom_bar(stat = "identity") + 
scale_fill_manual(values=c("black","Green"))



#****************************************LEGO SETS ************************************

#Group by year and calculate average parts per year
mean_parts_by_year <- group_by(sets, year) %>% summarise(avg_parts = mean(num_parts))

#Line Plot for average parts per year 
#Shows Increasing trend for avg parts used per year
 ggplot(mean_parts_by_year, aes(x = year, y = avg_parts)) + geom_line() +
 scale_x_continuous(name = "Year", limits = c(1950, 2020), breaks = seq(1950, 2020, 10)) +
 scale_y_continuous(name = "Average parts", limits = c(0, 270), breaks = seq(0,270,20)) + 
 geom_point(color = "blue") + ggtitle("Average number of parts per Year")
 

#Group themes by year and calculate themes released per year
themes_by_year <- group_by(sets, year) %>% summarise(theme_count = length(unique(theme_id)))

#Plot shows increasing trend for theme release except after 2015 it plummeted
ggplot(themes_by_year, aes(x = year, y = theme_count)) + geom_line() +
  scale_x_continuous(name = "Year", limits = c(1950, 2020), breaks = seq(1950, 2020, 10)) +
  geom_point(color = "blue") + ggtitle("Unique Themes per Year")



#****************************************LEGO PART-CATEGORIES ************************************

#Rename same column names and id for joining
colnames(part_categories)[2] <- "part_category_name"
colnames(part_categories)[1] <- "part_cat_id"
colnames(parts)[2] <- "part_name"
colnames(colors)[2] <- "color_name"

#Join parts and parts categories dfs
part_cat <- left_join(part_categories, parts, by = "part_cat_id" )

#Get parts per category
parts_by_category <- part_cat %>% select(part_category_name,part_num) %>% group_by(part_category_name) %>%
                     summarise(parts_under_cat = length(unique(part_num))) %>% arrange(desc(parts_under_cat))

#Bar - Plot Parts per category
ggplot(parts_by_category, aes(x = reorder(part_category_name,parts_under_cat),y = parts_under_cat,fill = part_category_name)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values=colors$rgb) + theme(legend.position = "none") +
 labs(title="Parts under Category",x="Category",y="No of Parts")



              

