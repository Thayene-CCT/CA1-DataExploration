#Students:

#Thayene Lorens: 2020293
#Mayara Lorens: 2020292

#Installing all the necessary packages for this project.
install.packages(c("tidyverse","dummy","caTools","conflicted"))
install.packages("skimr")
install.packages("caret")

#Importing all the necessary libraries for this project.
library(conflicted)
library(tidyverse)
library(dummy)
library(caTools)
library(skimr)
library(caret)
library(ggplot2)

#Reading the covid_df dataset.
covid_df <- read_csv(file="covid_data_2023.csv")
#Function sourced from: https://teacherscollege.screenstepslive.com/a/1127011-display-your-data-in-r-studio
#Display the data frame using the 'View' function.
View (covid_df)

#Source: https://uc-r.github.io/missing_values
#Computing the total missing values in each column of our data frame.
colSums(is.na(covid_df))


#Reference: Muhammad's Sample Tutorial.
#Skimming the data set. 
skimmed_df <- skim(covid_df)
#Printing the skimmed summary
print(skimmed_df)

#Storing all numeric columns by index in the numeric_columns variable.
numeric_columns <- covid_df[,c(4,6,8,9)]

#Imputing NA by using na.omit
#Source: https://www.tutorialspoint.com/how-to-remove-all-rows-having-na-in-r
cleaned_data <- na.omit(numeric_columns)

#--------------------------Question B---------------------------------

#Using glimpse to display the mean values and standard deviation for each of the numerical variables.
glimpse (covid_df)
#Displaying results.
summary (covid_df)


#--------------------------Question C---------------------------------

#Use preProcess function from caret package to preprocess selected numeric columns (4, 6, 8, 9) in covid_df.
preprocess_df <- preProcess(covid_df[,c(4,6,8,9)], method = c('center', 'scale'))
#Using the predict function to apply the preprocessing transformations to the selected columns in covid_df
data <- predict(preprocess_df, covid_df[,c(4,6,8,9)])
summary (data)


#--------------------------Question D---------------------------------

#Reference: Tutorial 5 & Sample Tutorial by Muhammad. 

#Scatter plot: based on sample tutorial
ggplot(covid_df, aes(x=rate_14_day, y= country
)) + geom_point(size=1, color="Blue")

covid_df %>%
  ggplot() +
  geom_point(
    mapping = aes(x = rate_14_day, y = country),
    size = 1,
    color = "blue"
  ) +
  labs(title = "14 days rate", x = "rate_14_day", y = "country") +
  theme_minimal() + 
  theme(text = element_text(size = 14))

#Line Plot
 
ggplot(covid_df, aes(x = rate_14_day, y = country)) +
  geom_line(color = "red") +
  labs(title = "14 days rate", x = "rate_14_day", y = "country")

#Heatmap
ggplot(covid_df, aes(x = year_week, y = country, fill = rate_14_day)) +
  geom_tile() +
  labs(title = "Heatmap of 14-Day Rates by Country and Week",
       x = "Year-Week",
       y = "Country")


 covid_df.pca <- prcomp(cleaned_data, center = TRUE, scale. = TRUE)
summary (covid_df.pca)

#--------------------------Question F---------------------------------

head(covid_df)

#dummy variables
#One indicator for deaths and another one for cases
covid_df$Indicator_Deaths<- ifelse(covid_df$indicator == 'cases', 1, 0)
covid_df$Indicator_Cases <- ifelse(covid_df$indicator == 'deaths', 1, 0)

view(covid_df)

# Task E

#draft

#Tutorial 4

#
# summarize dataset

print(summary(covid_df))





