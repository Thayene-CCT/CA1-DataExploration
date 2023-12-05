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

#Dropping the note column because it is completely empty.
#Source: https://sparkbyexamples.com/r-programming/remove-column-in-r/
covid_df <- covid_df[,-11]
View (covid_df)

str(covid_df)

#Source: https://uc-r.github.io/missing_values
#Computing the total missing values in each column of our data frame.
colSums(is.na(covid_df))

#--------------------------Question A---------------------------------

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

#Categorical Plots: Bar chart

ggplot(covid_df) + geom_bar(aes(y = country), fill = 'lightgreen')

ggplot(covid_df) + geom_bar(aes(y = country_code), fill = 'lightyellow')

ggplot(covid_df) + geom_bar(aes(x = continent), fill = 'lightblue')

ggplot(covid_df) + geom_bar(aes(x = indicator), fill = 'lightcoral')

ggplot(covid_df) + geom_bar(aes(x = source), fill = 'lightpink')


#Continuous data: scatter plot

ggplot(covid_df) + geom_histogram(aes(x = rate_14_day), fill = 'lightcoral', bins = 30)

#Discrete data: Histogram plot

ggplot(covid_df) + geom_histogram(aes(x = weekly_count), fill = 'lightgreen', bins = 30)

ggplot(covid_df) + geom_histogram(aes(x = cumulative_count), fill = 'lightblue', bins = 30)

#Filtering column and converting date using as.Date function
#Source: https://stackoverflow.com/questions/21556772/filtering-a-data-frame-by-date-in-r
covid_df$year_week <- as.Date(covid_df$year_week)

ggplot(covid_df) + geom_bar(aes(x = year_week), fill = 'lightpink')

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

#--------------------------Question E---------------------------------



# summarize dataset

print(summary(covid_df))


# Data Exploration

# Summary statistics
#descriptive analysis
ncol(covid_df) # number of cols
nrow(covid_df) # number of rows
dim(covid_df)# number of cols and rows
str(covid_df)# it specifies whether it is numerical, factor, etc
summary(covid_df)#summary of dataset, it provides mean, median and other pertinent info


#stacked bar chart

#Question: how do the 14-day rates of cases and deaths compare across different countries?
#Are there countries with significantly higher or lower rates compared to others?

#Stacked bars to evaluate the amount of cases and deaths for each country according to the 14 days rate.
ggplot(data = filtered_data, aes(x = country_code, y = rate_14_day, fill = indicator)) +
  geom_bar(stat = "identity") +
  labs(title = "Cases and Deaths in 14 Days by Country Code",
       x = "Country Code",
       y = "Rate_14_day") +
  
  theme_minimal()

#checking weeks of the year and week count - cases and deaths
ggplot(data = covid_df, aes(x = year_week, y = weekly_count, color = indicator)) +
  geom_line() +
  labs(title = "Weekly Counts of Cases and Deaths Over Time",
       x = "Year",
       y = "Weekly Count",
       color = "Indicator") +
  theme_minimal()


