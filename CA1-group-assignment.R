#Based on Tutorial 5
install.packages(c("tidyverse","dummy","caTools","conflicted"))
install.packages("skimr")
install.packages("caret")


library(conflicted)
library(tidyverse)
library(dummy)
library(caTools)
library(skimr)
library(caret)
library(ggplot2)

covid_df <- read_csv(file="covid_data_2023.csv")
#Sourced from: https://teacherscollege.screenstepslive.com/a/1127011-display-your-data-in-r-studio
View (covid_df)

#Source: https://uc-r.github.io/missing_values
colSums(is.na(covid_df))

# Skimming the dataset - Muhammad's Tutorial
skimmed_df <- skim(covid_df)
# Printing the skim summary - Muhammad's Tutorial
print(skimmed_df)

#question b
glimpse (covid_df)
summary (covid_df)

numeric_columns <- covid_df[,c(4,6,8,9)]

#Impute NA
#Source: https://www.tutorialspoint.com/how-to-remove-all-rows-having-na-in-r
cleaned_data <- na.omit(numeric_columns)


#question c
preprocess_df <- preProcess(covid_df[,c(4,6,8,9)], method = c('center', 'scale'))
data <- predict(preprocess_df, covid_df[,c(4,6,8,9)])
summary (data)

#question d


# Scatter plot: based on sample tutorial
ggplot(covid_df, aes(x=rate_14_day, y= country
)) + geom_point(size=1, color="Blue")

# Based on tutorial 5 - scatter plot
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

#heatmap
ggplot(covid_df, aes(x = year_week, y = country, fill = rate_14_day)) +
  geom_tile() +
  labs(title = "Heatmap of 14-Day Rates by Country and Week",
       x = "Year-Week",
       y = "Country")

#sketch
ggplot(covid_df, aes(x = indicator, y = rate_14_day, color = country)) +
  geom_point() +
  labs(title = "Rate of Deaths Over Time by Country",
       x = "Year-Week",
       y = "Rate of Deaths",
       color = "Country")


 covid_df.pca <- prcomp(cleaned_data, center = TRUE, scale. = TRUE)
summary (covid_df.pca)

#Task F

head(covid_df)

#dummy variables
covid_df$Indicator_Deaths<- ifelse(covid_df$indicator == 'cases', 1, 0)
covid_df$Indicator_Cases <- ifelse(covid_df$indicator == 'deaths', 1, 0)

view(covid_df)
