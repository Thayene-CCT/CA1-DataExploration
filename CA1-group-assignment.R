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

preprocess_df <- preProcess(covid_df[,c(4,6,8,9)], method = c('center', 'scale'))
data <- predict(preprocess_df, covid_df[,c(4,6,8,9)])
summary (data)

covid_df.pca <- prcomp(cleaned_data, center = TRUE, scale. = TRUE)
summary (covid_df.pca)


