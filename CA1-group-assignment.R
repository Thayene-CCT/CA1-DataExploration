#Based on Tutorial 5
install.packages(c("tidyverse","dummy","caTools","conflicted"))
install.packages("skimr")

library(conflicted)
library(tidyverse)
library(dummy)
library(caTools)
library(skimr)

covid_df <- read_csv(file="covid_data_2023.csv")
#Sourced from: https://teacherscollege.screenstepslive.com/a/1127011-display-your-data-in-r-studio
View (covid_df)

#Source: https://uc-r.github.io/missing_values
colSums(is.na(covid_df))

# Skimming the dataset - Muhammad's Tutorial
skimmed_df <- skim(covid_df)
# Printing the skim summary - Muhammad's Tutorial
print(skimmed_df)

glimpse (covid_df)
summary (covid_df)
