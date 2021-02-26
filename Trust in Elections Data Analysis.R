# Let's load the packages we'll be using
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("survey")
install.packages("jtools")
library(tidyverse)
library(ggplot2)
library(corrplot)
library(survey)
library(jtools)

# Upload data
data <- read.table("nonvoters_data.csv", header=TRUE, stringsAsFactors = FALSE, sep = ",")

# Get high-level stats
head(data)
str(data)
summary(data)

# Create basic charts
ggplot(data, aes(x=race)) + geom_bar() 
ggplot(data, aes(x=gender)) + geom_bar()
ggplot(data, aes(x=income_cat)) + geom_bar()
ggplot(data, aes(x=voter_category)) + geom_bar()
ggplot(data, aes(x=educ)) + geom_bar()

# Let's take a closer look at these two questions
ggplot(data, aes(x=Q27_2)) + geom_bar()
ggplot(data, aes(x=Q5)) + geom_bar()

# To make analysis easier to interpret, let's rename the three columns of interest
data <- data %>%
  rename(
    voted_in_2016 = Q27_2, 
    who_wins_matters = Q5,
    trust_in_officials = Q8_2
  )

# Create a survey data file
nhc <- svydesign(id=~RespId, weights=~weight, nest=TRUE, survey.lonely.psu = "adjust", data=data)

# Get mean values
svymean(~gender, nhc)



# Get summaries
summary(nhc)

# Additional summary stats
svysd(~ppage,design = nhc, na = TRUE)
svymean(~gender, nhc)
confint(svymean(~gender, nhc))

# Let's look at different cuts of the question of interest
svytable(~gender+race+educ, nhc)
svytable(~voted_in_2016+who_wins_matters+gender, nhc) # those who voted in 2016 believe that elections matter regardless of gender
svytable(~voted_in_2016+who_wins_matters+race, nhc) 
svytable(~voted_in_2016+who_wins_matters+income_cat, nhc) 
svytable(~voted_in_2016+who_wins_matters+income_cat+educ, nhc) 

# For fun, let's experiment with a regression model
summary(svyglm(voted_in_2016~+trust_in_officials, design=nhc, na.action = na.omit))










