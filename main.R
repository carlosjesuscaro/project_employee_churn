# Title     : Employee Churn
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-08-28

# Reading the data
data_raw <- read.csv('Employee Churn.csv')
# Source: https://www.kaggle.com/HRAnalyticRepository/employee-attrition-data

####################################################################
# DATA PREPARATION PLAN
####################################################################
# 1. Ensureing that all dates are in the format: month/day/year
# 2. Termination date must be replaced by hire date + length of service
# 3. Organizing employees based on 3 categorical groups regarding the number
# of years of service
# 4. Organizing employees based on 3 categorical groups: executives, management
# and non management
# 5. Replacing the employee ID for a unique code
# 6. Calculate information about the age when the employee was hired to verify
# the data consistency (Min, Max, Mean, Median)

# Data assumptions/corrections
# 1. The Employee ID column has multiple repeated and it is being assumed
# that it is an error
# 2. 'Termination Date' is set as '1/1/1900' to all employees. This is incorrect
# and it will be recalculated

####################################################################
# DATA PREPARATION EXECUTION
####################################################################
# 1. Ensure that all dates are in the format: month/day/year
library(tidyverse)
as_date <- function(x) as.Date(x, format = "%m/%d/%Y")
data <- data_raw %>% mutate(recorddate_key = as_date(recorddate_key),
           birthdate_key = as_date(birthdate_key),
           orighiredate_key = as_date(orighiredate_key),
           terminationdate_key = as_date(terminationdate_key))
# 1.1 Removing the timestamp from recorddate_key
data$recorddate_key <- format(as.POSIXct(data_raw$recorddate_key, format='%m/%d/%Y %H:%M'), format='%m/%d/%Y')

# 2. Termination date must be replaced by hire date + length of service
data$terminationdate_key <- data$orighiredate_key + data$length_of_service * 365

# 3. Organizing employees based on 3 categorical groups regarding the number
# of years of service
# The categories are: A) Short->0~5, Medium->5~15, Long->15+
# Defining the function that will create the categories
length_categ <- function(x){
  len_cat <- 'N'
  if (x < 5) {len_cat <- 'A'}
    else if (x >= 5 && x <= 15) {len_cat <- 'B'}
    else if (x > 15) {len_cat <- 'C'}
    return(len_cat)
}
for (i in 1:length(data$length_of_service)) {
  data$length_categ[i] <- length_categ(data$length_of_service[i])
}
# barplot just to get an idea of the proportion
barplot(table(data$length_categ), xlab = "Categories", ylab = "Number of employees",
        legend.text = 'A: Under 5y, B: between 5 and 15y, C: More than 15y')
title('Length of Service Categories')

# 4. Organizing employees based on 3 categorical groups: executives, management
# and non management
emp_categ <- function(x){
  if (x == 'CEO' || grepl('VP', x, ignore.case = TRUE) ||
    grepl('Chief', x, ignore.case = TRUE) ||
    grepl('legal', x, ignore.case = TRUE))
    {emp_cat <- 'Exec'
      return(emp_cat)}
    else if (grepl('manag', x, ignore.case = TRUE) ||
      grepl('Director', x, ignore.case = TRUE))
      {emp_cat <- 'Management'
        return(emp_cat)}
    else {emp_cat <- 'Worker'
          return(emp_cat)}
}
for (i in 1:length(data$job_title)) {
  data$emp_categ[i] <- emp_categ(data$job_title[i])
}



