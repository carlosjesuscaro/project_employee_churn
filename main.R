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
# 1. Reducing the number of rows as many employees are listed multiple times
# 2. Ensureing that all dates are in the format: month/day/year
# 3. Termination date must be replaced by hire date + length of service
# 4. Organizing employees based on 3 categorical groups regarding the number
# of years of service
# 5. Organizing employees based on 3 categorical groups: executives, management
# and non management
# 6. Create a ew EStatus column to 1/0 (Active or Terminated)

# Data assumptions/corrections
# 1. The Employee ID column has multiple repeated with the same information except
# for columns 'age' and 'length_of_service'.It is understood that the information was capture
# more than once for the same user. Therefore, the only value to be considered is
# the one with maximum number of 'years as length_of_service'
# 2. 'Termination Date' is set as '1/1/1900' to all employees. This is incorrect
# and it will be recalculated. Additionally, the column that detrmined whether the
# employee is active or not is the 'Status' column

####################################################################
# DATA PREPARATION EXECUTION
####################################################################

#1. Reducing the number of rows as many employees are listed multiple times
temp <- 0
data <- data_raw[0,]
for (i in 1:length(data_raw$EmployeeID)) {
  if (is.element(data_raw$EmployeeID[i],data$EmployeeID) == FALSE) {
    temp_index = which(data_raw$EmployeeID == data_raw$EmployeeID[i])
    temp_max = max(data_raw$length_of_service[temp_index])
    for (ii in length(temp_index)){
      if (data_raw$length_of_service[temp_index[ii]] == temp_max){
        data <- rbind(data, data_raw[temp_index[ii],])
        }
      }
    }
}

# 2. Ensure that all dates are in the format: month/day/year
library(tidyverse)
as_date <- function(x) as.Date(x, format = "%m/%d/%Y")
data <- data %>% mutate(recorddate_key = as_date(recorddate_key),
           birthdate_key = as_date(birthdate_key),
           orighiredate_key = as_date(orighiredate_key),
           terminationdate_key = as_date(terminationdate_key))
# 2.1 Removing the timestamp from recorddate_key
data$recorddate_key <- format(as.POSIXct(data$recorddate_key, format='%m/%d/%Y %H:%M'), format='%m/%d/%Y')

# 3. Termination date must be replaced by hire date + length of service
data$terminationdate_key <- data$orighiredate_key + data$length_of_service * 365

# 4. Organizing employees based on 3 categorical groups regarding the number
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

# 5. Organizing employees based on 3 categorical groups: executives, management
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
# barplot just to get an idea of the proportion
barplot(table(data$emp_categ), xlab = "Categories", ylab = "Number of employees")
title('Job Categories')

# 6. Create a ew EStatus column to 1/0 (Active or Terminated)
for (iii in 1:length(data$STATUS)){
    if (data$STATUS[iii] == 'ACTIVE')
      {data$EStatus[iii] <- 1}
    else {data$EStatus[iii] <- 0}
  }


# Writting the clean dataframe as CSV
write.csv(data, 'employee_churn_clean.csv')

