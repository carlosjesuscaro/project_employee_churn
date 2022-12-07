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
# 6. Organizing employees age based on their live's decade ('20s','30s',etc)
# 7. Create a new EStatus column to 0/1 (Active or Terminated)
# 8. Creating a new baseline for time: 2006 ~ Year 0
# 9. Removing the columns that will not be used in the analysis
# 10. Writting the clean dataframe as CSV


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
library(dplyr)
as_date <- function(x) as.Date(x, format = "%m/%d/%Y")
data <- data %>% mutate(recorddate_key = as_date(recorddate_key),
           birthdate_key = as_date(birthdate_key),
           orighiredate_key = as_date(orighiredate_key),
           terminationdate_key = as_date(terminationdate_key))
# 2.1 Removing the timestamp from recorddate_key
data$recorddate_key <- format(as.POSIXct(data$recorddate_key, format='%m/%d/%Y %H:%M'), format='%m/%d/%Y')

# 3. Termination date must be replaced by hire date + length of service
data$terminationdate_key <- data$orighiredate_key + data$length_of_service * 365.25

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

# 6. Organizing employees age based on their live's decade ('20s','30s',etc
for (i in 1:length(data$age)){
  temp_num <- floor(data$age[i]/10)
  if (temp_num == 1) {data$age_dec[i] <- '10s'}
  else if (temp_num == 2) {data$age_dec[i] <- '20s'}
  else if (temp_num == 3) {data$age_dec[i] <- '30s'}
  else if (temp_num == 4) {data$age_dec[i] <- '40s'}
  else if (temp_num == 5) {data$age_dec[i] <- '50s'}
  else if (temp_num == 6) {data$age_dec[i] <- '60s'}
}

# 7. Create a new EStatus column to 1/0 (Active or Terminated)
for (i in 1:length(data$STATUS)){
    if (data$STATUS[i] == 'ACTIVE')
      {data$EStatus[i] <- 0}
    else {data$EStatus[i] <- 1}
  }

# 8. Creating a new baseline for time: 2006 ~ Year 0
for (i in 1:length(data$STATUS_YEAR)){
  data$ESY[i] <- data$STATUS_YEAR[i] - 2006
}

# 9. Removing the columns that will not be used in the analysis
data_new <- data%>%select('EmployeeID','age','length_of_service', 'gender_short',
               'termtype_desc', 'length_categ', 'emp_categ', 'age_dec', 'EStatus', 'ESY')

# 10. Writting the clean dataframe as CSV
write.csv(data_new, 'employee_churn_clean.csv')

####################################################################
# ANALYSIS PLAN
####################################################################

library(survival)
library(ggplot2)
library(survminer)
library(stringr)

# 1. Comparing nested models
# 1.1 Likelihood Ratio Test (anova)
# 1.2 AIC
# 1.3 Concordance index
# 1.4 ROC
# 2. Comparing non-nested models
# 3. Assessing goodness of fit
# 3.1 Martingale
# 3.2 Case deletion
# 4. Checking model assumptions
# 4.1 Proportiionality of hazards
# 4.2 Schoenfeld residuals

# Estimating the Kaplan Meier curve
fit.KM <- survfit(Surv(ESY, EStatus) ~ 1, data = data_new)
fit.KM
summary(fit.KM)

# Plotting
plot(fit.KM, mark.time = TRUE,
     main = "Kaplan-Meier estimator",
     ylab = "Survival probability",
     xlab = "Years (since 2006)")

# Comparing survival among groups
# Gender
surv_gender <- survfit(Surv(ESY, EStatus) ~ gender_short, data = data_new)
survdiff(Surv(ESY, EStatus) ~ gender_short, data = data_new)
surv_gender

# Length category
surv_lcat <- survfit(Surv(ESY, EStatus) ~ length_categ, data = data_new)
survdiff(Surv(ESY, EStatus) ~ length_categ, data = data_new)
surv_lcat

# Employment category
surv_ecat <- survfit(Surv(ESY, EStatus) ~ emp_categ, data = data_new)
survdiff(Surv(ESY, EStatus) ~ emp_categ, data = data_new)
surv_ecat

# Age based on decades
surv_aged <- survfit(Surv(ESY, EStatus) ~ age_dec, data = data_new)
survdiff(Surv(ESY, EStatus) ~ age_dec, data = data_new)
surv_aged

# Plotting the survival functions for each covariate
par(mfrow=c(2,2))
plot(surv_gender, col = 1:2, main = "Gender", ylab = "Survival probability", xlab = "Years")
plot(surv_lcat, col = 1:3, main = "Employment length", ylab = "Survival probability", xlab = "Years")
plot(surv_ecat, col = 1:3, main = "Employment Category", ylab = "Survival probability", xlab = "Years")
plot(surv_aged, col = 1:3, main = "Age by decades", ylab = "Survival probability", xlab = "Years")


# Marginal association
# 1. Age
# 1.1 Analyzing age by year
fit.KM <- coxph(Surv(ESY, EStatus) ~ age, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation
# The risk of being terminated increases by 5% every extra year in age

# 1.2 Analyzing age by decade
data <- mutate(data, age_dec_alt = age / 10)
fit.KM <- coxph(Surv(ESY, EStatus) ~ age_dec_alt, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation
# The risk of being terminated increases by 63% every extra 10 years in age

# 1.3 Analyzing age by categorical group
fit.KM <- coxph(Surv(ESY, EStatus) ~ age_dec, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation

# Employment category
# There is a significant difference among the categories of Employment category:
# Exec, Management and Worker
fit.KM <- coxph(Surv(ESY, EStatus) ~ emp_categ, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE, col=1:3)
legend("bottomleft", legend = c('Exec','Worker','Management'), lty = 1, col = 1:3, text.col = 1:3,
       title = 'Employment Category')

# Lenght of service
fit.KM <- coxph(Surv(ESY, EStatus) ~ length_categ, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE, col=1:3)
legend("bottomleft", legend = c('Exec','Worker','Management'), lty = 1, col = 1:3, text.col = 1:3,
       title = 'Length of Service Category')
# Interpretation
# Employees between 5 and 15 years are 7% less likely to stop working while employees with more than
# 15 years are 2.3 more likely to stop working
# There is a significant difference among the categories of Length of Service:
# A: Less than 5 years
# B: Between 5 and 15 years
# C: More than 15 years

# Genre
fit.KM <- coxph(Surv(ESY, EStatus) ~ gender_short, data = data)
summary(fit.KM)
# Interpretation
# Male employees are 33% less likely to stop working than female employees

# Termination type
fit.KM <- coxph(Surv(ESY, EStatus) ~ termtype_desc, data = data)
summary(fit.KM)
# Interpretation
# It is almost 3 times more likely that a termination is voluntary versus being
# involuntary

# Cox Model with all the covariants
fit.KM <- coxph(Surv(ESY, EStatus) ~ age + length_categ + emp_categ +
  gender_short + termtype_desc, data = data)
summary(fit.KM)

























##################################################################
##################################################################
##################################################################

# Automatic detection of covariants for the model
model <- coxph(Surv(ESY, EStatus) ~ age + gender_short + length_categ +
  length_of_service + age_dec , data = data_new)
summary(model)
test_model <- step(model)
# Based on the outcome,the best model is achieved with all the covariants

# Manual selection of covarinats



##############################################################################################

# 1. Age
# 1.1 Analyziong age by year
fit.KM <- coxph(Surv(ESY, EStatus) ~ age, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation
# The risk of being terminated increases by 5% every extra year in age

# 1.2 Analyzing age by decade
data <- mutate(data, age_dec_alt = age / 10)
fit.KM <- coxph(Surv(ESY, EStatus) ~ age_dec_alt, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation
# The risk of being terminated increases by 63% every extra 10 years in age

# 1.3 Analyzing age by categorical group
fit.KM <- coxph(Surv(ESY, EStatus) ~ age_dec, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE)
# Interpretation
#

# Employment category
# There is a significant difference among the categories of Employment category:
# Exec, Management and Worker
fit.KM <- coxph(Surv(ESY, EStatus) ~ emp_categ, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE, col=1:3)
legend("bottomleft", legend = c('Exec','Worker','Management'), lty = 1, col = 1:3, text.col = 1:3,
      title = 'Employment Category')

# Lenght of service
fit.KM <- coxph(Surv(ESY, EStatus) ~ length_categ, data = data)
summary(fit.KM)
plot(survfit(fit.KM), xlab = 'Years after 2006', ylab = 'Survival Rate', conf.int = TRUE, col=1:3)
legend("bottomleft", legend = c('Exec','Worker','Management'), lty = 1, col = 1:3, text.col = 1:3,
       title = 'Length of Service Category')
# Interpretation
# Employees between 5 and 15 years are 7% less likely to stop working while employees with more than
# 15 years are 2.3 more likely to stop working
# There is a significant difference among the categories of Length of Service:
# A: Less than 5 years
# B: Between 5 and 15 years
# C: More than 15 years

# Genre
fit.KM <- coxph(Surv(ESY, EStatus) ~ gender_short, data = data)
summary(fit.KM)
# Interpretation
# Male employees are 33% less likely to stop working than

# Termination type
fit.KM <- coxph(Surv(ESY, EStatus) ~ termtype_desc, data = data)
summary(fit.KM)
# Interpretation
# It is almost 3 times more likelt that a termination is due to a voluntary decision by the
# employee

# Cox Model with all the covariants
fit.KM <- coxph(Surv(ESY, EStatus) ~ age + length_categ + emp_categ +
          gender_short + termtype_desc + age:termtype_desc, data = data)
summary(fit.KM)

# Model selecttion
# 1. Partial Likelihood Ratio Test
fit.KM1 <- coxph(Surv(ESY, EStatus) ~ age + length_categ  +
  gender_short + termtype_desc, data = data)
summary(fit.KM1)

fit.KM2 <- coxph(Surv(ESY, EStatus) ~ age + length_categ  +
  gender_short, data = data)
summary(fit.KM2)

fit.KM3 <- coxph(Surv(ESY, EStatus) ~ age + length_categ, data = data)
summary(fit.KM3)

fit.KM4 <- coxph(Surv(ESY, EStatus) ~ age, data = data)
summary(fit.KM4)

fit.KM5 <- coxph(Surv(ESY, EStatus) ~ 1, data = data)
summary(fit.KM5)

anova(fit.KM5, fit.KM1) #fit.KM1
anova(fit.KM4, fit.KM1) #fit.KM1
anova(fit.KM2, fit.KM1) #fit.KM1
anova(fit.KM3, fit.KM1) #fit.KM1
# fit.KM1 is the best model

# 2. AIC method
fits <- list(fit.KM1, fit.KM2, fit.KM3, fit.KM4, fit.KM5)
sapply(fits, AIC)
# fit.KM1 is the best model

# 3. Martingale residual
data$residual <- residuals(fit.KM1, type = "martingale")

#par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
with(data, {
  plot(age, residual)
  lines(lowess(age, residual), lwd = 2)

  plot(residual ~ gender_short)

  plot(residual ~ termtype_desc)

  plot(residual ~ length_categ)
})

residual.sch <- cox.zph(fit.KM1)
plot(residual.sch)



