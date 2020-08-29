# Title     : Employee Churn
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-08-28

# Reading the data
data <- read.csv('Employee Churn.csv')

# Organizing/cleaning the data
# 1. Ensure that all dates are in the format: month/day/year
# 2. Termination date must be replaced by hire date + length of service
# 3. Organizing employees based on 3 categorical groups regardin the number
# of years of service
# 4. Organizing employees based on 3 categorical groups: executives, management
# and non management
# 5. Replacing the employee ID for a unique code
# 6. Calculate information about the age when the employee was hired to verify
# the data consistency (Min, Max, Mean, Median)

# DATA ASUMPTIONS/CORRECTIONS
# 1. The Employee ID column has multiple repeated and it is being assumed
# that it is a typo
# 2. 'Termination Date' is set as '1/1/1900' to all employees. This is incorrect
# and it will be recalculated



