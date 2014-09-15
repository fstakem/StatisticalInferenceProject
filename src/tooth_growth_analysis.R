# <<<<--------------------------------------------<>-------------------------------------------->>>>
#
#       tooth_growth_analysis.R
#       By: Fredrick Stakem
#       Date: 9.14.14
#
#       Purpose:    To simulate the exponential distribution to learn basic statistics
#                   for the class Statistical Inference from John Hopkins offered
#                   through Coursera.
#
# <<<<--------------------------------------------<>-------------------------------------------->>>>

# Libraries

# <<<<-----------------------------------------< Part 2 >----------------------------------------->>>>
# Parameters

# Get data
data(ToothGrowth)

# Objective 1
# Load the ToothGrowth data and perform some basic exploratory data analyses 
summary(ToothGrowth)
aggregate(ToothGrowth$supp ~ ToothGrowth$dose, ToothGrowth, summary)

# Objective 2
# Provide a basic summary of the data.

# Objective 3
# Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose.

# Objective 4
# State your conclusions and the assumptions needed for your conclusions.  



