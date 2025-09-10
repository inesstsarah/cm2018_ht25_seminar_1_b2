
#The promising results of MECAS-123 and its favourable safety profile has led to the execution of a rudimentary dose finding study. 
# A follow-up study was carried out to measure NfL in AD patients treated across four study arms: control treatment, experimental treatment (with MECAS-123) at a low, medium and high dose. 
# A total of 80 patients were recruited (20 patients per study arm).

#Propose a statistical analysis for multiple comparisons.
# 
#Carry out the analysis using the attached data (Data_T3.csv Download Data_T3.csv).
#Plot the data, interpret and comment on the results.


install.packages('tidyverse',dependency=T)

library(tidyverse)
data <- read.csv('data/Data_T3.csv')
head(data)

# A statistical analysis can be done by visualising the distributions of the control, low, high and medium dose 
control_group <- data$NFL[data$GROUP=="Control"]
low_group <- data$NFL[data$GROUP=="Low"]
medium_group <- data$NFL[data$GROUP=="Medium"]
high_group <- data$NFL[data$GROUP=="High"]
treated_group <- data$NFL[data$GROUP!="Control"]
# Visualize histograms for each distribution 
hist(control_group)
hist(low_group)
hist(medium_group)
hist(high_group)
hist(treated_group)


# Do t test on control and low, medium and high group (control vs treated group)
t.test(treated_group, control_group)

# The results of the test are as follows
# data:  treated_group and control_group
# t = -2.5014, df = 30.227, p-value = 0.01801
# alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -15.492663  -1.568003
#sample estimates:
#  mean of x mean of y 
# 24.48952  33.01985
# This shows that there is a significant difference within the distributions because of the low p value

# Calculate Cohen's d value


