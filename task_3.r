
#The promising results of MECAS-123 and its favourable safety profile has led to the execution of a rudimentary dose finding study. 
# A follow-up study was carried out to measure NfL in AD patients treated across four study arms: control treatment, experimental treatment (with MECAS-123) at a low, medium and high dose. 
# A total of 80 patients were recruited (20 patients per study arm).

#Propose a statistical analysis for multiple comparisons.
# 
#Carry out the analysis using the attached data (Data_T3.csv Download Data_T3.csv).
#Plot the data, interpret and comment on the results.


install.packages('tidyverse',dependency=T)
install.packages('ggplot')

library(readr)
library(car)
library(dplyr)
library(MASS)
library(tidyverse)
library(ggplot2)

data <- read.csv('data/Data_T3.csv')
head(data)

# A statistical analysis can be done by visualising the distributions of the control, low, high and medium dose 
control_group <- data$NFL[data$GROUP=="Control"]
low_group <- data$NFL[data$GROUP=="Low"]
medium_group <- data$NFL[data$GROUP=="Medium"]
high_group <- data$NFL[data$GROUP=="High"]
treated_group <- data$NFL[data$GROUP!="Control"]

# Visualizing 
library(tidyr)
data %>% 
  select(-Species) %>%
  pivot_longer(everything()) %>% 
  ggboxplot(x = 'name', fill = "name", y = 'value', 
            palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00FABA"))

# Visualize histograms for each distribution 
hist(control_group, main = "Histogram of Control Group", xlab = 'NfL', ylab = 'Frequency', breaks = 5, ylim = c(0,10), xlim=c(0,100))
hist(low_group, main = "Histogram of Low Dose Group", xlab = 'NfL', ylab = 'Frequency', breaks = 5, ylim = c(0,10), xlim = c(0,100))
hist(medium_group, main = "Histogram of Medium Dose Group", xlab = 'NfL', ylab = 'Frequency', breaks = 5, ylim = c(0,10), xlim = c(0,100))
hist(high_group, main = "Histogram of High Dose Group", xlab = 'NfL', ylab = 'Frequency', breaks = 3, ylim = c(0,10), xlim = c(0,100))
hist(treated_group, main = "Histogram of Treated Group", xlab = 'NfL', ylab = 'Frequency', breaks = 5, ylim = c(0,30), xlim = c(0,100))

# Check the normality of every distribution 
shapiro.test(control_group) 
# data:  control_group
# W = 0.94499, p-value = 0.2974 p-value>0.05 indicates non-normality

shapiro.test(low_group) 
# data:  low_group
# W = 0.8813, p-value = 0.01868 p-value<0.05 indicates non-normality

shapiro.test(medium_group) 
# data:  medium_group
# W = 0.76382, p-value = 0.0002617 p-value<0.05 indicates non-normality

shapiro.test(high_group) 
# data:  high_group
# W = 0.9676, p-value = 0.7035 p-value>0.05 indicates normality

shapiro.test(treated_group)
# data:  treated_group
# W = 0.82543, p-value = 6.17e-07

# Q-Q Plot to visualize normality
qqPlot(control_group, main = "Q-Q Plot of Control Group")
qqPlot(low_group, main = "Q-Q Plot of Low Dose Group")
qqPlot(medium_group, main = "Q-Q Plot of Medium Dose Group")
qqPlot(high_group, main = "Q-Q Plot of High Dose Group")
qqPlot(treated_group, main = "Q-Q Plot of Treated Group")



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
library(effectsize)

cohens_d(control_group, treated_group)

# The results of the Cohen's D is as follows:
#Cohen's d |       95% CI
#------------------------
#  0.68      | [0.16, 1.19]
# This shows that there is a large separation within the control group and the treated group's means

# Do ANOVA on the 4 distributions
# Because the data is only slightly non-normal, ANOVA may still provide reliable results. Simulation studies 
# have shown that the false positive rate is not significantly affected by moderate deviations from normality.
# 
kruskal.test(NFL ~ GROUP, data = data)
# Kruskal-Wallis chi-squared = 12.64, df = 3, p-value = 0.005483

