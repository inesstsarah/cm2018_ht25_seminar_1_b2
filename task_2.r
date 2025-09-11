library(pwrss)
library(pwr)
install.packages(pwr)
data <- read.csv("data/Data_T1.csv")

head(data)

contr <-  data$NFL[data$GROUP == 0]
treat <- data$NFL[data$GROUP == 1]

# Get means
mean_contr <- mean(contr)
mean_treat <- mean(treat)

# Standard deviations
sd_contr <- sd(contr)
sd_treat <- sd(treat)

# We want to detect a difference of 30% reduction in the geometric mean of NfL.
mean_treat <- mean_treat * 0.7

nc_samples <- length(data$NFL)
NFL_contr <- rnorm(nc_samples, mean_contr, sd_contr)

# Calculate Cohen's D effect size
sd_pooled <- sqrt( (sd_treat^2 + sd_contr^2) / 2)
es_d <- (mean_treat - mean_contr) / sd_pooled

# calculate power over sample size
p.t.two <- pwr.t.test(d = es_d, power = 0.80, type = "two.sample", 
                      alternative = "two.sided")
plot(p.t.two, xlab = "sample size per group")

library(devtools)
install_github("adamdarwichkth/CM2018rpackage")
library(CM2018rpackage)
# run the data retrieval function to load data as dataframe, 
# specifying the number of study participants per study arm (=N)
my_dataframe <- ad_trial_data(n_per_arm = 89)

contr2 <- my_dataframe$NFL[my_dataframe$GROUP == 0]
treat_after <- my_dataframe$NFL[my_dataframe$GROUP == 1]

# Visualise data
hist(treat, main = "Before 3 months")
hist(treat_after, main = "After 3 months")

boxplot(treat, treat_after, names = c("Before", "After"), main = "NfL - before and after")

# Calculate reduction
log_before <- log(treat)
log_after <-  log(treat_after)

geometric_mean <- exp(mean(log_after) - mean(log_before))
percent_change <- (geometric_mean - 1) * 100

# Test for normality
shapiro.test(treat)
shapiro.test(treat_after)

# Wilcoxon-Mann-Whitney test on raw data
wilcox.test(treat, treat_after)

# Test log-transformed groups for normality
shapiro.test(log_before)
shapiro.test(log_after)

# T-test for log-transformed groups
var.test(log_before, log_after)
t.test(log_before, log_after, var.equal = FALSE)





