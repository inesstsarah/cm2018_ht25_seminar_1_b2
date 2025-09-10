# Clozapine-induced tachycardia
# According to Persson et al. 2024, heart rate as bpm is 
# approximately normal
# The mean (sd) heart rate is 73 (9) bpm. Under that assumption we 
# can sample from the theorical distribution.
# Normal sampling
library(pwrss)
library(pwr)
install.packages(pwr)
data <- read.csv("data/Data_T1.csv")

head(data)
# get mean

x_NFL <- mean(data$NFL)

# Function for geometric mean
geometric_mean_base <- exp(mean(log(data$NFL)))

sd_contr <- sd(data$NFL)
nc_samples <- length(data$NFL)
NFL_contr <- rnorm(nc_samples, x_NFL, sd_NFL)

# We want to detect a difference of 30% reduction in the geometric mean of NfL.
x_treat <- x_NFL *0.7
sd_treat <- sd_NFL
# Data is continuous and approximately normal, suggesting a t-test. 
# Tachycardia means a bpm of 100, but not the whole day.
# For simplicity, let's say we want to detect a difference 10 bpm.
x_treat <- 83
sd_treat <- 9
# calculate Cohen's D effect size
sd_pooled <- sqrt((sd_treat^2+sd_contr^2)/2)
es_d <- (x_treat- x_contr)/sd_pooled

# calculate power over sample size
p.t.two <- pwr.t.test(d = es_d, power = 0.80, type = "two.sample", 
                      alternative = "two.sided")
plot(p.t.two, xlab = "sample size per group")