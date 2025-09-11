# Install packages
install.packages("tidyverse")
install.packages("car")

# Import packages
library(readr)
library(car)
library(dplyr)
library(MASS)

data <- read.csv("data/Data_T1.csv")

# Examine data structure
head(data)
summary(data)


# Visualise data
hist(data$NFL[data$GROUP == 0], main = "NfL - group 0", xlab = "X")
hist(data$NFL[data$GROUP == 1], main = "NfL - group 1", xlab = "X")

# Fit the NFL data to the normal distribution
normal_params_0 <- fitdistr(data$NFL[data$GROUP == 0], "normal")
normal_params_1 <- fitdistr(data$NFL[data$GROUP == 1], "normal")
normal_params_1

NFL_group_0 = data$NFL[data$GROUP == 0]
NFL_group_1 = data$NFL[data$GROUP == 1]

# Do fitted distributions for group 0 
height_min <- min(NFL_group_0)
height_max <- max(NFL_group_0)
x_pdf <- seq(height_min, height_max, 1)

weibull_params_0 <- fitdistr(NFL_group_0, "weibull")
# Then we sample from the pdfs using the parameter estimates
y_norm <- dnorm(x_pdf,
                mean = normal_params_0$estimate["mean"],
                sd   = normal_params_0$estimate["sd"])

y_weib <- dweibull(x_pdf,
                   shape = weibull_params_0$estimate["shape"],
                   scale = weibull_params_0$estimate["scale"])

# Plot the histogram and lines
hist(NFL_group_0, freq = FALSE, xlab = "NFL concentration", ylab = "Density", main = "NFL Group 0 Fitted Distributions")
lines(x_pdf, y_norm, col="blue")
lines(x_pdf, y_weib, col="red")
legend(x = "topleft", c("normal","Weibull"), col=c("blue","red"), lty = 1, cex = 1)

# Do fitted distributions for group 1 
height_min <- min(NFL_group_1)
height_max <- max(NFL_group_1)
x_pdf <- seq(height_min, height_max, 1)

weibull_params_1 <- fitdistr(NFL_group_1, "weibull")
# Then we sample from the pdfs using the parameter estimates
y_norm <- dnorm(x_pdf, 
                normal_params_1$estimate["mean"], 
                normal_params_1$estimate["sd"])
y_weib <- dweibull(x_pdf, 
                   weibull_params_1$estimate["shape"], 
                   weibull_params_1$estimate["scale"])


# Plot the histogram and lines
hist(NFL_group_1, freq = FALSE, xlab = "NFL concentration", ylab = "Density", main = "NFL Group 1 Fitted Distributions")
lines(x_pdf, y_norm, col="purple")
lines(x_pdf, y_weib, col="orange")
legend(x = "topleft", c("normal","Weibull"), col=c("purple","orange"), lty = 1, cex = 1)



hist(data$PTAU181[data$GROUP == 0], main = "P-tau181 - group 0", xlab = "X")
hist(data$PTAU181[data$GROUP == 1], main = "P-tau181 - group 1", xlab = "X")

#boxplot(data$NFL[data$GROUP == 0], data$NFL[data$GROUP == 1], data = data, main = "NfL - both groups")

boxplot(NFL ~ GROUP, data = data, main = "NfL - both groups")
boxplot(PTAU181 ~ GROUP, data = data, main = "P-tau181 - both groups")

qqPlot(data$NFL[data$GROUP == 0], main = "NfL - group 0")
qqPlot(data$NFL[data$GROUP == 1], main = "NfL - group 1")

qqPlot(data$PTAU181[data$GROUP == 0], main = "NfL - group 0")
qqPlot(data$PTAU181[data$GROUP == 1], main = "NfL - group 1")

# Test for normality
shapiro.test(data$NFL[data$GROUP == 0])
shapiro.test(data$NFL[data$GROUP == 1])
shapiro.test(data$PTAU181[data$GROUP == 0])
shapiro.test(data$PTAU181[data$GROUP == 1])

# T-test for NfL (normal)
var.test(NFL ~ GROUP, data = data)

t.test(NFL ~ GROUP, data = data, var.equal = FALSE)

# Wilcoxon test for P-tau181 (not normal)
wilcox.test(PTAU181 ~ GROUP, data = data)

# Correlation test. Filter only Group 0 (Control participants)
group1 <- subset(data, GROUP == 0)

# Scatter plot with regression line
ggplot(group1, aes(x = NFL, y = PTAU181)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Correlation between NFL and P-tau181 (Group 0: Control)",
    x = "NFL (pg/mL)",
    y = "P-tau181 (pg/mL)"
  ) +
  theme_minimal(base_size = 14)

# Pearson correlation
cor(group1$NFL, group1$PTAU181, method = "pearson")

# Correlation test. Filter only Group 1 (AD participants)
group1 <- subset(data, GROUP == 1)

# Scatter plot with regression line
ggplot(group1, aes(x = NFL, y = PTAU181)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Correlation between NFL and P-tau181 (Group 1: AD)",
    x = "NFL (pg/mL)",
    y = "P-tau181 (pg/mL)"
  ) +
  theme_minimal(base_size = 14)

# Pearson correlation
cor(group1$NFL, group1$PTAU181, method = "pearson")

# Correlation test. Scatter plot for ALL participants (Group 0 + Group 1 together)
ggplot(data, aes(x = NFL, y = PTAU181)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Correlation between NFL and P-tau181 (All Participants)",
    x = "NFL (pg/mL)",
    y = "P-tau181 (pg/mL)"
  ) +
  theme_minimal(base_size = 14)

# Pearson correlation across all participants
cor(data$NFL, data$PTAU181, method = "pearson")




