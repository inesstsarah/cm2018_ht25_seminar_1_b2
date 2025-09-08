# Examine data structure
head(data)
summary(data)

# Visualise data
hist(data$NFL[data$GROUP == 0], main = "NfL - group 0", xlab = "X")
hist(data$NFL[data$GROUP == 1], main = "NfL - group 1", xlab = "X")

hist(data$PTAU181[data$GROUP == 0], main = "P-tau181 - group 0", xlab = "X")
hist(data$PTAU181[data$GROUP == 1], main = "P-tau181 - group 1", xlab = "X")

#boxplot(data$NFL[data$GROUP == 0], data$NFL[data$GROUP == 1], data = data, main = "NfL - both groups")

boxplot(NFL ~ GROUP, data = data, main = "NfL - both groups")
boxplot(PTAU181 ~ GROUP, data = data, main = "P-tau181 - both groups")

library(car)
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