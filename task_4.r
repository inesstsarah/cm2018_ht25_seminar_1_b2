# Load packages
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)

# Read The T4 data
data_t4a <- read.csv("data/Data_T4A.csv")
head(data_t4a)

#cleaning up the data so there is no commas or space
data_t4a <- janitor::clean_names(data_t4a)

#checking for column names 
colnames(data_t4a)

# Setting the order of the categories frpm the SPMSQ
# and converting the responses to a factor instead for easier plots
df_t4a <- data_t4a %>%
  mutate(response = factor(response,levels = c("Normal", "Mild", "Moderate", "Severe")))

#gg-plot in percentage 
ggplot(df_t4a, aes(x = factor(treatment), fill = response)) +
  geom_bar(position = "fill") + scale_y_continuous(labels = scales:: percent) +
  labs(
    x = "Treatment Arm",
    y = "Proportion",
    fill = "SPMSQ Category",
    title = "SPMSQ Distribution by Treatment Arm"
  ) 
  theme_minimal()