# Load packages
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)

# Read The T4 and T3 data
data_t4a <- read.csv("data/Data_T4A.csv")

data_T3 <- read.csv("data/Data_T3.csv")

head(data_t4a)


#cleaning up the data so there is no commas or space
data_t4a <- janitor::clean_names(data_t4a)

#checking for column names 
colnames(data_t4a)

# Setting the order of the categories frpm the SPMSQ
# and converting the responses to a factor instead for easier plots
df_t4a <- data_t4a %>%
  mutate(response = factor(response,levels = c("Normal", "Mild", "Moderate", "Severe")))

#With the SPMSQ raw data we can show it in a gg plot to easily show the proportions of 
#the different treatments (control, low, medium and high)
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

# count plot for spmsq vs group
ggplot(data_t4a, aes(x = Response, fill = Response)) +
  geom_bar() +
  facet_wrap(~Treatment, nrow = 1) +  # separate panels per treatment group
  labs(x = "SPMSQ category", y = "Number of patients") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")  # legend not needed since x-axis labels show Response
  
  
#Given the biomarkers from previous data in Task3 we now can find out the relation
#between the biomarkers and the cognitive impairment categories
  
#Merging the SPMSQ data with the NFL data into one
  #use the df_t4a instead of raw data to have order of the categories in the merge
  
  
merged_data <- merge(data_T3, df_t4a, by.x = "ID", by.y = "id")   
#Boxplot to se the relation
  
library(ggplot2)

ggplot(merged_data, aes(x = response, y = NFL, fill = response)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "NFL biomarker levels by SPMSQ category",
       x = "SPMSQ Category",
       y = "NFL concentration") +
  theme_minimal() +
  theme(legend.position = "none")
  
  
  
  
