# TASK 4

# Install packages
install.packages("ggalluvial")

# Load packages
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)
library(ggalluvial)
library(tidyr)

# Read all the Data needed 
data_t4a <- read.csv("data/Data_T4A.csv")
data_T3 <- read.csv("data/Data_T3.csv")
data_t4b <- read.csv("data/Data_T4B.csv")

#------------------------- Visualising SPMSQ --------------------------------------------


# We clean the data from commas/space, checking for columns,sort the data and converting
# responses etc for easier use
data_t4a <- janitor::clean_names(data_t4a)
colnames(data_t4a)

df_t4a <- data_t4a %>%
  mutate(response = factor(response,levels = c("Normal", "Mild", "Moderate", "Severe")))

#Counting the amount of patients in every category
counts <- df_t4a %>%
  group_by(treatment, response) %>%
  summarise(n = n(), .groups = "drop")

# Procentage plot for SPMSQ vs group______________________________________________________
ggplot(counts, aes(x = factor(treatment), y = n, fill = response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = n),
            position = position_fill(vjust = 0.5), 
            color = "black",
            size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Treatment Arm",
    y = "Proportion",
    fill = "SPMSQ Category",
    title = "SPMSQ Distribution by Treatment"
  ) +
  theme_minimal()

# Count plot for SPMSQ vs group____________________________________________________________
ggplot(data_t4a, aes(x = response, fill = response)) +
  geom_bar() +
  facet_wrap(~treatment, nrow = 1) +  # separate panels per treatment group
  labs(x = "SPMSQ category", y = "Number of patients") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")  # legend not needed since x-axis labels show Response
 


#---------------------- Visualize SPMSQ + NfL -------------------------------------------
  
#Given the biomarkers from previous data in Task3 we now can find out the relation
#between the biomarkers and the cognitive impairment categories
  
#Merging the SPMSQ data with the NFL data into one
merged_data <- merge(data_T3, df_t4a, by.x = "ID", by.y = "id")   

# Counting the amount of patients in every category
box_counts <- merged_data %>%
  group_by(response) %>%
  summarise(n = n(), .groups = "drop")

#Boxplot___________________________________________________________________________________
ggplot(merged_data, aes(x = response, y = NFL, fill = response)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_text(data = box_counts,
            aes(x = response, y = max(merged_data$NFL, na.rm = TRUE) * 1.05, 
            label = paste0("n=", n)),
            inherit.aes = FALSE,
            vjust = 0,
            size = 3.5) +
  labs(title = "NFL biomarker levels by SPMSQ category",
       x = "SPMSQ Category",
       y = "NFL concentration") +
  theme_minimal() +
  theme(legend.position = "none")
  
  

#----------------------- Visualising SPMSQ + NfL in 2 occassions ---------------------------

# Now we want to see the intra -individual variability were as the patients have answered
# the SPMSQ at two occassions.

df_t4b <- data_t4b %>%
  mutate(response = factor(SPMSQ,levels = c("Normal", "Mild", "Moderate", "Severe")))

# Count how many patients per category at each occasion
counts <- df_t4b %>%
  group_by(OCC, response) %>%
  summarise(n = n(), .groups = "drop")


#Speghetti-plot____________________________________________________________________________
ggplot(df_t4b, aes(x = OCC, y = response, group = ID, color = response)) +
  geom_line(alpha = 0.6, size = 1) +
  geom_point(size = 2) +
  geom_text(
    data = counts,
    aes(x = OCC, y = response, label = paste0("n=", n)),
    inherit.aes = FALSE,
    vjust = -1,
    nudge_x = ifelse(counts$OCC == 1, -0.1, 0.1), 
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Occasion 1", "Occasion 2"),
    expand = expansion(add = c(0.3, 0.3))
  ) +
  labs(
    title = "Patient progress in SPMSQ categories",
    x = "OCCASSION",
    y = "SPMSQ Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Alluvial plot______________________________________________________________________________

# Sorting so that each patient gets its own row 
df_sorted <- df_t4b %>%
  pivot_wider(
    id_cols = ID,
    names_from = OCC,
    values_from = response,
    names_prefix = "OCC"
  )

ggplot(df_sorted,
       aes(axis1 = OCC1, axis2 = OCC2, y = 1)) +  # y = 1 per patient
  geom_alluvium(aes(fill = OCC2), width = 1/12, alpha = 0.7) +
  geom_stratum(width = 1/8, fill = "grey80", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Occasion 1", "Occasion 2"), expand = c(.1, .05)) +
  labs(title = "Patient flows between OCC1 and OCC2",
       y = "Number of patients") +
  theme_minimal()+
  theme(legend.position = "none")


# Paired staple plot________________________________________________________________________
df_t4b %>%
  group_by(OCC, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = response, y = n, fill = factor(OCC))) +
  geom_col(position = "dodge") +
  labs(title = "SPMSQ category counts at each occasion",
       x = "SPMSQ Category",
       y = "Number of patients",
       fill = "Occasion") +
  theme_minimal()

# Computing how many patients got
# better, worse or same______________________________________________________________________

# Sorting so that each patient gets its own row 
df_sorted <- df_t4b %>%
  pivot_wider(
    id_cols = ID,
    names_from = OCC,
    values_from = response,
    names_prefix = "OCC"
  )

# removing the unused space in col
data_t4b <- data_t4b[ , -1]

# Convert SPMSQ to ordered factor 
df_t4b <- data_t4b %>%
  mutate(
    response = factor(SPMSQ,levels = c("Normal", "Mild", "Moderate", "Severe"),
    ordered = TRUE)
  )

# Compare per-patient change
df_sorted <- df_sorted %>%
  mutate(change = case_when(
    OCC2 > OCC1 ~ "worse",
    OCC2 < OCC1 ~ "better",
    TRUE ~ "same"
  ))

# Count patients by change type
df_sorted %>% count(change)


# Bar plot_________________________________________________________________________________
change_counts <- df_wide %>%
  count(change)

ggplot(change_counts, aes(x = change, y = n, fill = change)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), 
            vjust = -0.5, size = 5) +
            scale_fill_manual(values = c("better" = "forestgreen",
                                          "same" = "steelblue",
                                          "worse" = "firebrick")) +
            scale_y_continuous(expand = expansion(mult = c(0,0.2)))+
  labs(
    title = "Patient Changes in SPMSQ",
    x = "Change",
    y = "Number of Patients"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



