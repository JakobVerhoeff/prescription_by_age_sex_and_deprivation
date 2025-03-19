# Code for Figure 2

library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(viridis)
library(here)
setwd(here())

# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

# # Filter antibiotics that have a negative pseudo R-squared
# codes <- c("0501100H0", "0501060D0", "0501070X0", "0501070AE", 
#            "0501070I0", "0501090R0", "0501090K0")
# foidata <- foidata %>% 
#   filter(!BNF_CHEMICAL_SUBSTANCE_CODE %in% codes)
# 
# # Load antibiotic data
# # Load deprivation data
# deprivationdata <- read.csv("data/deprivation_data.csv") %>%
#   filter(Time.period == "2019", Area.Type == "GPs") %>%
#   select(PRACTICE_CODE = Area.Code, Value) 
# 
# # Create deprivation quantiles
# deprivationdata$quantile <- cut(
#   deprivationdata$Value,
#   breaks = quantile(deprivationdata$Value, probs = seq(0, 1, 0.1), na.rm = TRUE),
#   labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
#   include.lowest = TRUE
# )
# 
# # Process FOI data for males
# foidata_male <- foidata %>%
#   filter(GENDER == "Male") %>%
#   left_join(gpdata_combined %>% select(PRACTICE_CODE, TOTAL_MALE_PATIENTS), by = "PRACTICE_CODE") %>%
#   left_join(deprivationdata, by = "PRACTICE_CODE") %>%
#   mutate(GENDER = "Male")
# 
# # Process FOI data for females
# foidata_female <- foidata %>%
#   filter(GENDER == "Female") %>%
#   left_join(gpdata_combined %>% select(PRACTICE_CODE, TOTAL_FEMALE_PATIENTS), by = "PRACTICE_CODE") %>%
#   left_join(deprivationdata, by = "PRACTICE_CODE") %>%
#   mutate(GENDER = "Female")
# 
# # Combine male and female data
# foidata <- bind_rows(foidata_male, foidata_female)%>%
#   select(quantile,TOTAL_MALE_PATIENTS,TOTAL_FEMALE_PATIENTS,PRACTICE_CODE,ITEMS,BNF_CHEMICAL_SUBSTANCE_CODE,GENDER)
# 
# 
# foidata <- foidata %>%
#   mutate(TOTAL_PATIENTS = ifelse(GENDER == "Male", TOTAL_MALE_PATIENTS, TOTAL_FEMALE_PATIENTS)) %>%
#   select(-TOTAL_MALE_PATIENTS, -TOTAL_FEMALE_PATIENTS)
# 
# # Categorize antibiotics into groups
# foidata <- foidata %>%
#   mutate(ANTIBIOTIC_GROUP = case_when(
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501060D0") ~ "C&L",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501021M0", "0501021L0") ~ "Ceph's",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501100H0") ~ "Lep",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501050N0", "0501050H0", "0501050B0", "0501050A0") ~ "Macrolides",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501110C0") ~ "MTO",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501070X0", "0501070AE", "0501070I0") ~ "Other",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501015P0", "0501011P0", "0501012G0", "0501013K0", "0501013B0") ~ "Penicillins",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501120P0", "0501120X0", "0501120L0") ~ "Quinolones",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501080W0", "0501080D0") ~ "S&T",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501090R0", "0501090K0") ~ "TB",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501030V0", "0501030T0", "0501030P0", "0501030L0", "0501030Z0", "0501030I0") ~ "Tetracyclines",
#     BNF_CHEMICAL_SUBSTANCE_CODE %in% c("0501130R0", "0501130H0") ~ "UTIs",
#     TRUE ~ NA_character_
#   ))%>%
#   select(-BNF_CHEMICAL_SUBSTANCE_CODE)
# 
# 
# total_patients_per_quantile <- foidata %>%
#   group_by(quantile, GENDER, PRACTICE_CODE) %>%
#   summarise(TOTAL_PATIENTS = first(TOTAL_PATIENTS), .groups = "drop") %>%  # Keep only one TOTAL_PATIENTS per PRACTICE_CODE
#   group_by(quantile, GENDER) %>%
#   summarise(TOTAL_PATIENTS = sum(as.numeric(TOTAL_PATIENTS), na.rm = TRUE), .groups = "drop") %>%
#   pivot_wider(names_from = GENDER, values_from = TOTAL_PATIENTS, names_prefix = "TOTAL_")
# 
# 
# 
# foidata<-foidata%>%
#   select(-PRACTICE_CODE,-TOTAL_PATIENTS)
# 
# final <- foidata %>%
#   group_by(quantile, GENDER, ANTIBIOTIC_GROUP)%>%
#   summarise(ITEMS = sum(ITEMS, na.rm = TRUE), .groups = "drop")
# 
# 
# total_patients_long <- total_patients_per_quantile %>%
#   pivot_longer(cols = starts_with("TOTAL_"), names_to = "GENDER", values_to = "TOTAL_PATIENTS") %>%
#   mutate(GENDER = ifelse(GENDER == "TOTAL_Female", "Female", "Male"))  # Standardize gender names
# 
# # Join with final dataset
# final <- final %>%
#   left_join(total_patients_long, by = c("quantile", "GENDER"))
# final<-final%>%
#   mutate(items_per_10000 = (ITEMS / TOTAL_PATIENTS)*10000)
# 
# final <- final %>%
#   filter(!is.na(quantile))%>%
#   filter(!is.na(ANTIBIOTIC_GROUP))
# # Plot male and female antibiotic prescriptions
# last_points <- final %>%
#   group_by(ANTIBIOTIC_GROUP) %>%
#   slice_max(order_by = quantile, n = 1)

#### Do per 10000

# pivot_wider(names_from = GENDER, values_from = TOTAL_PATIENTS, names_prefix = "TOTAL_")
  # 
  # 
  # 
  # foidata<-foidata%>%
  #   select(-PRACTICE_CODE,-TOTAL_PATIENTS)
  # 
  # final <- foidata %>%
  #   group_by(quantile, GENDER, ANTIBIOTIC_GROUP)%>%
  #   summarise(ITEMS = sum(ITEMS, na.rm = TRUE), .groups = "drop")
  # 
  # 
  # total_patients_long <- total_patients_per_quantile %>%
  #   pivot_longer(cols = starts_with("TOTAL_"), names_to = "GENDER", values_to = "TOTAL_PATIENTS") %>%
  #   mutate(GENDER = ifelse(GENDER == "TOTAL_Female", "Female", "Male"))  # Standardize gender names
  # 
totals_gp_bysplit10 <- read_csv("data/cleaned_totals_gp_bysplit10.csv")

final = data %>%
  group_by(split10, GENDER, ANTIBIOTIC_GROUP)%>%
  summarise(ITEMS = sum(ITEMS)) %>%
  left_join(totals_gp_bysplit10) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

final$split10 <- factor(final$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

ggplot(final, aes(x = split10, y = items_per_10000, 
                  color = ANTIBIOTIC_GROUP, group = interaction(GENDER, ANTIBIOTIC_GROUP), 
                  linetype = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender",
       x = "Quintile",
       y = "Items per 10,000 Patients",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(option = "turbo")

ggplot(final, aes(x = quantile, y = items_per_10000, color = ANTIBIOTIC_GROUP, 
                  group = interaction(GENDER, ANTIBIOTIC_GROUP), linetype = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender zoomed",
       x = "Quintile",
       y = "Items per 10,000 Patients",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(option = "turbo") +
  ylim(0, 1200)

ggsave("plots/figure2.jpeg")
