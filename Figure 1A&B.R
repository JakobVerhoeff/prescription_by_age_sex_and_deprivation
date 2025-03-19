##### Code to generate Figure 1A and B

#detach("package:MASS", unload = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)
setwd(here())

# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

# For all antibiotics
data_totals <- data %>% group_by(GENDER, AGE_BAND, quintile) %>%
  summarise(total_patients_all = sum(total_patients, na.rm = TRUE),
            items_total = sum(ITEMS),
            items4_total = sum(ITEMS4), 
            items_per_patient_all = items_total / total_patients_all,
            items4_per_patient_all = items4_total / total_patients_all)

# Ensure AGE_BAND is ordered correctly
data_totals$AGE_BAND <- factor(data_totals$AGE_BAND, 
                           levels = c("0-5", "6-10", "11-20", 
                                      "21-30", "31-40", "41-50", 
                                      "51-60", "61-70", "71-80", 
                                      "81-90", "91-100"))

###########################################################
### PLOT ##################################################
###########################################################
ggplot(data_totals %>% filter(quintile != 'NA'), 
       aes(x = AGE_BAND, y = (items_per_patient_all + items4_per_patient_all) / 2, fill = quintile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = items_per_patient_all, ymax = items4_per_patient_all),
                width = 0.2, position = position_dodge(width = 0.7)) +
  labs(x = "Age Band", y = "Items Per Patient", 
       title = "Items Per Patient by Quintile and Age Band", 
       fill = "Deprivation Quintile\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1", 
                    labels = c("Q1 (Least Deprived)", "Q2", "Q3", "Q4", "Q5 (Most Deprived)")) +
  facet_wrap(~ GENDER) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black"))

ggsave("plots/figure1a.pdf")

g1a <- ggplot(data_totals %>% filter(GENDER == "Female"), 
              aes(x = AGE_BAND, y = (items_per_patient_all + items4_per_patient_all) / 2, fill = quintile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = items_per_patient_all, ymax = items4_per_patient_all),
                width = 0.2, position = position_dodge(width = 0.7)) +
  #geom_point(data = data_totals %>% filter(GENDER == "Female"), position = position_dodge(width = 1),
  #           aes(x = AGE_BAND, y = items_per_patient_all, group = interaction(AGE_BAND,quintile)), pch = "x") + 
  labs(x = "Age Band", y = "Items Per Patient", 
       fill = "Deprivation\nQuintile\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1", 
                    labels = c("Q1 (Least\nDeprived)", "Q2", "Q3", "Q4", "Q5 (Most\nDeprived)")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels for clarity


##### Items per patient by age and gender
data_totals_all <- data_totals %>% group_by(GENDER, AGE_BAND) %>%
  summarise(total_p = sum(total_patients_all),
            total_items = sum(items_total),
            total_items4 = sum(items4_total),
            items_per_patient = total_items / total_p,
            items4_per_patient = total_items4 / total_p) %>%
  ungroup()

g1b <- ggplot(data_totals_all %>% filter(!is.na(AGE_BAND)),  
       aes(x = AGE_BAND, y = items_per_patient, group = GENDER)) +
  geom_line(aes(colour = GENDER)) + 
  geom_point(aes(colour = GENDER)) +
  geom_line(aes(y = items4_per_patient, colour = GENDER), linetype = "dashed") + 
  geom_point(aes(y = items4_per_patient, colour = GENDER), pch = 2) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels for clarity

ggsave("plots/figure1b_1&4.pdf")

g1b <- ggplot(data_totals_all %>% filter(!is.na(AGE_BAND)),  
              aes(x = AGE_BAND, y = items_per_patient, group = GENDER)) +
  geom_line(aes(colour = GENDER)) + 
  geom_point(aes(colour = GENDER)) +
  scale_color_discrete("Gender") + 
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity

ggsave("plots/figure1b.pdf")

### Stats 

data_totals_all %>% filter(AGE_BAND == "0-5")

dg <- data_totals_all %>% group_by(AGE_BAND) %>%
  select(AGE_BAND, GENDER, items_per_patient) %>%
  pivot_wider(names_from = GENDER, values_from = items_per_patient) %>%
  summarise(diff = Female - Male)

dg %>% filter(!AGE_BAND%in% c("0-5","91-100")) %>% summarise(mean(diff))


#############################################################################
#### Does this vary much by GP practice? ############################################
#############################################################################
data_totals <- data %>% group_by(PRACTICE_CODE, GENDER, AGE_BAND) %>%
  summarise(total_patients_all = sum(total_patients, na.rm = TRUE),
            items_total = sum(ITEMS),
            items4_total = sum(ITEMS4), 
            items_per_patient_all = items_total / total_patients_all,
            items4_per_patient_all = items4_total / total_patients_all)

# Ensure AGE_BAND is ordered correctly
data_totals$AGE_BAND <- factor(data_totals$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))

ggplot(data_totals %>% filter(!is.na(AGE_BAND)),  
       aes(x = AGE_BAND, y = items_per_patient_all, group = interaction(PRACTICE_CODE, GENDER))) +
  geom_line(aes(colour = GENDER)) + 
  geom_point(aes(colour = GENDER)) +
  geom_line(aes(y = items4_per_patient_all, colour = GENDER), linetype = "dashed") + 
  geom_point(aes(y = items4_per_patient_all, colour = GENDER), pch = 2) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # R


