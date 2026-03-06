#### Figure 1 
# Plot of data trends 

#detach("package:MASS", unload = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)
library(patchwork)
setwd(here())

# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

################################################
##### Code to generate Figure 1A and B ########
################################################


# For all antibiotics
data_totals <- data %>%
  group_by(PRACTICE_CODE, GENDER, AGE_BAND, quintile) %>%
  summarise(
    total_patients_all = first(total_patients),
    items_total = sum(ITEMS_1, na.rm = TRUE),
    items4_total = sum(ITEMS_4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(GENDER, AGE_BAND, quintile) %>%
  summarise(
    total_patients_all = sum(total_patients_all, na.rm = TRUE),
    items_total = sum(items_total, na.rm = TRUE),
    items4_total = sum(items4_total, na.rm = TRUE),
    items_per_patient_all = items_total / total_patients_all,
    items4_per_patient_all = items4_total / total_patients_all,
    .groups = "drop"
  )

# Ensure AGE_BAND is ordered correctly
data_totals$AGE_BAND <- factor(data_totals$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))

###########################################################
### PLOT ##################################################
###########################################################
ggplot(data_totals %>% filter(!is.na(quintile)), 
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
data_totals_practice <- data %>%
  group_by(PRACTICE_CODE, GENDER, AGE_BAND, quintile) %>%
  summarise(
    total_patients_all = first(total_patients),
    items_total = sum(ITEMS_1, na.rm = TRUE),
    items4_total = sum(ITEMS_4, na.rm = TRUE),
    .groups = "drop"
  )

# Ensure AGE_BAND is ordered correctly
data_totals_practice$AGE_BAND <- factor(data_totals_practice$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))

ggplot(data_totals_practice %>% filter(!is.na(AGE_BAND)),  
       aes(x = AGE_BAND, y = items_total, group = interaction(PRACTICE_CODE, GENDER))) +
  geom_line(aes(colour = GENDER)) + 
  geom_point(aes(colour = GENDER)) +
  geom_line(aes(y = items4_total, colour = GENDER), linetype = "dashed") + 
  geom_point(aes(y = items4_total, colour = GENDER), pch = 2) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "black"),
        panel.grid.minor.y = element_line(size = 0.1, color = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # R


###### C&D
### Code for Figure 1b and c
# Summarize data
###############################################
foidata_summary <- data %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS_1, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband = ave(total_items, AGE_BAND, FUN = sum),
         percentage = (total_items / total_items_ageband) * 100)
#############################################
foidata_summary <- data %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS_1, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband_gender = ave(total_items, AGE_BAND, GENDER, FUN = sum),
         percentage = (total_items / total_items_ageband_gender) * 100)

#############################################
# Ensure AGE_BAND is ordered correctly
foidata_summary <- foidata_summary %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = c("0-5", "6-10", "11-20", 
                                                "21-30", "31-40", "41-50", 
                                                "51-60", "61-70", "71-80", "81-90", 
                                                "91-100", "over 100")))

# Create plot with different linetypes for gender
foidata_summary<-foidata_summary%>%
  filter(!is.na(AGE_BAND))
# Create last_points to extract the last data point for each group
last_points <- foidata_summary %>%
  group_by(ANTIBIOTIC_GROUP, GENDER) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Now plot the graph with geom_text() for labeling
g1c <- 
  ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                              color = ANTIBIOTIC_GROUP, 
                              group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                              linetype = GENDER)) +
  geom_line(linewidth = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #           show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female",
    x = "Age Band",
    y = "Percentage (%)",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  theme(legend.position="bottom")

ggsave("plots/figure1c.pdf")

ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                            color = ANTIBIOTIC_GROUP, 
                            group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                            linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #            size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #            show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female zoomed",
    x = "Age Band",
    y = "Percentage (%)",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  ylim(0, 25)  # Set y-axis limits to match percentage range

ggsave("plots/figure1d_ALL.pdf")

g1d <- ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                                   color = ANTIBIOTIC_GROUP, 
                                   group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                                   linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #            size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #            show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female zoomed",
    x = "Age Band",
    y = "Percentage (%)",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  scale_y_continuous(lim = c(1, 25)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo",drop = FALSE) +  # Apply viridis color palette
  theme(legend.position="bottom")

#### Stats for text 
foidata_summary %>% filter(ANTIBIOTIC_GROUP == "S&T", GENDER == "Male")
foidata_summary %>% filter(ANTIBIOTIC_GROUP == "UTIs", GENDER == "Male")
foidata_summary %>% filter(ANTIBIOTIC_GROUP == "UTIs", GENDER == "Female")


f_wide = foidata_summary %>%
  select(GENDER, ANTIBIOTIC_GROUP, percentage, AGE_BAND) %>%
  pivot_wider(names_from = GENDER, values_from = percentage) %>%
  mutate(ratio = Female / Male)

f_wide %>% filter(ANTIBIOTIC_GROUP == "UTIs") 

f_wide %>% filter(ANTIBIOTIC_GROUP == "Ceph's")

f_wide %>% filter(ANTIBIOTIC_GROUP == "Penicillins")

f_wide %>% filter(ANTIBIOTIC_GROUP == "Tetracyclines")

####################
#### BUILD FIGURE ##
###################
(g1a + g1b )/ (g1c + g1d + plot_layout(guides='collect') & theme(legend.position = 'bottom')) +
  plot_layout(heights = c(1, 2))

ggsave("plots/figure1.jpeg")
