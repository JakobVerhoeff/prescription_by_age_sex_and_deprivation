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
totals_gp_bysplit10 <- read_csv("data/cleaned_totals_gp_bysplit10_gender.csv")

final = data %>%
  group_by(split10, GENDER, ANTIBIOTIC_GROUP)%>%
  summarise(ITEMS = sum(ITEMS)) %>%
  left_join(totals_gp_bysplit10) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

final$split10 <- factor(final$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

g1 <- ggplot(final, aes(x = split10, y = items_per_10000, 
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
  guides(col=guide_legend(ncol=2), linetype=guide_legend(ncol=2)) + 
  scale_color_viridis_d(option = "turbo")

ggsave("plots/figure2a.jpeg")

g2 <- ggplot(final, aes(x = split10, y = items_per_10000, color = ANTIBIOTIC_GROUP, 
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
  guides(col=guide_legend(ncol=2), linetype=guide_legend(ncol=2)) + 
  scale_color_viridis_d(option = "turbo") +
  ylim(0, 1200)

ggsave("plots/figure2b.jpeg")

#### Age/sex distribution by quintile 
totals_gp_bysplit10_as <- read_csv("data/cleaned_totals_gp_bysplit10_agesex.csv")
totals_gp_bysplit10_as$split10 <- factor(totals_gp_bysplit10_as$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))
totals_gp_bysplit10_as <- as.data.frame(totals_gp_bysplit10_as)

# Visualise this
ggplot(totals_gp_bysplit10_as, aes(x = factor(split10), y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Demographic split by IMD Decile and Sex",
    x = "IMD Decile",
    y = "Population proportion"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)
ggsave("plots/age_sex_split_by_IMD.jpeg")

ggplot(totals_gp_bysplit10_as, aes(x = factor(split10), y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Demographic split by IMD Decile and Sex",
    x = "IMD Decile",
    y = "Population size"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)
ggsave("plots/age_sex_split_by_IMD_numbers.jpeg")

g3 <- ggplot(totals_gp_bysplit10_as, aes(x = GENDER, y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity") +
#  guides(fill=guide_legend(ncol=2)) + 
  labs(
    x = "Gender",
    y = "Population size"
  ) +
  scale_fill_discrete("Age Band") + 
  theme_minimal() + 
  facet_wrap(~factor(split10), nrow = 2)

ggsave("plots/figure2c.jpeg")

g4 <- ggplot(totals_gp_bysplit10_as %>% filter(split10 %in% c("Q1", "Q10")), aes(x = GENDER, y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  #  guides(fill=guide_legend(ncol=2)) + 
  labs(
    x = "Gender",
    y = "Population proportions"
  ) +
  scale_fill_discrete("Age Band") + 
  theme_minimal() + 
  facet_wrap(~factor(split10), ncol = 2)

ggsave("plots/figure2d.jpeg")

## Prescription rates by age sex
age_sex_imd = data %>%
  group_by(split10, GENDER, AGE_BAND)%>%
  summarise(ITEMS = sum(ITEMS)) %>% 
            #agesexpop = sum(total_patients)) %>% # this doesn't work as doesn't sum those with no prescriptions
  left_join(totals_gp_bysplit10_as) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

age_sex_imd$split10 <- factor(age_sex_imd$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

age_sex_imd$AGE_BAND <- factor(age_sex_imd$AGE_BAND, 
                                    levels = c("0-5", "6-10", "11-20", 
                                               "21-30", "31-40", "41-50", 
                                               "51-60", "61-70", "71-80", 
                                               "81-90", "91-100"))


ggplot(age_sex_imd, aes(x = factor(split10), y = items_per_10000, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Antibiotic Prescriptions by IMD Decile, Age Band, and Sex",
    x = "IMD Decile",
    y = "Number of Prescriptions"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)

### each antibiotic 
age_sex_imd = data %>%
  group_by(split10, GENDER, AGE_BAND, ANTIBIOTIC_GROUP)%>%
  summarise(ITEMS = sum(ITEMS)) %>% 
  #agesexpop = sum(total_patients)) %>% # this doesn't work as doesn't sum those with no prescriptions
  left_join(totals_gp_bysplit10_as) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

age_sex_imd$split10 <- factor(age_sex_imd$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

age_sex_imd$AGE_BAND <- factor(age_sex_imd$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))


ggplot(age_sex_imd, aes(x = factor(split10), y = items_per_10000, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Antibiotic Prescriptions by IMD Decile, Age Band, and Sex",
    x = "IMD Decile",
    y = "Number of Prescriptions"
  ) +
  theme_minimal() + 
  facet_grid(ANTIBIOTIC_GROUP~GENDER)

ggplot(age_sex_imd, aes(x = split10, y = items_per_10000, 
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
  scale_color_viridis_d(option = "turbo") + 
  facet_wrap(~AGE_BAND)

#### Join most useful together
library(patchwork) 
(g1 + g2 + plot_layout(guides = "collect", widths = c(2,2))) / (g3 + g4 + plot_layout(guides = "collect", widths = c(3,1))) & plot_annotation(tag_levels = 'A')
ggsave("plots/figure2.jpeg", width = 12, height = 8)






## Do men get more pencillins than women? Does this explain it? 
data %>% filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
  group_by(GENDER) %>%
  summarise(sum(ITEMS)) 
# No... 
# Does a certain age group get more penicillins? 
data$AGE_BAND <- factor(data$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))
totals_age <- totals_gp_bysplit10_as %>%
  group_by(AGE_BAND) %>%
  summarise(total = sum(total_population))
pen_age <- data %>% filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
  group_by(AGE_BAND) %>%
  summarise(total_items = sum(ITEMS)) %>%
  left_join(totals_age) %>%
  mutate(items_per_10000 = (total_items / total)*10000) %>%
  arrange(items_per_10000)
pen_age$AGE_BAND <- factor(pen_age$AGE_BAND, 
                        levels = c("0-5", "6-10", "11-20", 
                                   "21-30", "31-40", "41-50", 
                                   "51-60", "61-70", "71-80", 
                                   "81-90", "91-100"))


ggplot(pen_age, aes(x=AGE_BAND, y = items_per_10000)) + 
  geom_point() + 
  geom_line() + 
  theme_minimal()

## yes more in kids and v old... 
