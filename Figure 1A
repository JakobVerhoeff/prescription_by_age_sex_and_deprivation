
detach("package:MASS", unload = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
##########################################
##### FILTER ANTIBIOTICS #################
##########################################
#codes <- c()
#foidata<- foidata %>%
 #filter(BNF_CHEMICAL_SUBSTANCE_CODE %in% codes)
gpdatamale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-male.csv") #As of april 2022.
gpdatafemale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-female (1).csv") #As of april 2022
# I chose this date because foidata also starts from april 2022
###########################################################
### EDIT DEPRIVATION DATA #################################
###########################################################
deprivationdata<-read.csv("data/Data_fingertips/deprivation_data.csv")%>%
  select(-Sex,-Age,-Category,-Category.Type,-Lower.CI.95.0.limit,-Time.period.range,-Compared.to.goal,-New.data,-Time.period.Sortable,-Compared.to.ICB.sub.locations.value.or.percentiles,-Compared.to.England.value.or.percentiles,-Recent.Trend,-Value.note,-Denominator,-Count,-Upper.CI.99.8.limit,-Lower.CI.99.8.limit,-Upper.CI.95.0.limit,-Lower.CI.95.0.limit,-Indicator.ID,-Indicator.Name)
deprivationdata<-subset(deprivationdata, Time.period == "2019") # Select IMD2019
deprivationdata<-subset(deprivationdata,Area.Type=="GPs") # only use GP's
deprivationdata <- deprivationdata %>% rename(PRACTICE_CODE = Area.Code)
#edit deprivationdata into quantiles
deprivationdata$quintile <- cut(
  deprivationdata$Value,
  breaks = quantile(deprivationdata$Value, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  include.lowest = TRUE
)
###########################################################
### EDIT FOIDATA ##########################################
###########################################################
foidata <- merge(foidata, deprivationdata[, c("PRACTICE_CODE", "quintile")], 
                 by = "PRACTICE_CODE", all.x = TRUE)
foidata$ITEMS<-as.numeric(foidata$ITEMS)
foidata$ITEMS[is.na(foidata$ITEMS)] <- 1 # I convert the NA's into 1's.
foidata <- foidata[foidata$GENDER %in% c("Male", "Female"), ]
foidata<- foidata%>%select(-UNIQUE_PATIENT_COUNT,-FINANCIAL_YEAR,-BNF_CHEMICAL_SUBSTANCE_CODE)

foidata <- foidata %>%
  group_by(PRACTICE_CODE, GENDER, AGE_BAND) %>%
  summarise(
    ITEMS = sum(ITEMS, na.rm = TRUE),
    .groups = "drop"
  )
###########################################################
# EDIT GPDATA/MALE-FEMALE #################################
###########################################################
gpdatamale <- gpdatamale %>% rename(PRACTICE_CODE = ORG_CODE)
gpdatafemale<-gpdatafemale%>%rename(PRACTICE_CODE = ORG_CODE)
# Define age groups
gpdatamale$AGE<-as.numeric(gpdatamale$AGE)
gpdatamale <- gpdatamale %>% # Divide into the same 5-year agebands as FOI data
  mutate(
    age_group = case_when(
      AGE >= 0 & AGE <= 5  ~ "Age 0-5",
      AGE >= 6 & AGE <= 10 ~ "Age 6-10",
      AGE >= 11 & AGE <= 20 ~ "Age 11-20",
      AGE >= 21 & AGE <= 30 ~ "Age 21-30",
      AGE >= 31 & AGE <= 40 ~ "Age 31-40",
      AGE >= 41 & AGE <= 50 ~ "Age 41-50",
      AGE >= 51 & AGE <= 60 ~ "Age 51-60",
      AGE >= 61 & AGE <= 70 ~ "Age 61-70",
      AGE >= 71 & AGE <= 80 ~ "Age 71-80",
      AGE >= 81 & AGE <= 90 ~ "Age 81-90",
      AGE >= 91 & AGE <= 100 ~ "Age 91-100",
      AGE > 100 ~"Age over 100",
      TRUE ~ NA_character_  # Exclude other ages
    )
  ) %>%
  filter(!is.na(age_group))  # Remove rows with excluded ages

# Aggregate the number of patients by PRACTICE_CODE and age group
gpdatamale <- gpdatamale %>%
  group_by(PRACTICE_CODE, age_group) %>%
  summarise(
    total_patients = sum(NUMBER_OF_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  )
# Same for female
gpdatafemale$AGE<-as.numeric(gpdatafemale$AGE)
gpdatafemale <- gpdatafemale %>%
  mutate(
    age_group = case_when(
      AGE >= 0 & AGE <= 5  ~ "Age 0-5",
      AGE >= 6 & AGE <= 10 ~ "Age 6-10",
      AGE >= 11 & AGE <= 20 ~ "Age 11-20",
      AGE >= 21 & AGE <= 30 ~ "Age 21-30",
      AGE >= 31 & AGE <= 40 ~ "Age 31-40",
      AGE >= 41 & AGE <= 50 ~ "Age 41-50",
      AGE >= 51 & AGE <= 60 ~ "Age 51-60",
      AGE >= 61 & AGE <= 70 ~ "Age 61-70",
      AGE >= 71 & AGE <= 80 ~ "Age 71-80",
      AGE >= 81 & AGE <= 90 ~ "Age 81-90",
      AGE >= 91 & AGE <= 100 ~ "Age 91-100",
      AGE > 100 ~"Age over 100",
      TRUE ~ NA_character_  # Exclude other ages
    )
  ) %>%
  filter(!is.na(age_group))  # Remove rows with excluded ages

# Aggregate the number of patients by PRACTICE_CODE and age group
gpdatafemale <- gpdatafemale %>%
  group_by(PRACTICE_CODE, age_group) %>%
  summarise(
    total_patients = sum(NUMBER_OF_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  )
# Add GENDER column to each dataset
gpdatamale <- gpdatamale %>%
  mutate(GENDER = "Male")

gpdatafemale <- gpdatafemale %>%
  mutate(GENDER = "Female")

# Combine both datasets
gpdata_combined <- bind_rows(gpdatamale, gpdatafemale)
gpdata_combined <- gpdata_combined %>%
  rename(AGE_BAND = age_group)
# combine with foidata
foidata <- foidata %>%
  left_join(gpdata_combined, by = c("PRACTICE_CODE", "GENDER", "AGE_BAND"))
# make items per patient column
foidata <- foidata %>%
  mutate(items_per_patient = ITEMS / total_patients)
# add quintile
foidata <- foidata %>%
  left_join(deprivationdata %>% select(PRACTICE_CODE, quintile), by = "PRACTICE_CODE")


# make new dataframe with desired columns
foidata_quintile <- foidata %>%
  select(quintile, GENDER, AGE_BAND, ITEMS, total_patients) %>%
  group_by(quintile, GENDER, AGE_BAND) %>%
  summarise(
    total_items = sum(ITEMS, na.rm = TRUE),
    total_patients = sum(total_patients, na.rm = TRUE)
  ) %>%
  ungroup()
foidata_quintile <- foidata_quintile %>%
  mutate(items_per_patient = total_items / total_patients)

###########################################################
### CALCULATE ERROR BARS ##################################
###########################################################
# Recalculate foidata with NA set to 4 before they are removed
foidata_with_na <- read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")

# Set NA ITEMS to 4 for the interval calculation
foidata_with_na$ITEMS<-as.numeric(foidata_with_na$ITEMS)
foidata_with_na$ITEMS[is.na(foidata_with_na$ITEMS)] <- 4 # Set redacted data to 4

# Calculate items per patient with NA = 4 (for error bars)
foidata_na4_summary <- foidata_with_na %>%
  group_by(PRACTICE_CODE, GENDER, AGE_BAND) %>%
  summarise(
    total_items_na4 = sum(ITEMS, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate to quintile level
foidata_na4_summary <- foidata_na4_summary %>%
  left_join(deprivationdata %>% select(PRACTICE_CODE, quintile), by = "PRACTICE_CODE") %>%
  group_by(quintile, GENDER, AGE_BAND) %>%
  summarise(
    total_items_na4 = sum(total_items_na4, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate items per patient with NA = 1 (original)
foidata_combined <- foidata %>%
  group_by(quintile, GENDER, AGE_BAND) %>%
  summarise(
    total_items = sum(ITEMS, na.rm = TRUE),
    total_patients = sum(total_patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(items_per_patient = total_items / total_patients)

# Merge with NA=4 summary
foidata_combined <- foidata_combined %>%
  left_join(foidata_na4_summary, by = c("quintile", "GENDER", "AGE_BAND")) %>%
  mutate(items_per_patient_na4 = total_items_na4 / total_patients)

# Ensure AGE_BAND is ordered correctly
foidata_combined$AGE_BAND <- factor(foidata_combined$AGE_BAND, 
                                    levels = c("Age 0-5", "Age 6-10", "Age 11-20", 
                                               "Age 21-30", "Age 31-40", "Age 41-50", 
                                               "Age 51-60", "Age 61-70", "Age 71-80", 
                                               "Age 81-90", "Age 91-100"))

###########################################################
### PLOT ##################################################
###########################################################
ggplot(foidata_combined %>% filter(!is.na(AGE_BAND) & quintile != 'NA'), 
       aes(x = AGE_BAND, y = (items_per_patient + items_per_patient_na4) / 2, fill = quintile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = items_per_patient, ymax = items_per_patient_na4),
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



