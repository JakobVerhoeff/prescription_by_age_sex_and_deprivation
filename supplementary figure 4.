library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")%>%
  select(ITEMS,GENDER,AGE_BAND)
foidata$ITEMS<-as.numeric(foidata$ITEMS)
foidata$ITEMS[is.na(foidata$ITEMS)] <- 1 # I convert the NA's into 1's.
foidata<-foidata%>%
  group_by(GENDER, AGE_BAND) %>%
  summarise(Sum_ITEMS = sum(ITEMS), .groups = 'drop')
foidata <- foidata %>%
  filter(GENDER %in% c("Male", "Female"))
###

gpdatamale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-male.csv")#As of april 2022.
# I chose this date because that is also the start of the financial year at which foidata begins.
# I assume there are no significant changes in the amount of patients across GP's for this year.
gpdatafemale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-female (1).csv")#As of april 2022
# Combine into one dataframe
gpdata_combined <- merge(gpdatafemale, gpdatamale, 
                         by = c("EXTRACT_DATE", "CCG_CODE", "ONS_CCG_CODE", "ORG_CODE", "POSTCODE", "AGE"), 
                         suffixes = c("_female", "_male"))  %>%
  select(-SEX_male,-SEX_female,-EXTRACT_DATE,-CCG_CODE,-ONS_CCG_CODE,-POSTCODE)

gpdata_combined <- gpdata_combined %>%
  mutate(AGE_BAND = cut(as.numeric(AGE), 
                        breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
                        labels = c("Age 0-5", "Age 6-10", "Age 11-20", "Age 21-30", "Age 31-40",
                                   "Age 41-50", "Age 51-60", "Age 61-70", "Age 71-80",
                                   "Age 81-90", "Age 91-100", "Age 100+"),
                        right = TRUE,       # Include the upper boundary
                        include.lowest = TRUE))%>%
  select(AGE_BAND, NUMBER_OF_PATIENTS_female,NUMBER_OF_PATIENTS_male)
  
  gpdata_combined <- gpdata_combined %>%
  group_by(AGE_BAND) %>%
  summarise(
    Total_Female_Patients = sum(NUMBER_OF_PATIENTS_female),
    Total_Male_Patients = sum(NUMBER_OF_PATIENTS_male),
    .groups = "drop"
  )

foidata<-foidata%>%
  left_join(gpdata_combined,by ="AGE_BAND")
foidata <- foidata %>%
  mutate(Total_Patients = case_when(
    GENDER == "Female" ~ Total_Female_Patients,
    GENDER == "Male" ~ Total_Male_Patients
  )) %>%
  select(GENDER, AGE_BAND, Sum_ITEMS, Total_Patients)
foidata<-foidata%>%
  mutate(items_per_patient = Sum_ITEMS/Total_Patients )
##########################


###
# Define the correct order of AGE_BAND levels
foidata <- foidata %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = c(
    "Age 0-5", "Age 6-10", "Age 11-20", "Age 21-30", 
    "Age 31-40", "Age 41-50", "Age 51-60", "Age 61-70",
    "Age 71-80", "Age 81-90", "Age 91-100", "Age over 100"
  )))

# Create the line plot with ordered AGE_BAND
ggplot(foidata, aes(x = AGE_BAND, y = items_per_patient, group = GENDER, color = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) + # Optional: Adds points for better visibility
  labs(title = "ITEMS Across Age Bands by Gender",
       x = "Age Band",
       y = "ITEMS per patient",
       color = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

