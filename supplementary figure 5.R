library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
deprivationdata<-read.csv("data/Data_fingertips/deprivation_data.csv")%>%
  select(-Sex,-Age,-Category,-Category.Type,-Lower.CI.95.0.limit,-Time.period.range,-Compared.to.goal,-New.data,-Time.period.Sortable,-Compared.to.ICB.sub.locations.value.or.percentiles,-Compared.to.England.value.or.percentiles,-Recent.Trend,-Value.note,-Denominator,-Count,-Upper.CI.99.8.limit,-Lower.CI.99.8.limit,-Upper.CI.95.0.limit,-Lower.CI.95.0.limit,-Indicator.ID,-Indicator.Name)
deprivationdata<-subset(deprivationdata, Time.period == "2019") # Use only IDM 2019 (not other years)
deprivationdata<-subset(deprivationdata,Area.Type=="GPs") # Use only GP's (not ICB's)
deprivationdata <- deprivationdata %>%
  rename(PRACTICE_CODE = Area.Code)
deprivationdata<-deprivationdata%>%
  select(PRACTICE_CODE,Value)
gpdatamale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-male.csv")#As of april 2022.
# I chose this date because that is also the start of the financial year at which foidata begins.
# I assume there are no significant changes in the amount of patients across GP's for this year.
gpdatafemale<-read.csv("data/GP_data_NHS/gp-reg-pat-prac-sing-age-female (1).csv")#As of april 2022
# Combine into one dataframe
gpdata_combined <- merge(gpdatafemale, gpdatamale, 
                         by = c("EXTRACT_DATE", "CCG_CODE", "ONS_CCG_CODE", "ORG_CODE", "POSTCODE", "AGE"), 
                         suffixes = c("_female", "_male"))  %>%
  select(-SEX_male,-SEX_female,-EXTRACT_DATE,-CCG_CODE,-ONS_CCG_CODE,-POSTCODE)
gpdata_combined$AGE<-as.numeric(gpdata_combined$AGE)

# Remove rows where AGE_BAND is NA (That is where AGE = ALL)
gpdata_combined <- gpdata_combined %>%
  filter(!is.na(AGE))
gpdata_combined <- gpdata_combined %>%
  rename(PRACTICE_CODE = ORG_CODE)
gpdata_combined$AGE<-as.numeric(gpdata_combined$AGE)
gpdata_combined$NUMBER_OF_PATIENTS_female<-as.numeric(gpdata_combined$NUMBER_OF_PATIENTS_female)
gpdata_combined$NUMBER_OF_PATIENTS_male<-as.numeric(gpdata_combined$NUMBER_OF_PATIENTS_male)
gpdata_combined <- gpdata_combined %>%
  mutate(Total_Patients = NUMBER_OF_PATIENTS_female + NUMBER_OF_PATIENTS_male)%>%
  select(AGE,Total_Patients,PRACTICE_CODE)# Sum total patients per age
deprivationdata$quantile <- cut(
  deprivationdata$Value,
  breaks = quantile(deprivationdata$Value, probs = seq(0, 1, 0.1), na.rm = TRUE),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
  include.lowest = TRUE
)
deprivationdata<-deprivationdata%>%
  select(quantile,PRACTICE_CODE)
gpdata_combined<-gpdata_combined%>%
  left_join(deprivationdata, by = "PRACTICE_CODE")%>%
  select(quantile, AGE,Total_Patients)
gpdata_combined <- gpdata_combined %>%
  group_by(quantile, AGE) %>%
  summarise(Total_Patients = sum(Total_Patients), .groups = "drop")
mean_age_per_quantile <- gpdata_combined %>%
  group_by(quantile) %>%
  summarise(
    Mean_Age = sum(AGE * Total_Patients) / sum(Total_Patients),  # Weighted mean formula
    .groups = "drop"
  )
# Calculate percentage of Total_Patients in each age within a quantile
gpdata_combined <- gpdata_combined %>%
  group_by(quantile) %>%
  mutate(
    Total_Quantile_Patients = sum(Total_Patients),  # Total patients per quantile
    Percentage = (Total_Patients / Total_Quantile_Patients) * 100  # Percentage calculation
  ) %>%
  ungroup()  # Remove grouping

# Filter data for Q1 and Q10
filtered_data <- gpdata_combined %>%
  filter(quantile %in% c("Q1", "Q10"))

# Create overlapping histogram with transparency
ggplot(filtered_data, aes(x = AGE, weight = Percentage, fill = quantile)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4, color = "black") +
  labs(title = " Age Distribution for Q1 and Q10",
       x = "Age",
       y = "percentage of total patients",
       fill = "Quantile") +
  theme_minimal() +
  scale_fill_manual(values = c("Q1" = "blue", "Q10" = "red"))


