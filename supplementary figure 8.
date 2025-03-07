library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
antibiotics<-read_excel("C:/Users/Jakob/Documents/Research/Actual research/data/FOIdata/foi02243_reference_tables.xlsx",sheet=2)
deprivationdata<-read.csv("data/Data_fingertips/deprivation_data.csv")%>%
  select(-Sex,-Age,-Category,-Category.Type,-Lower.CI.95.0.limit,-Time.period.range,-Compared.to.goal,-New.data,-Time.period.Sortable,-Compared.to.ICB.sub.locations.value.or.percentiles,-Compared.to.England.value.or.percentiles,-Recent.Trend,-Value.note,-Denominator,-Count,-Upper.CI.99.8.limit,-Lower.CI.99.8.limit,-Upper.CI.95.0.limit,-Lower.CI.95.0.limit,-Indicator.ID,-Indicator.Name)
deprivationdata<-subset(deprivationdata, Time.period == "2019")
deprivationdata<-subset(deprivationdata,Area.Type=="GPs")
deprivationdata <- deprivationdata %>% rename(PRACTICE_CODE = Area.Code)
#########
foidata$ITEMS<-as.numeric(foidata$ITEMS)
foidata$ITEMS[is.na(foidata$ITEMS)] <- 1 # I convert the NA's into 1's.
foidata<- foidata%>%select(-UNIQUE_PATIENT_COUNT,-FINANCIAL_YEAR,-GENDER)
foidata <- foidata %>%
  group_by(PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE) %>%
  summarize(ITEMS = sum(ITEMS), .groups = "drop") # Sum ITEMS and drop grouping structure
antibiotics <- antibiotics %>%
  select(-AWaRe)
foidata<- foidata%>%
  left_join(antibiotics, by = c("BNF_CHEMICAL_SUBSTANCE_CODE" = "BNF_CHEMICAL_SUBSTANCE"))
foidata<-foidata%>%
  select(-BNF_CHEMICAL_SUBSTANCE_CODE)


foidata <- foidata %>%
  group_by(PRACTICE_CODE) %>%
  mutate(total_prescriptions = sum(ITEMS)) %>%
  ungroup()
foidata_broadspectrum <- foidata%>%
  filter(CHEMICAL_SUBSTANCE_BNF_DESCR %in% c("Co-amoxiclav (Amoxicillin/clavulanic acid)", 
                                           "Ciprofloxacin", 
                                             "Levofloxacin", 
                                             "Ofloxacin", 
                                             "Cefalexin", 
                                            "Cefradine"))
foidata_broadspectrum_summary <- foidata_broadspectrum %>%
  select(-CHEMICAL_SUBSTANCE_BNF_DESCR) %>%  # Remove the CHEMICAL_SUBSTANCE_BNF_DESCR column
  group_by(PRACTICE_CODE) %>%
  summarize(
    total_ITEMS = sum(ITEMS),
    total_prescriptions = sum(total_prescriptions),
    .groups = "drop"
  )
foidata_broadspectrum_summary <- foidata_broadspectrum_summary %>%
  mutate(ratio = (total_ITEMS / total_prescriptions)*100)

foidata_broadspectrum_summary <- merge(foidata_broadspectrum_summary, deprivationdata[, c("PRACTICE_CODE", "Value")], 
                 by = "PRACTICE_CODE", all.x = TRUE)

# get rid of outliers, value can be changed around
foidata_broadspectrum_summary <- foidata_broadspectrum_summary %>%
  filter(total_prescriptions >= 1000)


# graph
# Create the scatter plot with a line
ggplot(foidata_broadspectrum_summary, aes(x = Value, y = ratio)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of IMD2019 vs broad-spectrum percentage",
       x = "IMD2019",
       y = "percentage") +
  ylim(0, 10) +  # Set y-axis limits from 0 to 10
  theme_minimal()  # Apply a clean theme
 # All the points on this scatterplot represent a GP practice. Ratio is the percentage of prescriptions
# from a GP that are broad-spectrum antibiotics (specified in line 36 of this code).


