library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
foidata <- foidata %>% select(-UNIQUE_PATIENT_COUNT,-FINANCIAL_YEAR)
foidata$ITEMS<-as.numeric(foidata$ITEMS)

# Filter rows where ITEMS is NA

foidataNA <- foidata %>% filter(is.na(ITEMS))
foidata_no_NA <- foidata %>% filter(!is.na(ITEMS))

# make dataframe for total count per AGE_BAND

foidatagroupedby_AGE_BAND <- foidata %>%
  group_by(AGE_BAND) %>%
  summarise(count = n())

# make dataframe for total NA count per AGE_BAND

foidataNAgroupedby_AGE_BAND <- foidataNA %>%
  group_by(AGE_BAND) %>%
  summarise(count = n())

# these tables show the proportion of:

AGE_BAND_NA_prop <- prop.table(table(foidataNA$AGE_BAND)) # proportion of certain ageband in total NA's
AGE_BAND_no_NA_prop <- prop.table(table(foidata_no_NA$AGE_BAND)) # proportion of certain ageband in total not-NA's
AGE_BAND_total_prop <- prop.table(table(foidata$AGE_BAND)) # proportion of certain ageband in total rows

# Now check make this into a graph
AGE_BAND_NA_prop <- as.data.frame(AGE_BAND_NA_prop)
AGE_BAND_total_prop <- as.data.frame(AGE_BAND_total_prop)
colnames(AGE_BAND_NA_prop) <- c("AGE_BAND", "proportion_NA")
colnames(AGE_BAND_total_prop) <- c("AGE_BAND", "proportion_total")
merged <- merge(AGE_BAND_NA_prop, AGE_BAND_total_prop, by = "AGE_BAND")
merged<- merge(merged,foidatagroupedby_AGE_BAND, by = "AGE_BAND")

# graph
# Specify the desired order of AGE_BAND
desired_order <- c(
  "Age 0-5", "Age 6-10", "Age 11-20", "Age 21-30",
  "Age 31-40", "Age 41-50", "Age 51-60", "Age 61-70",
  "Age 71-80", "Age 81-90", "Age 91-100","Unknown"
)

# Convert AGE_BAND to a factor with the custom order
merged$AGE_BAND <- factor(merged$AGE_BAND, levels = desired_order)
ggplot(data = merged, aes(x = AGE_BAND)) +
  # Add line for proportion_NA
  geom_line(aes(y = proportion_NA, color = "Proportion NA"), group = 1, size = 1) +
  # Add line for proportion_total
  geom_line(aes(y = proportion_total, color = "Proportion Total"), group = 1, size = 1) +
  # Add points for both lines
  geom_point(aes(y = proportion_NA, color = "Proportion NA"), size = 2) +
  geom_point(aes(y = proportion_total, color = "Proportion Total"), size = 2) +
  # Customize labels and theme
  labs(
    title = "Comparison of Proportions by AGE_BAND",
    x = "AGE_BAND",
    y = "Proportion",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  )

