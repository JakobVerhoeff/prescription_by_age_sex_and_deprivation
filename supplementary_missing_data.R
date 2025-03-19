#### Analysis of missing data 

# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)
setwd(here())

foidata<-read.csv("data/foi02243_practice_2022_2023.csv")

### 


### Gender missing by age 
foidata_g <- foidata %>%
  select(GENDER,AGE_BAND)%>%
  group_by(AGE_BAND) %>%
  summarise(
    total_count = n(),
    unknown_indetermined_count = sum(GENDER %in% c("Unknown", "Indetermined"))
  )%>%
  mutate(percentage =( unknown_indetermined_count/total_count)*100)

# Ensure AGE_BAND is ordered correctly
foidata_g <- foidata_g %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = c("Age 0-5", "Age 6-10", "Age 11-20", 
                                                "Age 21-30", "Age 31-40", "Age 41-50", 
                                                "Age 51-60", "Age 61-70", "Age 71-80", 
                                                "Age 81-90", "Age 91-100")))

# SUPPLEMENTARY FIGURE 2
# Create the bar chart
ggplot(foidata_g, aes(x = AGE_BAND, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Percentage of Unknown/Indetermined Gender by Age Band",
       x = "Age Band",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/supplementary_fig1.jpeg")



