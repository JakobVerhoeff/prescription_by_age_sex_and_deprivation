library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
foidata$ITEMS<-as.numeric(foidata$ITEMS)

# Create a dataframe with rows where ITEMS is NA
df_na <- foidata[is.na(foidata$ITEMS), ]%>%
  select(AGE_BAND)

# Create a dataframe with rows where ITEMS is not NA
df_not_na <- foidata[!is.na(foidata$ITEMS), ]%>%
  select(AGE_BAND)

# For df_na
df_na_prop <- as.data.frame(prop.table(table(df_na$AGE_BAND)))
colnames(df_na_prop) <- c("AGE_BAND", "Proportion")

# For df_not_na
df_not_na_prop <- as.data.frame(prop.table(table(df_not_na$AGE_BAND)))
colnames(df_not_na_prop) <- c("AGE_BAND", "Proportion")

# Add a 'Source' column to identify the data source
df_na_prop$Source <- "ITEMS = NA"
df_not_na_prop$Source <- "ITEMS != NA"

# Combine the two dataframes
df_combined <- bind_rows(df_na_prop, df_not_na_prop)

# Define the custom order for AGE_BAND
age_order <- c("Age 0-5", "Age 6-10", "Age 11-20", "Age 21-30", "Age 31-40", 
               "Age 41-50", "Age 51-60", "Age 61-70", "Age 71-80", 
               "Age 81-90", "Age 91-100", "Age over 100", "Unknown")

# Convert AGE_BAND to a factor with the specified order
df_combined$AGE_BAND <- factor(df_combined$AGE_BAND, levels = age_order)

# Create the line plot with updated legend labels
ggplot(df_combined, aes(x = AGE_BAND, y = Proportion, color = Source, group = Source)) +
  geom_line(size = 1) + # Line size
  geom_point(size = 2) + # Add points for better visibility
  scale_color_manual(
    values = c("ITEMS = NA" = "blue", "ITEMS != NA" = "red"),
    labels = c("ITEMS = NA" = "Proportion of age band in NA", 
               "ITEMS != NA" = "Proportion of age band in non-NA")
  ) +
  labs(title = "Proportion of Age Bands",
       x = "AGE_BAND",
       y = "Proportion",
       color = "Source") + # Legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #

