### Explore NA distributions by AGE BAND

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

foidata$UNIQUE_PATIENT_COUNT<-as.numeric(foidata$UNIQUE_PATIENT_COUNT)
foidata <- foidata %>% filter(GENDER %in% c("Male", "Female"))

# I want to test how the NA's are distributed across the dataset.
# Are most of the NA's in male or female?

foidata <- foidata %>% select(-UNIQUE_PATIENT_COUNT,-FINANCIAL_YEAR)
foidata$ITEMS<-as.numeric(foidata$ITEMS)

# Filter rows where ITEMS is NA
foidataNA <- foidata %>% filter(is.na(ITEMS))
foidata_no_NA <- foidata %>% filter(!is.na(ITEMS))

# first I want to check the difference in proportion of gender.
gender_prop_NA <- prop.table(table(foidataNA$GENDER))
gender_prop_no_NA <- prop.table(table(foidata_no_NA$GENDER))
gender_prop_total<- prop.table(table(foidata$GENDER))

comparison <- data.frame(
  Dataset = c("foidataNA", "foidata_no_NA","foidata"),
  Male = c(gender_prop_NA["Male"], gender_prop_no_NA["Male"],gender_prop_total["Male"]),
  Female = c(gender_prop_NA["Female"], gender_prop_no_NA["Female"],gender_prop_total["Female"])
)
# Comparison contains for male and female, the ratio they occupy in the full dataset,
# in a dataset with only redacted items, and in a dataset with no redacted items. This
# table shows rows, and not total items.


