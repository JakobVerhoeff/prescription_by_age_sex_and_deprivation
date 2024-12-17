library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
setwd("C:/Users/Jakob/Documents/Research/Actual research")
foidata<-read.csv("data/FOIdata/foi02243_practice_2022_2023.csv")
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

# now do a test to check if the difference in proportions is statistically significant
# Proportions of Male and Female in foidataNA
male_prop_NA <- sum(foidataNA$GENDER == "Male", na.rm = TRUE) / length(foidataNA$GENDER)
female_prop_NA <- sum(foidataNA$GENDER == "Female", na.rm = TRUE) / length(foidataNA$GENDER)

# Sample sizes for foidataNA
n_NA <- length(foidataNA$GENDER)
# Proportions of Male and Female in foidata
male_prop_total <- sum(foidata$GENDER == "Male", na.rm = TRUE) / length(foidata$GENDER)
female_prop_total <- sum(foidata$GENDER == "Female", na.rm = TRUE) / length(foidata$GENDER)

# Sample sizes for foidata
n_total <- length(foidata$GENDER)

# Perform a two-sample Z-Test for proportions

###  MANUAL Z-TEST
p1 <- 0.4788548
p2 <- 0.4453842
n1 <- 1257095
n2 <- 2483218

# Pooled proportion
pooled_p <- ((p1 * n1) + (p2 * n2)) / (n1 + n2)

# Standard error
SE <- sqrt(pooled_p * (1 - pooled_p) * ((1 / n1) + (1 / n2)))

# Z-statistic
z_stat <- (p1 - p2) / SE

# P-value (two-tailed)
p_value <- 2 * (1 - pnorm(abs(z_stat)))

cat("Z-statistic:", z_stat, "\nP-value:", p_value)

# this two-sample Z-Test gives a p-value of 2.2e-16, which states a significant difference
# between the proportion of male/female in the two datasets. This means we can't
# assume that the two datasets are equally distributed in gender and there are
# significantly more Male's in the NA dataframe. That means that in our original
# dataframe foidata, there are significantly more instances of ITEMS missing in Males than in Females.


