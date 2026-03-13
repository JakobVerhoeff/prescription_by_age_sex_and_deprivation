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
deprivationdata<-subset(deprivationdata, Time.period == "2019")
deprivationdata<-subset(deprivationdata,Area.Type=="GPs")
deprivationdata <- deprivationdata %>% rename(PRACTICE_CODE = Area.Code)
deprivationdata<-deprivationdata%>%
  select(PRACTICE_CODE, Value)
foidata<-foidata%>%
  left_join(deprivationdata, by ="PRACTICE_CODE")%>%
  select(-PRACTICE_CODE)
foidata$ITEMS<-as.numeric(foidata$ITEMS)
foidata$ITEMS[is.na(foidata$ITEMS)] <- 1 # I convert the NA's into 1's.
df_over_100 <- foidata[foidata$AGE_BAND == "Age over 100", ]%>%
  select(ITEMS,Value)
df_rest <- foidata[foidata$AGE_BAND != "Age over 100", ]%>%
  select(ITEMS,Value)

# Create a combined dataframe with a new column for grouping
df_rest$Group <- "Ages under 100"
df_over_100$Group <- "Ages over 100"
df_combined <- rbind(df_rest, df_over_100)

# Create the density plot with legend and updated x-axis label
ggplot(df_combined, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Ages under 100" = "blue", "Ages over 100" = "red")) +
  labs(title = "Density Plot of Values",
       x = "IMD2019 Value",
       y = "Density",
       fill = "Age Group") + # Legend title
  theme_minimal()

