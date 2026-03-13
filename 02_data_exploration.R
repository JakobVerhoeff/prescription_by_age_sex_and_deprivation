# #####################################
# Data exploration scrip             #
# Author: Gwen Knight            #
# Date: Fri Mar 13 2026               #
# #####################################
library(tidyverse)
library(janitor)

### 1. What is the variation in population sizes by LAD? 
ons_popn <- read_csv("data/ons_popn_lad_age_sex.csv") |>
  clean_names() |>
  rename(lad_code = ladcode23) |>
  pivot_longer(cols = starts_with("population_"), values_to = "popn") %>%
  mutate(year = str_remove(name, "population_")) %>%
  group_by(lad_code) |>
  summarise(population = sum(popn, na.rm = TRUE))

ggplot(ons_popn, aes(x=population)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Population Distribution by LAD",
    x = "Population",
    y = "Count of LADs"
  ) +
  theme_minimal()

ons_popn %>% filter(population < 100000) # E06000053 = Isle of Scily 

summary(ons_popn$population, na.rm = TRUE)
