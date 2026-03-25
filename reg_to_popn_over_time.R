##### registrations to population over time

library(tidyverse)
library(here)
library(scales)

# ============================================================
# 1. READ AND PROCESS GP REGISTRATION FILES
# ============================================================

gp_files <- list.files(
  path = here("data/registration_to_popn/"), 
  pattern = "^gp-reg-pat", 
  full.names = TRUE
)

# Read and combine all 5 files
gp_raw <- map_dfr(gp_files, read_csv) %>%
  # Extract date/year from EXTRACT_DATE
  mutate(
    EXTRACT_DATE = dmy(EXTRACT_DATE),
    year = year(EXTRACT_DATE)
  )

# Filter to practice-level, single year of age, male/female only
gp_clean <- gp_raw %>%
  filter(
    ORG_TYPE == "PCN",
    SEX %in% c("FEMALE", "MALE"),
    AGE != "ALL",
    !is.na(AGE)
  ) %>%
  mutate(
    AGE = if_else(AGE == "95+", "95", AGE),
    AGE = as.numeric(AGE),
    gender = tolower(SEX)
  ) %>%
  filter(!is.na(AGE)) %>%
  # Assign 5-year age bands
  mutate(
    age_band = case_when(
      AGE >= 0  & AGE <= 4  ~ "0-4",
      AGE >= 5  & AGE <= 9  ~ "5-9",
      AGE >= 10 & AGE <= 14 ~ "10-14",
      AGE >= 15 & AGE <= 19 ~ "15-19",
      AGE >= 20 & AGE <= 24 ~ "20-24",
      AGE >= 25 & AGE <= 29 ~ "25-29",
      AGE >= 30 & AGE <= 34 ~ "30-34",
      AGE >= 35 & AGE <= 39 ~ "35-39",
      AGE >= 40 & AGE <= 44 ~ "40-44",
      AGE >= 45 & AGE <= 49 ~ "45-49",
      AGE >= 50 & AGE <= 54 ~ "50-54",
      AGE >= 55 & AGE <= 59 ~ "55-59",
      AGE >= 60 & AGE <= 64 ~ "60-64",
      AGE >= 65 & AGE <= 69 ~ "65-69",
      AGE >= 70 & AGE <= 74 ~ "70-74",
      AGE >= 75 & AGE <= 79 ~ "75-79",
      AGE >= 80 & AGE <= 84 ~ "80-84",
      AGE >= 85 & AGE <= 89 ~ "85-89",
      AGE >= 90             ~ "90+",
      TRUE ~ NA_character_
    ),
    age_band = factor(age_band, levels = c(
      "0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
      "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
      "75-79","80-84","85-89","90+"
    ))
  ) %>%
  filter(!is.na(age_band)) %>%
  group_by(year, gender, age_band) %>%
  summarise(tot_reg = sum(NUMBER_OF_PATIENTS, na.rm = TRUE), .groups = "drop")

gp_clean %>% group_by(year) %>% 
  summarise(total_reg = sum(tot_reg)) %>% 
  print()

# ============================================================
# 2. READ AND PROCESS ONS POPULATION DENOMINATOR
# ============================================================
ons_raw <- read_csv(here("data/registration_to_popn", "ons_2024 dep_pop.csv"))

# Check structure first
glimpse(ons_raw)

# Expected: wide format with columns: level code, level name, level, year, gender, under 1, 1, 2 ... 90
# Pivot to long
ons_long <- ons_raw %>%
  # Pivot all age columns (under 1 through 90) to long
  pivot_longer(
    cols = "0":"90",
    names_to = "age_raw",
    values_to = "population"
  ) %>%
  mutate(
    # Convert age labels to numeric
    AGE = case_when(
      age_raw == "under_1" ~ 0,
      TRUE ~ as.numeric(age_raw)
    ),
    gender = tolower(gender)
  ) %>%
  filter(gender %in% c("female", "male")) %>%
  # Same 5-year age bands
  mutate(
    age_band = case_when(
      AGE >= 0  & AGE <= 4  ~ "0-4",
      AGE >= 5  & AGE <= 9  ~ "5-9",
      AGE >= 10 & AGE <= 14 ~ "10-14",
      AGE >= 15 & AGE <= 19 ~ "15-19",
      AGE >= 20 & AGE <= 24 ~ "20-24",
      AGE >= 25 & AGE <= 29 ~ "25-29",
      AGE >= 30 & AGE <= 34 ~ "30-34",
      AGE >= 35 & AGE <= 39 ~ "35-39",
      AGE >= 40 & AGE <= 44 ~ "40-44",
      AGE >= 45 & AGE <= 49 ~ "45-49",
      AGE >= 50 & AGE <= 54 ~ "50-54",
      AGE >= 55 & AGE <= 59 ~ "55-59",
      AGE >= 60 & AGE <= 64 ~ "60-64",
      AGE >= 65 & AGE <= 69 ~ "65-69",
      AGE >= 70 & AGE <= 74 ~ "70-74",
      AGE >= 75 & AGE <= 79 ~ "75-79",
      AGE >= 80 & AGE <= 84 ~ "80-84",
      AGE >= 85 & AGE <= 89 ~ "85-89",
      AGE >= 90             ~ "90+",
      TRUE ~ NA_character_
    ),
    age_band = factor(age_band, levels = c(
      "0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
      "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
      "75-79","80-84","85-89","90+"
    ))
  ) %>%
  filter(!is.na(age_band)) %>%
  group_by(year, gender, age_band) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), .groups = "drop")


# ============================================================
# 3. JOIN AND CALCULATE RATIO
# ============================================================

ratio_df <- left_join(gp_clean, ons_long, 
                      by = c("year", "gender", "age_band")) %>%
  mutate(ratio = tot_reg / tot_pop)

# Sense check
ratio_df %>% 
  group_by(year) %>% 
  summarise(
    total_reg = sum(tot_reg),
    total_pop = sum(tot_pop),
    overall_ratio = total_reg / total_pop
  ) %>% 
  print()


# ============================================================
# 4. PLOTS
# ============================================================

# --- Plot 1: ratio by age band and year, faceted by gender ---
ggplot(ratio_df, aes(x = age_band, y = ratio, 
                     colour = factor(year), group = factor(year))) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "C") +
  scale_x_discrete("Age band") +
  scale_y_continuous("Ratio: registrations / population") +
  labs(
    colour = "Year",
    title  = "GP registrations vs population by age band over time",
    subtitle = "Ratio > 1 = more registered than population estimate"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot 2: change over time for selected age bands ---
ggplot(ratio_df, aes(x = year, y = ratio, 
                     colour = age_band, group = age_band)) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "C") +
  scale_y_continuous("Ratio: registrations / population") +
  labs(
    colour  = "Age band",
    title   = "Registration-to-population ratio over time",
    subtitle = "By age band and gender"
  ) +
  theme_bw()
