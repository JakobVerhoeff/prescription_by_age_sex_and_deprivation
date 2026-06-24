# #####################################
# Linkage method comparison script    #
# Crude vs age-standardised rates     #
# by IMD quintile, LAD vs LSOA        #
# Author: Gwen Knight / Abel          #
# Date: Mon Jun 09 2026               #
# #####################################
library(tidyverse)

# =========================================================================
# 0. Load data  -----------------------------------------------------------
# =========================================================================
combined_data <- read_csv("data/combined_data.csv")
combined_data_lsoa <- read_csv("data/combined_data_lsoa.csv")

# Raw IMD for full LSOA population denominators
imd_raw <- read_csv(
  "data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv",
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  select(
    lsoa_code = lsoa_code_2011,
    lad_code = local_authority_district_code_2019,
    imd_score = index_of_multiple_deprivation_imd_score,
    population = total_population_mid_2015_excluding_prisoners
  )

# =========================================================================
# 1. Crude rates by linkage method  ---------------------------------------
# =========================================================================

# LSOA linkage: full England LSOA populations as denominator
full_lsoa_pop_by_quintile <- imd_raw |>
  mutate(
    imd_quintile = ntile(imd_score, 5),
    imd_quintile = paste0("Q", 6 - imd_quintile)
  ) |>
  group_by(imd_quintile) |>
  summarise(total_pop = sum(population, na.rm = TRUE))

lsoa_full_pop <- combined_data_lsoa |>
  group_by(imd_quintile, year) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(full_lsoa_pop_by_quintile, by = "imd_quintile") |>
  mutate(rate = total_items / total_pop)

# LAD linkage: ONS population by age/sex as denominator (year-specific)
lad_pop_by_quintile <- combined_data |>
  distinct(lad_code, gender, age_band, pop_a_s, imd_quintile, year) |>
  group_by(imd_quintile, year) |>
  summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop")

lad_pop <- combined_data |>
  group_by(imd_quintile, year) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(lad_pop_by_quintile, by = c("imd_quintile", "year")) |>
  mutate(rate = total_items / total_pop)

# Compare crude rates
bind_rows(
  lsoa_full_pop |> mutate(method = "LSOA (full England pop)"),
  lad_pop       |> mutate(method = "LAD linkage")
) |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = rate, fill = method)) +
  geom_col(position = "dodge") +
  facet_wrap(~year) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y = "Items per person",
    fill = NULL,
    title = "Antibiotic prescribing rate by IMD quintile",
    subtitle = "Comparison of linkage and denominator methods"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/crude_rate_linkage_comparison.pdf", width = 12, height = 8)

# =========================================================================
# 2. Age-sex standardisation (LAD linkage)  -------------------------------
# =========================================================================

# Standard population (total across all LADs)
standard_pop <- combined_data |>
  distinct(lad_code, gender, age_band, pop_a_s, year) |>
  group_by(gender, age_band, year) |>
  summarise(std_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop")

# Stratum-specific rates within each IMD quintile (year-specific denominators)
stratum_rates <- combined_data |>
  group_by(imd_quintile, gender, age_band, year) |>
  summarise(
    stratum_items = sum(total_items, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    combined_data |>
      distinct(lad_code, gender, age_band, pop_a_s, imd_quintile, year) |>
      group_by(imd_quintile, gender, age_band, year) |>
      summarise(stratum_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("imd_quintile", "gender", "age_band", "year")
  ) |>
  mutate(stratum_rate = stratum_items / stratum_pop)

# Age-sex standardised rate
asr <- stratum_rates |>
  left_join(standard_pop, by = c("gender", "age_band", "year")) |>
  group_by(imd_quintile, year) |>
  summarise(
    asr = sum(stratum_rate * std_pop, na.rm = TRUE) / sum(std_pop, na.rm = TRUE),
    .groups = "drop"
  )

# ASR by IMD quintile
asr |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = asr)) +
  geom_col(fill = "#4DAF4A") +
  facet_wrap(~year) +
  labs(
    x = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y = "Age-sex standardised items per person",
    title = "Age-sex standardised antibiotic prescribing by IMD quintile",
    subtitle = "Direct standardisation, England internal standard population"
  ) +
  theme_minimal()
ggsave("plots/asr_by_IMD.pdf", width = 10, height = 6)

# =========================================================================
# 3. Crude vs age-sex standardised comparison  ----------------------------
# =========================================================================
bind_rows(
  lad_pop       |> mutate(type = "Crude rate (LAD, our linkage)"),
  lsoa_full_pop |> mutate(type = "Crude rate (LSOA, UKHSA method)"),
  asr           |> mutate(rate = asr, type = "Age-sex standardised rate")
) |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = rate, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x    = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y    = "Items per person",
    fill = NULL,
    title = "Crude vs age-sex standardised antibiotic prescribing by IMD quintile",
    subtitle = "Age-sex standardisation steepens gradient vs crude LAD; LSOA-level IMD shows steepest gradient"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~year)
ggsave("plots/crude_vs_asr_by_IMD.pdf", width = 12, height = 8)

# Faceted by type and year for clearer comparison
bind_rows(
  lad_pop       |> mutate(type = "Crude rate (LAD, our linkage)"),
  lsoa_full_pop |> mutate(type = "Crude rate (LSOA, UKHSA method)"),
  asr           |> mutate(rate = asr, type = "Age-sex standardised rate")
) |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = rate, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x    = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y    = "Items per person",
    fill = NULL,
    title = "Crude vs age-sex standardised antibiotic prescribing by IMD quintile",
    subtitle = "Age-sex standardisation steepens gradient vs crude LAD; LSOA-level IMD shows steepest gradient"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_grid(type ~ year, scales = "free")
ggsave("plots/crude_vs_asr_by_IMD_faceted.pdf", width = 14, height = 10)
