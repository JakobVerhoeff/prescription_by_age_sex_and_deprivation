# #####################################
# Data exploration script             #
# Author: Gwen Knight                 #
# Date: Fri Mar 13 2026               #
# Updated: Mon Jun 09 2026            #
# #####################################
library(tidyverse)
library(janitor)
library(readxl)

# =========================================================================
# 0. Load data  -----------------------------------------------------------
# =========================================================================
# LAD-level combined data (from 01_data_cleaning.R)
combined_data <- read_csv("data/combined_data.csv")

# ONS population by IMD decile, age and sex (for demographic exploration)
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/12386populationbyindexofmultipledeprivationimdengland2001to2019
imd_pops <- read_csv("data/popsbyimdengland20012019.csv")[, 1:23] %>%
  select(-year, imd:gender) %>%
  pivot_longer(cols = `<1`:`90+`, names_to = "age", values_to = "popn") %>%
  mutate(
    age_band = dplyr::recode(
      age,
      "<1" = "0-5",
      "01-04" = "0-5",
      "05-09" = "6-10",
      "10-14" = "11-20",
      "15-19" = "11-20",
      "20-24" = "21-30",
      "25-29" = "21-30",
      "30-34" = "31-40",
      "35-39" = "31-40",
      "40-44" = "41-50",
      "45-49" = "41-50",
      "50-54" = "51-60",
      "55-59" = "51-60",
      "60-64" = "61-70",
      "65-69" = "61-70",
      "70-74" = "71-80",
      "75-79" = "71-80",
      "80-84" = "81-90",
      "85-89" = "81-90",
      "90+" = "91-100"
    ),
    gender = recode(gender, "males" = "man", "females" = "woman")
  )

age_band_levels <- c(
  "0-5",
  "6-10",
  "11-20",
  "21-30",
  "31-40",
  "41-50",
  "51-60",
  "61-70",
  "71-80",
  "81-90",
  "91-100"
)

# =========================================================================
# A. Population size by LAD  ----------------------------------------------
# =========================================================================
ons_popn <- read_csv("data/ons_popn_lad_age_sex.csv") |>
  clean_names() |>
  rename(lad_code = ladcode23) |>
  pivot_longer(cols = starts_with("population_"), values_to = "popn") %>%
  mutate(year = str_remove(name, "population_")) %>%
  group_by(lad_code) |>
  summarise(population = sum(popn, na.rm = TRUE))

ggplot(ons_popn, aes(x = population)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Population Distribution by LAD",
    x = "Population",
    y = "Count of LADs"
  ) +
  theme_minimal()

ons_popn %>% filter(population < 100000) # E06000053 = Isle of Scilly
summary(ons_popn$population, na.rm = TRUE)

# =========================================================================
# B. Age/sex distribution by IMD  -----------------------------------------
# =========================================================================
imd_pops_sex <- imd_pops %>%
  group_by(gender, imd, age_band) %>%
  summarise(popn = sum(popn), .groups = "drop")
imd_pops_sex$split10 <- paste0("Q", imd_pops_sex$imd)
imd_pops_sex$split10 <- factor(imd_pops_sex$split10, levels = paste0("Q", 1:10))
imd_pops_sex$age_band <- factor(imd_pops_sex$age_band, levels = age_band_levels)

# B1. Proportions by IMD decile and sex
ggplot(imd_pops_sex, aes(x = split10, y = popn, fill = age_band)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~gender) +
  labs(
    x = "IMD Decile (Q1 = most deprived)",
    y = "Population proportion",
    fill = "Age Band"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/age_sex_distribution_by_IMD_prop.pdf", width = 10, height = 5)

# B2. Absolute numbers by IMD decile and sex
ggplot(imd_pops_sex, aes(x = split10, y = popn, fill = age_band)) +
  geom_bar(stat = "identity") +
  facet_wrap(~gender) +
  labs(
    x = "IMD Decile (Q1 = most deprived)",
    y = "Population size",
    fill = "Age Band"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/age_sex_distribution_by_IMD_numbers.pdf", width = 10, height = 5)

# B3. Population pyramids by IMD decile (proportional)
imd_pyramid <- imd_pops_sex %>%
  group_by(imd) %>%
  mutate(prop = popn / sum(popn)) %>%
  ungroup() %>%
  mutate(prop = ifelse(gender == "man", -prop, prop))

# Selected deciles: most deprived, middle, least deprived
ggplot(
  imd_pyramid %>% filter(imd %in% c(1, 5, 10)),
  aes(x = age_band, y = prop, fill = gender)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~split10, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(abs(x) * 100, "%")) +
  scale_fill_manual(values = c("woman" = "#E76F51", "man" = "#264653")) +
  labs(x = "Age Band", y = "Proportion of decile population", fill = "Gender") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines"))
ggsave("plots/pyramid_by_IMD_selected.pdf", width = 10, height = 5)

# All deciles
ggplot(imd_pyramid, aes(x = age_band, y = prop, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~split10, ncol = 5) +
  scale_y_continuous(labels = function(x) paste0(abs(x) * 100, "%")) +
  scale_fill_manual(values = c("woman" = "#E76F51", "man" = "#264653")) +
  labs(x = "Age Band", y = "Proportion of decile population", fill = "Gender") +
  theme_minimal() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.text.x = element_text(size = 7)
  )
ggsave("plots/pyramid_by_IMD_all.pdf", width = 12, height = 8)

# B4. Overlay pyramid: Q1 vs Q5
imd_overlay <- imd_pyramid %>%
  filter(imd %in% c(1, 5)) %>%
  mutate(group = paste0(gender, " - ", split10))

ggplot(imd_overlay, aes(x = age_band, y = prop, fill = group)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(abs(x) * 100, "%")) +
  scale_fill_manual(
    values = c(
      "woman - Q1" = "#E76F51",
      "woman - Q5" = "#F4A896",
      "man - Q1" = "#264653",
      "man - Q5" = "#7BA7B5"
    )
  ) +
  labs(x = "Age Band", y = "Proportion of decile population", fill = "") +
  theme_minimal()
ggsave("plots/pyramid_Q1_vs_Q5_overlay.pdf", width = 8, height = 5)

# B5. Overlay pyramid: absolute numbers
imd_overlay_abs <- imd_pops_sex %>%
  filter(imd %in% c(1, 5)) %>%
  mutate(
    popn = ifelse(gender == "man", -popn, popn),
    group = paste0(gender, " - ", split10)
  )

ggplot(imd_overlay_abs, aes(x = age_band, y = popn, fill = group)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  scale_fill_manual(
    values = c(
      "woman - Q1" = "#E76F51",
      "woman - Q5" = "#F4A896",
      "man - Q1" = "#264653",
      "man - Q5" = "#7BA7B5"
    )
  ) +
  labs(x = "Age Band", y = "Population", fill = "") +
  theme_minimal()
ggsave("plots/pyramid_Q1_vs_Q5_overlay_absolute.pdf", width = 8, height = 5)

# B6. Gender ratio comparison across deciles
sex_props <- imd_pops_sex %>%
  group_by(imd, split10, gender) %>%
  summarise(popn = sum(abs(popn)), .groups = "drop") %>%
  pivot_wider(names_from = gender, values_from = popn) %>%
  mutate(pct_man = round(100 * man / (man + woman), 2), total = man + woman)
print("Gender ratio by IMD decile")
print(sex_props)

# Two-proportion z-test: proportion male in Q1 vs Q5
sex_q1q5 <- sex_props %>% filter(imd %in% c(1, 5))
prop.test(x = sex_q1q5$man, n = sex_q1q5$total)

# B7. Age structure summary across deciles
age_summary <- imd_pops_sex %>%
  group_by(imd, split10) %>%
  mutate(total = sum(abs(popn))) %>%
  ungroup() %>%
  mutate(
    age_group = case_when(
      age_band %in% c("0-5", "6-10", "11-20") ~ "young (0-20)",
      age_band %in% c("71-80", "81-90", "91-100") ~ "elderly (71+)",
      TRUE ~ "working age (21-70)"
    )
  ) %>%
  group_by(imd, split10, age_group, total) %>%
  summarise(popn = sum(abs(popn)), .groups = "drop") %>%
  mutate(pct = round(100 * popn / total, 1))
print("Age structure summary by IMD decile")
print(age_summary)

# =========================================================================
# C. Antibiotic prescribing by IMD, age, sex, and family  -----------------
# =========================================================================
# Prepare data: aggregate prescribing with population denominators
prescribing <- combined_data |>
  filter(!is.na(antibiotic_group), !is.na(imd_quintile), !is.na(pop_a_s)) |>
  mutate(
    age_band = factor(age_band, levels = age_band_levels),
    imd_quintile = factor(
      imd_quintile,
      levels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    )
  )

# Population denominators (independent of antibiotic group to avoid double-counting)
pop_denominators <- prescribing |>
  distinct(lad_code, gender, age_band, imd_quintile, year, pop_a_s)

# C1. Overall prescribing rate by IMD quintile
overall_by_imd <- prescribing |>
  group_by(imd_quintile, year) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(imd_quintile, year) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("imd_quintile", "year")
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

ggplot(overall_by_imd, aes(x = imd_quintile, y = rate_per_1000)) +
  geom_col() +
  facet_wrap(~year) +
  labs(
    x = "IMD Quintile (Q1 = most deprived)",
    y = "Items per 1,000 population"
  ) +
  theme_minimal()
ggsave("plots/prescribing_rate_by_IMD.pdf", width = 10, height = 6)

# C2. Prescribing rate by antibiotic family and IMD quintile
by_family_imd <- prescribing |>
  group_by(antibiotic_group, imd_quintile) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(imd_quintile) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = "imd_quintile"
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

ggplot(
  by_family_imd,
  aes(x = imd_quintile, y = rate_per_1000, fill = antibiotic_group)
) +
  geom_col() +
  facet_wrap(~antibiotic_group, scales = "free_y") +
  labs(
    x = "IMD Quintile (Q1 = most deprived)",
    y = "Items per 1,000 population",
    fill = "Antibiotic Family"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("plots/prescribing_by_family_and_IMD.pdf", width = 12, height = 8)

# C3. Prescribing rate by antibiotic family, IMD quintile and sex
by_family_imd_sex <- prescribing |>
  group_by(antibiotic_group, imd_quintile, gender) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(imd_quintile, gender) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("imd_quintile", "gender")
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

ggplot(
  by_family_imd_sex,
  aes(x = imd_quintile, y = rate_per_1000, colour = gender, group = gender)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~antibiotic_group, scales = "free_y") +
  labs(
    x = "IMD Quintile (Q1 = most deprived)",
    y = "Items per 1,000 population",
    colour = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/prescribing_by_family_IMD_sex.pdf", width = 12, height = 8)

# C4. Prescribing rate by antibiotic family, age band and IMD quintile
by_family_age_imd <- prescribing |>
  group_by(antibiotic_group, age_band, imd_quintile) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(age_band, imd_quintile) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("age_band", "imd_quintile")
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

ggplot(
  by_family_age_imd,
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = imd_quintile,
    group = imd_quintile
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~antibiotic_group, scales = "free_y") +
  scale_colour_viridis_d(option = "D") +
  labs(
    x = "Age Band",
    y = "Items per 1,000 population",
    colour = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
ggsave("plots/prescribing_by_family_age_IMD.pdf", width = 14, height = 8)

# C4b. Same as C4 but split by gender
by_family_age_imd_sex <- prescribing |>
  group_by(antibiotic_group, age_band, imd_quintile, gender) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(age_band, imd_quintile, gender) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("age_band", "imd_quintile", "gender")
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

ggplot(
  by_family_age_imd_sex,
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = imd_quintile,
    linetype = gender,
    group = interaction(imd_quintile, gender)
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~antibiotic_group, scales = "free_y") +
  scale_colour_viridis_d(option = "D") +
  scale_linetype_manual(values = c("woman" = "dashed", "man" = "solid")) +
  labs(
    x = "Age Band",
    y = "Items per 1,000 population",
    colour = "IMD Quintile",
    linetype = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
ggsave("plots/prescribing_by_family_age_IMD_sex.pdf", width = 14, height = 8)

ggplot(
  by_family_age_imd_sex,
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = gender,
    group = interaction(imd_quintile, antibiotic_group, gender)
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(antibiotic_group ~ imd_quintile, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Age Band",
    y = "Items per 1,000 population",
    colour = "Gender",
    linetype = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )
ggsave(
  "plots/prescribing_by_family_age_IMD_sex_panels.pdf",
  width = 16,
  height = 12
)

ggplot(
  by_family_age_imd_sex %>% filter(imd_quintile %in% c("Q1", "Q5")),
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = imd_quintile,
    group = interaction(imd_quintile, antibiotic_group, gender)
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(antibiotic_group ~ gender, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age Band", y = "Items per 1,000 population", colour = "IMD") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )
ggsave(
  "plots/prescribing_by_family_age_IMD_sex_group_panels.pdf",
  width = 16,
  height = 12
)

# Selected antibiotic families only
key_families <- c(
  "Penicillins",
  "Macrolides",
  "Tetracyclines",
  "UTIs",
  "Quinolones",
  "Ceph's",
  "MTO"
)
ggplot(
  by_family_age_imd_sex %>%
    filter(antibiotic_group %in% key_families, imd_quintile %in% c("Q1", "Q5")),
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = imd_quintile,
    group = interaction(imd_quintile, antibiotic_group, gender)
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(antibiotic_group ~ gender, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age Band", y = "Items per 1,000 population", colour = "IMD") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )
ggsave(
  "plots/prescribing_by_family_age_IMD_sex_key_families.pdf",
  width = 16,
  height = 12
)

# Selected age bands only
ggplot(
  by_family_age_imd_sex %>%
    filter(age_band %in% c("11-20", "21-30", "31-40", "41-50", "51-60")),
  aes(
    x = age_band,
    y = rate_per_1000,
    colour = imd_quintile,
    group = interaction(imd_quintile, antibiotic_group, gender)
  )
) +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(antibiotic_group ~ gender, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Age Band", y = "Items per 1,000 population", colour = "IMD") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )
ggsave(
  "plots/prescribing_by_family_age_IMD_sex_group_panels.pdf",
  width = 16,
  height = 12
)


# C4c. Year-specific version of C4b (one PDF per year)
by_family_age_imd_sex_yr <- prescribing |>
  group_by(antibiotic_group, age_band, imd_quintile, gender, year) |>
  summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop") |>
  left_join(
    pop_denominators |>
      group_by(age_band, imd_quintile, gender, year) |>
      summarise(total_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("age_band", "imd_quintile", "gender", "year")
  ) |>
  mutate(rate_per_1000 = total_items / total_pop * 1000)

for (yr in sort(unique(by_family_age_imd_sex_yr$year))) {
  ggplot(
    by_family_age_imd_sex_yr |> filter(year == yr),
    aes(
      x = age_band,
      y = rate_per_1000,
      colour = imd_quintile,
      linetype = gender,
      group = interaction(imd_quintile, gender)
    )
  ) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~antibiotic_group, scales = "free_y") +
    scale_colour_viridis_d(option = "D") +
    scale_linetype_manual(values = c("woman" = "dashed", "man" = "solid")) +
    labs(
      x = "Age Band",
      y = "Items per 1,000 population",
      colour = "IMD Quintile",
      linetype = "Gender",
      title = yr
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  ggsave(
    paste0("plots/prescribing_by_family_age_IMD_sex_", yr, ".pdf"),
    width = 14,
    height = 8
  )
}

# C5. Composition: which antibiotic families make up prescribing by IMD?
ggplot(
  by_family_imd,
  aes(x = imd_quintile, y = rate_per_1000, fill = antibiotic_group)
) +
  geom_col(position = "fill") +
  labs(
    x = "IMD Quintile (Q1 = most deprived)",
    y = "Proportion of prescribing",
    fill = "Antibiotic Family"
  ) +
  theme_minimal()
ggsave("plots/prescribing_composition_by_IMD.pdf", width = 8, height = 5)
