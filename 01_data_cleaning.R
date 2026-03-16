# #####################################
# Cleaning data script.               #
# Author: Abel Kjaersgaard            #
# Date: Thu Mar 12 2026               #
# #####################################
library(tidyverse)
library(janitor)

# 1. Load and clean analysis data ---------------------------------------
## FOI data ---------------------------------------
#fmt:skip
age_band_levels <- c(
  "0-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-60",
  "61-70", "71-80", "81-90", "91-100", "over 100", "Unknown"
)

# Read FOI data for all years in directory
foi_path <- "data/foi"
foi_files <- list.files(path = foi_path, full.names = TRUE)

foi_raw <- foi_files |>
  map_df(~ read_csv(.x, show_col_types = FALSE)) |>
  clean_names() |>
  mutate(
    gender = str_to_lower(gender),
    age_band = str_remove(age_band, "Age "),
    age_band = factor(age_band, levels = age_band_levels)
  )

# Impute missing data
foi_data <- foi_raw |>
  mutate(across(
    c(unique_patient_count, items),
    ~ as.numeric(ifelse(.x == "*", 1, .x)) # stick with 1 for now 
  )) |>
  select(-financial_year)

## IMD score ---------------------------------------
# Read raw data
imd_raw <- read_csv(
  "data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  select(
    lsoa_code = lsoa_code_2011,
    lad_code = local_authority_district_code_2019,
    imd_score = index_of_multiple_deprivation_imd_score,
    population = total_population_mid_2015_excluding_prisoners
  )

# Aggregate from LSOA to LAD
imd_data <- imd_raw |>
  reframe(
    imd_score = sum(imd_score * population, na.rm = TRUE) /
      sum(population, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .by = lad_code
  ) |>
  mutate(
    imd_quintile = ntile(imd_score, 5),
    imd_quintile = paste0("Q", 6 - imd_quintile)
  )

# 2. Load reference lookup tables ---------------------------------------
## Practice-postcode lookup table ---------------------------------------
gp_to_postcode <- read_csv("data/epraccur-2.csv", show_col_types = FALSE) |>
  select(practice_code, postcode)

## Postcode-LAD lookup table ---------------------------------------
postcode_lookup <- read_csv(
  "data/PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv",
  show_col_types = FALSE
) |>
  select(pcd7, pcd8, pcds, lad_code = ladcd, lsoa_code = lsoa21cd)

# only difference in pcd is whitespace
# postcode_lookup |>
#   mutate(across(contains("pcd"), ~ str_remove_all(.x, "\\s+"))) |>
#   filter(!(pcd7 == pcd8 & pcd8 == pcds))

# 3. Geographic joining LAD ---------------------------------------
# Link FOI data at GP level to LAD level by postcode and sum 
# over both patients and items by drug, gender, age and LAD 
foi_combined <- foi_data |>
  left_join(gp_to_postcode, by = "practice_code") |>
  mutate(postcode = str_remove_all(postcode, "\\s+")) |>
  left_join(
    postcode_lookup |>
      select(pcds, lad_code) |>
      mutate(pcds = str_remove_all(pcds, "\\s+")),
    by = c("postcode" = "pcds")
  ) |>
  reframe(
    total_patients = sum(unique_patient_count, na.rm = TRUE),
    total_items = sum(items, na.rm = TRUE),
    .by = c(bnf_chemical_substance_code, gender, age_band, lad_code)
  )

# 4. Add in age and sex of population by LAD --------------------------------
ons_raw <- read_csv("data/ons_popn_lad_age_sex.csv")
ons_popn <- ons_raw |>
  clean_names() |>
  rename(lad_code = ladcode23) |>
  pivot_longer(cols = starts_with("population_"), values_to = "popn") |>
  mutate(year = str_remove(name, "population_")) |>
  group_by(lad_code) |>
  # recode f to female, m to male
  mutate(gender = recode(sex,
                      "f" = "female",
                      "m" = "male")) |>
  # add in age bands
  mutate(age_band = cut(
      age,
      breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
      labels = age_band_levels[-length(age_band_levels)], # remove Unknown
      include.lowest = TRUE,
      right = TRUE
    )
  ) |>
  group_by(age_band, gender, lad_code, year) |>
  summarise(pop_a_s = sum(popn, na.rm = TRUE), .groups = "drop") |> # population in this age / sex split
  filter(year == 2022) # to match FOI data (current) 
  
### Check = 0
sum(ons_popn$pop_a_s, na.rm = TRUE) - sum(ons_raw$population_2022, na.rm = TRUE)

# 5. Final data => baseline analysis  ---------------------------------------
# Combine IMD by LAD code with FOI data at LAD level (up from GP)
foi_combined_data_imd <- foi_combined |>
  left_join(imd_data, by = "lad_code") 
# Combine with population sizes by age and gender 
combined_data <- foi_combined_data_imd |>
  left_join(ons_popn, by = c("lad_code", "gender", "age_band"))

# Preview result
write_csv(combined_data, "data/combined_data.csv")



# 6. Geographic joining LSOA (UKHSA method) ---------------------------------------
# Link FOI data at GP level to LSOA level by postcode and sum 
# over both patients and items by drug, gender, age and LSOA 
foi_combined_lsoa <- foi_data |>
  left_join(gp_to_postcode, by = "practice_code") |>
  mutate(postcode = str_remove_all(postcode, "\\s+")) |>
  left_join(
    postcode_lookup |>
      select(pcds, lsoa_code) |>
      mutate(pcds = str_remove_all(pcds, "\\s+")),
    by = c("postcode" = "pcds")
  ) |>
  reframe(
    total_patients = sum(unique_patient_count, na.rm = TRUE),
    total_items = sum(items, na.rm = TRUE),
    .by = c(bnf_chemical_substance_code, gender, age_band, lsoa_code)
  ) 

# 7. Final data at LSOA ---------------------------------------
combined_data_lsoa <- foi_combined_lsoa |>
  left_join(imd_raw |> mutate(
    imd_quintile = ntile(imd_score, 5),
    imd_quintile = paste0("Q", 6 - imd_quintile)
  ), by = "lsoa_code")

# Preview result
write_csv(combined_data_lsoa, "data/combined_data_lsoa.csv")


# # 8. Use UKHSA IMD at GP level 
# Should we add in / use the IMD values from UKHSA? 
# Uses the LSAO where patients registered = divide by registered list? 
# deprivationdata<-read.csv("data/deprivation_data.csv")%>%
#   dplyr::select(-Sex,-Age,-Category,-Category.Type,-Lower.CI.95.0.limit,-Time.period.range,-Compared.to.goal,-New.data,-Time.period.Sortable,-Compared.to.ICB.sub.locations.value.or.percentiles,-Compared.to.England.value.or.percentiles,-Recent.Trend,-Value.note,-Denominator,-Count,-Upper.CI.99.8.limit,-Lower.CI.99.8.limit,-Upper.CI.95.0.limit,-Lower.CI.95.0.limit,-Indicator.ID,-Indicator.Name)
# deprivationdata<-subset(deprivationdata, Time.period == "2019") # Select IMD2019
# deprivationdata<-subset(deprivationdata,Area.Type=="GPs") # only use GP's
# deprivationdata <- deprivationdata %>% rename(PRACTICE_CODE = Area.Code)


# 8. Is there a difference in IMD distribution between 2 methods? ---------------------------------------
# IMD_RAW data
# LSOA selected by GP postcode? 

gp_lsoa <- combined_data_lsoa %>% 
  group_by(lsoa_code) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(source = "GP data")

# Raw LSOA data
raw_lsoa <- imd_raw %>%
  mutate(source = "Raw LSOA data")

# Combine
plot_data <- bind_rows(
  raw_lsoa %>% select(imd_score, source),
  gp_lsoa %>% select(imd_score, source)
)

# Plot
ggplot(plot_data, aes(x = imd_score, fill = source, colour = source)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100,
                 alpha = 0.2,
                 position = "identity") +
  labs(
    title = "IMD Score Distribution Comparison:\nif use only GP postcode LSOA\n then more higher IMD and fewer low IMD",
    x = "IMD Score",
    y = "Density"
  ) +
  theme_minimal()


# 9. ### Does the different combinations matter?  ---------------------------------------
# # Does the rate of items per patient vary by IMD quintile?
# UKHSA linkage:
lsoa_pop_by_quintile <- combined_data_lsoa |>
  distinct(lsoa_code, population, imd_quintile) |>
  group_by(imd_quintile) |>
  summarise(total_pop = sum(population, na.rm = TRUE))

# all populations by lsoa
full_lsoa_pop_by_quintile <- imd_raw |>
  mutate(
    imd_quintile = ntile(imd_score, 5),
    imd_quintile = paste0("Q", 6 - imd_quintile)
  ) |>
  group_by(imd_quintile) |>
  summarise(total_pop = sum(population, na.rm = TRUE))

lsoa_local_pop <- combined_data_lsoa |>
  group_by(imd_quintile) |>
  summarise(total_items = sum(total_items, na.rm = TRUE)) |>
  left_join(lsoa_pop_by_quintile, by = "imd_quintile") |>
  mutate(rate = total_items / total_pop)

lsoa_full_pop <- combined_data_lsoa |>
  group_by(imd_quintile) |>
  summarise(total_items = sum(total_items, na.rm = TRUE)) |>
  left_join(full_lsoa_pop_by_quintile, by = "imd_quintile") |>
  mutate(rate = total_items / total_pop)

### our linkage:
lad_pop_by_quintile <- combined_data |>
  distinct(lad_code, gender, age_band, pop_a_s, imd_quintile) |>
  group_by(imd_quintile) |>
  summarise(total_pop = sum(pop_a_s, na.rm = TRUE))

lad_pop <- combined_data |>
  group_by(imd_quintile) |>
  summarise(total_items = sum(total_items, na.rm = TRUE)) |>
  left_join(lad_pop_by_quintile, by = "imd_quintile") |>
  mutate(rate = total_items / total_pop)

## Compare 
bind_rows(
 # lsoa_local_pop |> mutate(method = "LSOA (GP-postcode pop)"),
  lsoa_full_pop  |> mutate(method = "LSOA (full England pop)"),
  lad_pop        |> mutate(method = "LAD linkage")
) |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = rate, fill = method)) +
  geom_col(position = "dodge") +
  facet_wrap(~ method) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y = "Items per person",
    fill = NULL,
    title = "Antibiotic prescribing rate by IMD quintile",
    subtitle = "Comparison of three linkage and denominator methods"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### 10. Age standardisation ---------------------------------------
### More old people in least deprived areas, so age standardisation is important to compare rates across IMD quintiles.
### Suggests that the higher prescribing rates in more deprived areas may be even higher than they appear, 
### once we account for the older population in less deprived areas.
# 1. Calculate the standard population (total across all LADs)
standard_pop <- combined_data |>
  distinct(lad_code, gender, age_band, pop_a_s) |>
  group_by(gender, age_band) |>
  summarise(std_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop")

# 2. Calculate stratum-specific rates within each IMD quintile
stratum_rates <- combined_data |>
  group_by(imd_quintile, gender, age_band) |>
  summarise(
    stratum_items = sum(total_items, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    combined_data |>
      distinct(lad_code, gender, age_band, pop_a_s, imd_quintile) |>
      group_by(imd_quintile, gender, age_band) |>
      summarise(stratum_pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
    by = c("imd_quintile", "gender", "age_band")
  ) |>
  mutate(stratum_rate = stratum_items / stratum_pop)

# 3. Apply standard population weights and sum to get ASR
asr <- stratum_rates |>
  left_join(standard_pop, by = c("gender", "age_band")) |>
  group_by(imd_quintile) |>
  summarise(
    asr = sum(stratum_rate * std_pop, na.rm = TRUE) / sum(std_pop, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Plot
asr |>
  filter(!is.na(imd_quintile)) |>
  ggplot(aes(x = imd_quintile, y = asr)) +
  geom_col(fill = "#4DAF4A") +
  labs(
    x = "IMD quintile (Q1 = most deprived, Q5 = least deprived)",
    y = "Age-sex standardised items per person",
    title = "Age-sex standardised antibiotic prescribing by IMD quintile",
    subtitle = "Direct standardisation, England internal standard population"
  ) +
  theme_minimal()

### But how can we see why more old in Q5 but more prescribing in Q1? 
### Effect of IMD vs age??
### Compare crude vs standardised
bind_rows(
  lad_pop |> mutate(type = "Crude rate"),
  lsoa_full_pop |> mutate(type = "Crude rate (LSOA)"),
  asr     |> mutate(rate = asr, type = "Age-sex standardised rate")
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
    subtitle = "Standardisation steepens the gradient, revealing suppression by age structure"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
