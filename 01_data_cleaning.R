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
    ~ as.numeric(ifelse(.x == "*", 2.5, .x))
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
  select(pcd7, pcd8, pcds, lad_code = ladcd)

# only difference in pcd is whitespace
# postcode_lookup |>
#   mutate(across(contains("pcd"), ~ str_remove_all(.x, "\\s+"))) |>
#   filter(!(pcd7 == pcd8 & pcd8 == pcds))

# 3. Geographic joining ---------------------------------------
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

# 4. Final data ---------------------------------------
combined_data <- foi_combined |>
  left_join(imd_data, by = "lad_code")

rm(list = setdiff(ls(), "combined_data"))

# Preview result
write_csv(combined_data, "data/combined_data.csv")
