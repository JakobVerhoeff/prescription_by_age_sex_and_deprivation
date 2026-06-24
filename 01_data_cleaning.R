# #####################################
# Cleaning data script                #
# Author: Abel Kjaersgaard            #
# R version: 4.5.2                    #
# Date: Thu Mar 12 2026               #
# #####################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readxl)
})

# 1. Load and clean analysis data ---------------------------------------
## FOI data ---------------------------------------
#fmt:skip
age_band_levels <- c(
  "0-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-60",
  "61-70", "71-80", "81+", "Unknown"
)

# Read FOI data for all years in directory
foi_path <- "data/foi"
foi_files <- list.files(path = foi_path, full.names = TRUE)

foi_raw <- foi_files |>
  map(~ read_csv(.x, show_col_types = FALSE)) |>
  list_rbind() |>
  clean_names()

# Impute missing data

foi_data <- foi_raw |>
  mutate(
    gender = recode(str_to_lower(gender), "female" = "woman", "male" = "man"), # recode sex/gender variables
    age_band = str_remove(age_band, "Age "),
    age_band = recode(
      age_band,
      "81-90" = "81+",
      "91-100" = "81+",
      "over 100" = "81+"
    ), # ONS has 90+ as max
    age_band = factor(age_band, levels = age_band_levels)
  ) |>
  mutate(across(
    c(unique_patient_count, items),
    ~ as.numeric(ifelse(.x == "*", 1, .x)) # stick with 1 for now
  )) |>
  # Extract first year from financial year/second year
  mutate(year = as.integer(str_extract(financial_year, "^\\d{4}"))) |>
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
      sum(population, na.rm = TRUE), # LAD IMD = population weighted average of LSOA IMD
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
    .by = c(bnf_chemical_substance_code, gender, age_band, lad_code, year)
  ) |>
  filter(!is.na(lad_code) & str_starts(lad_code, "E")) # England only

# 4. Add in age and sex of population by LAD --------------------------------
ons_raw <- read_csv("data/ons_popn_lad_age_sex.csv")
ons_data <- ons_raw |>
  clean_names() |>
  rename(lad_code = ladcode23) |>
  pivot_longer(cols = starts_with("population_"), values_to = "popn") |>
  mutate(year = as.numeric(str_remove(name, "population_"))) |>
  # recode f/m to man/woman
  mutate(gender = recode(sex, "f" = "woman", "m" = "man")) |>
  # add in age bands (81+ combines ONS 81-89 and 90+ to match FOI)
  mutate(
    age_band = cut(
      age,
      breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
      labels = age_band_levels[-length(age_band_levels)],
      include.lowest = TRUE,
      right = TRUE
    ),
    age_band = fct_na_value_to_level(age_band, level = "Unknown")
  ) |>
  reframe(
    pop_a_s = sum(popn, na.rm = TRUE),
    .by = c(age_band, gender, lad_code, year)
  ) #|> # population in this age / sex split
#filter(year == 2022) # to match FOI data (current)

### Check = 0
# sum(ons_data %>% filter(year == 2022) %>% select(pop_a_s), na.rm = TRUE) - sum(ons_raw$population_2022, na.rm = TRUE)

# 5. Final data => baseline analysis  ---------------------------------------
# Combine IMD by LAD code with FOI data at LAD level (up from GP)
foi_combined_data_imd <- foi_combined |>
  left_join(imd_data, by = "lad_code") # IMD 2019 - same across all years
# Combine with population sizes by age and gender
combined_data <- foi_combined_data_imd |>
  left_join(ons_data, by = c("lad_code", "gender", "age_band", "year"))

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
    .by = c(bnf_chemical_substance_code, gender, age_band, lsoa_code, year)
  ) |>
  filter(!is.na(lsoa_code), str_starts(lsoa_code, "E")) # England only

# 7. Final data at LSOA ---------------------------------------
combined_data_lsoa <- foi_combined_lsoa |>
  left_join(
    imd_raw |>
      mutate(
        imd_quintile = ntile(imd_score, 5),
        imd_quintile = paste0("Q", 6 - imd_quintile)
      ),
    by = "lsoa_code"
  )

# 8. Antibiotic group mapping  ---------------------------------------
# Map BNF codes to antibiotic families (from reference table)
#fmt:skip
{
CnL          <- c("0501060D0","0501060E0")
Cephs        <- c("0501023A0","0501021A0","0501021B0","0501021L0","0501024A0",
                  "0501021C0","0501021D0","0501021E0","050102020","0501021F0",
                  "0501021M0","0501021H0","0501021G0","0501021K0","0501021J0",
                  "0501022C0","0501022B0","0501022D0","0501022A0")
Lep          <- c("0501100H0","0501100J0","0501100C0")
Macrolides   <- c("0501050N0","0501050H0","0501050B0","0501050A0","0501050C0","0501050K0")
MTO          <- c("0501110C0","0501110G0")
Penicillins  <- c("0501015P0","0501011P0","0501012G0","0501013K0","0501013B0",
                  "0501011J0","0501012H0","0501012U0","0501013C0","0501013E0",
                  "0501013L0","0501014N0","0501014S0")
Quinolones   <- c("0501120P0","0501120X0","0501120L0","0501120Y0","0501120Q0","0501120N0")
SnT          <- c("0501080W0","0501080D0","0501080V0","0501080T0","0501080J0")
TB           <- c("0501090R0","0501090K0","0501090V0","0501090U0","0501090S0",
                  "0501090Q0","0501090N0","0501090H0","0501090E0","0501090C0","0501090A0")
Tetracyclines <- c("0501030V0","0501030T0","0501030P0","0501030L0","0501030Z0",
                   "0501030I0","0501030F0","0501030X0","0501030Y0")
UTIs         <- c("0501130R0","0501130H0","0501130S0")
Aminog       <- c("0501040C0","0501040H0","0501040N0","0501040U0")
Other        <- c("0501070X0","0501070AE","0501070I0","0501070F0","0501070H0",
                  "0501070Y0","0501070AC","0501070M0","0501070W0","0501070AB",
                  "0501070Z0","0501070N0","0501070AA","0501070T0","0501070U0")
}

antibiotic_lookup <- read_excel(
  "data/foi02243_reference_tables.xlsx",
  sheet = 2
) |>
  select(bnf_chemical_substance_code = BNF_CHEMICAL_SUBSTANCE) |>
  mutate(
    antibiotic_group = case_when(
      bnf_chemical_substance_code %in% CnL ~ "C&L",
      bnf_chemical_substance_code %in% Cephs ~ "Ceph's",
      bnf_chemical_substance_code %in% Lep ~ "Lep",
      bnf_chemical_substance_code %in% Macrolides ~ "Macrolides",
      bnf_chemical_substance_code %in% MTO ~ "MTO",
      bnf_chemical_substance_code %in% Penicillins ~ "Penicillins",
      bnf_chemical_substance_code %in% Quinolones ~ "Quinolones",
      bnf_chemical_substance_code %in% SnT ~ "S&T",
      bnf_chemical_substance_code %in% TB ~ "TB",
      bnf_chemical_substance_code %in% Tetracyclines ~ "Tetracyclines",
      bnf_chemical_substance_code %in% UTIs ~ "UTIs",
      bnf_chemical_substance_code %in% Aminog ~ "Aminog.",
      bnf_chemical_substance_code %in% Other ~ "Other",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(antibiotic_group))

# Add antibiotic groups to both datasets
combined_data <- combined_data |>
  left_join(antibiotic_lookup, by = "bnf_chemical_substance_code")
combined_data_lsoa <- combined_data_lsoa |>
  left_join(antibiotic_lookup, by = "bnf_chemical_substance_code")

# Overwrite with antibiotic groups included
write_csv(combined_data, "data/combined_data.csv")
write_csv(combined_data_lsoa, "data/combined_data_lsoa.csv")

rm(list = ls())
