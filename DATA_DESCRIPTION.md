# Data Description

## Overview

This analysis examines antibiotic prescribing patterns in England by age, gender, and deprivation (IMD) using Freedom of Information (FOI) data from NHS Business Services Authority (NHS BSA) for financial years 2015/16 to 2022/23, linked to area-level deprivation indices and population denominators.

## Input data

All input data files are in `data/`. Unused legacy files have been moved to `data/archive/`.

### 1. FOI prescribing data (NHS BSA)

**Files:** `data/foi/foi02243_practice_YYYY_YYYY.csv` (8 files, one per financial year)

**Source:** NHS BSA, obtained via FOI request (reference: FOI-02243)

**Coverage:** Financial years 2015/16 through 2022/23 (~23.5 million rows total across all years)

**Granularity:** Practice-level, by BNF chemical substance code, gender, and age band

**Key fields:**
- `FINANCIAL_YEAR`: e.g. "2022/2023" (converted to calendar year of start, e.g. 2022)
- `PRACTICE_CODE`: NHS GP practice code
- `BNF_CHEMICAL_SUBSTANCE_CODE`: BNF code identifying the antibiotic
- `GENDER`: Male, Female, Unknown, Indeterminate (recoded to man, woman, unknown, indeterminate)
- `AGE_BAND`: 10-year bands from 0-5 through 91-100, plus Unknown (81-90, 91-100, and over 100 are merged into 81+)
- `UNIQUE_PATIENT_COUNT`: Number of patients receiving prescriptions (suppressed values marked `*`)
- `ITEMS`: Number of prescription items (suppressed values marked `*`)

**Data suppression:** Values below a threshold are replaced with `*` in the source data. These are imputed as 1 in the cleaning pipeline. This is a conservative approach that systematically undercounts rare prescriptions in small practices.


### 2. Index of Multiple Deprivation 2019

**File:** `data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv`

**Source:** Ministry of Housing, Communities and Local Government (MHCLG), English Indices of Deprivation 2019. https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

**Coverage:** All 32,844 Lower Super Output Areas (LSOAs) in England (2011 boundaries)

**Key fields used:**
- `LSOA code (2011)`: LSOA identifier
- `Local Authority District code (2019)`: LAD code for aggregation
- `Index of Multiple Deprivation (IMD) Score`: Continuous deprivation score
- `Total population: mid 2015 (excluding prisoners)`: Population denominator (mid-2015 estimate)

**Processing:** IMD scores are aggregated from LSOA to LAD level using population-weighted means. LADs are then assigned to quintiles (Q1 = most deprived, Q5 = least deprived) based on this weighted score.

**Limitation:** The population field is from mid-2015 and is only used in the LSOA-level linkage pathway (`04_linkage_comparison.R`). The LAD-level pathway uses year-specific ONS population estimates instead.

### 3. GP practice to postcode lookup

**File:** `data/epraccur-2.csv`

**Source:** NHS England e-Practitioner Register: https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data 

**Coverage:** 15,430 GP practices in England

**Key fields used:** `practice_code`, `postcode`

**Limitation:** This is a single snapshot (not versioned by year) last edited 30 October 2025. Practices that opened, closed, or moved during 2015-2022 may be incorrectly linked or unlinked. Approximately 0.3% of FOI data rows cannot be linked to a postcode.

### 4. Postcode to geographic area lookup

**File:** `data/PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv`

**Source:** ONS Postcode to Output Area/LSOA/MSOA/LAD Lookup (February 2024), https://geoportal.statistics.gov.uk/datasets/80592949bebd4390b2cbe29159a75ef4/about

**Coverage:** 2.7 million postcodes across the UK

**Key fields used:** `pcds` (postcode), `ladcd` (LAD code), `lsoa21cd` (LSOA code)

**Note:** Only English LAD codes (starting with "E") are retained in the pipeline. Welsh practices (22 LAD codes) are filtered out.

### 5. ONS population estimates by LAD, age, and sex

**File:** `data/ons_popn_lad_age_sex.csv`

**Source:** ONS mid-year population estimates from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales ("Mid-2011 to mid-2024 detailed time series edition of this dataset")

**Coverage:** 297 English LADs, single-year ages 0-90+, by sex, for years 2011-2024, updated 30th July 2025

**Key fields used:** `ladcode23` (LAD code), `sex` (f/m, recoded to woman/man), `age` (0-90), `population_YYYY` columns

**Processing:** Single-year ages are aggregated into age bands matching the FOI data (0-5, 6-10, 11-20, ..., 71-80, 81+). ONS age 90 (representing 90+) is included in the 81+ band. Year-specific populations are used as denominators.

### 6. ONS population by IMD decile, age, and sex

**File:** `data/popsbyimdengland20012019.csv`

**Source:** ONS Population by Index of Multiple Deprivation, England ([adhoc request 12386](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/12386populationbyindexofmultipledeprivationimdengland2001to2019))

**Coverage:** 2019 only (despite filename suggesting 2001-2019), 10 IMD deciles, by sex and 5-year age bands

**Used by:** `02_data_exploration.R` only, for demographic pyramids and age/sex distribution by IMD. Not used in modelling.

### 7. BNF antibiotic reference table

**File:** `data/foi02243_reference_tables.xlsx` (sheet 2)

**Source:** NHS BSA (provided with FOI response)

**Coverage:** 97 BNF chemical substance codes mapped to 13 antibiotic families

**Antibiotic families:** Aminog. (aminoglycosides), C&L (clindamycin & lincomycin), Ceph's (cephalosporins), Lep (leprosy drugs), Macrolides, MTO (metronidazole & tinidazole), Other, Penicillins, Quinolones, S&T (sulfonamides & trimethoprim), TB (anti-tuberculosis), Tetracyclines, UTIs (urinary tract infection-specific)

## Output data

### Combined dataset (LAD level)

**File:** `data/combined_data.csv` (generated by `01_data_cleaning.R`)

**Rows:** 1,665,372

**Columns:** `bnf_chemical_substance_code`, `gender`, `age_band`, `lad_code`, `year`, `total_patients`, `total_items`, `imd_score`, `population`, `imd_quintile`, `pop_a_s`, `antibiotic_group`

**Aggregation level:** BNF code x gender x age band x LAD x year

**Key statistics:**
- Years: 2015-2022
- Gender: man, woman, unknown, indeterminate
- Age bands: 0-5, 6-10, 11-20, 21-30, 31-40, 41-50, 51-60, 61-70, 71-80, 81+, Unknown
- LADs: 296 English local authority districts
- Total prescription items: 424,826,568
- Antibiotic groups: 13 families

**Missing data:** 25.3% of rows have no population denominator (`pop_a_s`). This is entirely due to:
- Unknown/indeterminate gender (22.4% of rows) -- no ONS denominator exists
- Unknown age band (7.4% of rows) -- cannot be matched to ONS population

After filtering to known gender (man/woman) and known age band with population denominator, **1,244,002 rows** are retained containing **407,683,949 items** (96.0% of all items).

### Combined dataset (LSOA level)

**File:** `data/combined_data_lsoa.csv` (generated by `01_data_cleaning.R`)

**Aggregation level:** BNF code x gender x age band x LSOA x year

**Used by:** `03_model_try.R` (alternative linkage), `04_linkage_comparison.R`

## Geographic linkage

Two geographic linkage methods are implemented:

1. **LAD linkage** (primary): GP practice postcode -> LAD code -> IMD quintile (population-weighted LAD mean). Population denominator: ONS mid-year estimates by LAD, age, sex, and year.

2. **LSOA linkage** (UKHSA method): GP practice postcode -> LSOA code -> LSOA-level IMD. Population denominator: mid-2015 from IMD file (fixed across years).

The LAD method is used as the primary analysis because it provides year-specific population denominators. The LSOA method provides more granular deprivation measurement but uses a fixed population denominator. Both are compared in `04_linkage_comparison.R`.

