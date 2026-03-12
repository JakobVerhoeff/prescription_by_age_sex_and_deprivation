# Prescription by Age, Sex and Deprivation

Authors: Jakob Verhoeff, Naomi Waterlow, Gwen Knight
## Overview

This repository contains the analysis and figure-generation code for: "title" (link).
* 01_data_cleaning.R: prepares the data for analysis
* 1_figure1.R: generates ...
* 2_binomial.R: etc ...

## Data sources

All datasets used in this analysis are publicly available and can be accessed via the following links:

- FOI prescription data (NHSBSA):
	https://opendata.nhsbsa.net/dataset/foi-02243/resource/e45ebfda-a8af-43b2-a74e-f4436eadbc6f
- IMD data (NHS Digital CSV downloads):
    https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
- Geography lookup table (GP practice code to postcode lookup):
	https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data
- Geography lookup table (Postcode to LAD lookup):
	https://geoportal.statistics.gov.uk/datasets/f9c8996d451f44b79ab97ddd369ad5db/about

Please note the specific filenames and formats expected by the cleaning script (see 01_data_cleaning.R). Please note the expected `data/` folder structure for storing raw and intermediate files:

```
data/
├── foi/
│   └── (FOI raw data files) (e.g., foi02243_practice_2022_2023.csv)
├── (IMD data) (e.g., File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv)
├── (Postcode-LAD lookup table)  (e.g., PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv)
└── (GP-postcode lookup table) (e.g., epraccur-2.csv)
```

## Reproducing the analysis

R version and other requirements.
Analysis execution instructions.

## Notes

Notes here.

## License & Contact

Licence and contact statement.
