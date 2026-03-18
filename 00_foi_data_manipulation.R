### foi data manipulation
### 
### 
library(data.table)  # Much faster than base R or readr for big files

# Set path to your folder
foi_folder <- "data/practice_data_yearly"

# Get all matching CSV files
foi_files <- list.files(
  path = foi_folder,
  full.names = TRUE
)

cat("Found", length(foi_files), "files\n")

# Read and bind all files in one efficient step
# fill = TRUE handles any mismatched columns across files
foi_data <- rbindlist(
  lapply(foi_files, fread),
  use.names = TRUE,
  fill = TRUE
)

cat("Done! Final dimensions:", nrow(foi_data), "rows x", ncol(foi_data), "cols\n")

## check with existing data
foi_2022 <- read_csv("data/foi/foi02243_practice_2022_2023.csv")
colnames(foi_2022)
colnames(foi_data)

write.csv(foi_data, "data/foi/foi_all_years.csv", row.names = FALSE)

### IF USE MONTHLY DATA 
### Extract year and month from YEAR_MONTH column
# foi_data[, YEAR_MONTH := as.character(YEAR_MONTH)]  # in case it's numeric
# 
# foi_data[, `:=`(
#   YEAR  = as.integer(substr(YEAR_MONTH, 1, 4)),
#   MONTH = as.integer(substr(YEAR_MONTH, 5, 6))
# )]
# 
# # But only want annual numbers: 
# # If ITEMS == "*" set to 1 to indicate at least 1 item
# foi_data_all_annual <- foi_data[, .(
#   total_items = sum(ifelse(ITEMS == "*", 1, as.integer(ITEMS)), na.rm = TRUE)
# ), by = .(PRACTICE_CODE, GENDER, AGE_BAND, YEAR)]
