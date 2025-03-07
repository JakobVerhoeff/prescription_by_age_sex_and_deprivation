### Code for Figure 1b and c

library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)

setwd(here())
foidata_orig<-read.csv("data/foi02243_practice_2022_2023.csv") %>% 
  mutate(AGE_BAND = recode(
    AGE_BAND,
    "Age 0-5" = "0-5",
    "Age 6-10" = "6-10",
    "Age 11-20" = "11-20",
    "Age 21-30" = "21-30",
    "Age 31-40" = "31-40",
    "Age 41-50" = "41-50",
    "Age 51-60" = "51-60",
    "Age 61-70" = "61-70",
    "Age 71-80" = "71-80",
    "Age 81-90" = "81-90",
    "Age 91-100" = "91-100",
    "Age over 100" = "over 100", 
    "Unknown" = "NA"
  )
  ) %>%
  filter(!is.na(AGE_BAND))  # Remove rows with excluded ages

##### filter antibiotics that have a bad model fit
### 
codes <- c("0501100H0",
           "0501060D0",
           "0501070X0",
           "0501070AE",
           "0501070I0",
           "0501090R0",
           "0501090K0"
)
foidata<- foidata_orig %>%
  filter(!BNF_CHEMICAL_SUBSTANCE_CODE %in% codes)

#####
foidata$ITEMS<-as.numeric(foidata$ITEMS)
foidata$ITEMS[is.na(foidata$ITEMS)] <- 1 # I convert the NA's into 1's.
foidata <- foidata %>% filter(GENDER %in% c("Male", "Female"))


# # Add Gender column and combine both datasets
# foidata_male <- foidata %>%
#   filter(GENDER == "Male") %>%
#   mutate(GENDER = "Male")
# 
# foidata_female <- foidata %>%
#   filter(GENDER == "Female") %>%
#   mutate(GENDER = "Female")
# 
# foidata_combined <- bind_rows(foidata_male, foidata_female)

# Edit and group antibiotics
antibiotics<-read_excel("data/foi02243_reference_tables.xlsx",sheet=2)
antibiotics<-antibiotics%>%
  select(BNF_CHEMICAL_SUBSTANCE,CHEMICAL_SUBSTANCE_BNF_DESCR)
antibiotics <- antibiotics %>%
  mutate(ANTIBIOTIC_GROUP = case_when(
    BNF_CHEMICAL_SUBSTANCE %in% c("0501060D0") ~ "C&L",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501021M0", "0501021L0") ~ "Ceph's",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501100H0") ~ "Lep",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501050N0", "0501050H0", "0501050B0", "0501050A0") ~ "Macrolides",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501110C0") ~ "MTO",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501070X0", "0501070AE", "0501070I0") ~ "Other",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501015P0", "0501011P0", "0501012G0", "0501013K0", "0501013B0") ~ "Penicillins",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501120P0", "0501120X0", "0501120L0") ~ "Quinolones",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501080W0", "0501080D0") ~ "S&T",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501090R0", "0501090K0") ~ "TB",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501030V0", "0501030T0", "0501030P0", "0501030L0", "0501030Z0", "0501030I0") ~ "Tetracyclines",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501130R0", "0501130H0") ~ "UTIs",
    TRUE ~ NA_character_  # Assign NA if no match
  ))%>%
  select(-CHEMICAL_SUBSTANCE_BNF_DESCR)%>%
  rename(BNF_CHEMICAL_SUBSTANCE_CODE = BNF_CHEMICAL_SUBSTANCE) %>%
  filter(!is.na(ANTIBIOTIC_GROUP))  # Remove antibiotics not in these defined groups


foidata_combined<-foidata%>%
  left_join(antibiotics, by = "BNF_CHEMICAL_SUBSTANCE_CODE") %>%
  filter(!is.na(ANTIBIOTIC_GROUP))  # Remove antibiotics not in these defined groups
# Summarize data
###############################################
foidata_summary <- foidata_combined %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband = ave(total_items, AGE_BAND, FUN = sum),
         percentage = (total_items / total_items_ageband) * 100)
#############################################
foidata_summary <- foidata_combined %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband_gender = ave(total_items, AGE_BAND, GENDER, FUN = sum),
         percentage = (total_items / total_items_ageband_gender) * 100)

#############################################
# Ensure AGE_BAND is ordered correctly
foidata_summary <- foidata_summary %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = c("0-5", "6-10", "11-20", 
                                                "21-30", "31-40", "41-50", 
                                                "51-60", "61-70", "71-80", "81-90", 
                                                "91-100", "over 100")))

# Create plot with different linetypes for gender
foidata_summary<-foidata_summary%>%
  filter(!is.na(AGE_BAND))
# Create last_points to extract the last data point for each group
last_points <- foidata_summary %>%
  group_by(ANTIBIOTIC_GROUP, GENDER) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Now plot the graph with geom_text() for labeling
g1c <- 
  ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                            color = ANTIBIOTIC_GROUP, 
                            group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                            linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #           show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female",
       x = "Age Band",
       y = "Percentage (%)",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  ylim(0, 100)  # Set y-axis limits to match percentage range

ggsave("plots/figure1c.pdf")

g1d <- ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                            color = ANTIBIOTIC_GROUP, 
                            group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                            linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
 # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
#            size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
#            show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female zoomed",
       x = "Age Band",
       y = "Percentage (%)",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  ylim(0, 25)  # Set y-axis limits to match percentage range
 
ggsave("plots/figure1d.pdf")
