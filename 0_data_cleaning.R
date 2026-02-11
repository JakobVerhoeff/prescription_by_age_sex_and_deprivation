##### Clean the data into one dataset with
# no NAs
# age / gender for all 

# Libraries 
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
  mutate(AGE_BAND = dplyr::recode(
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
    "Unknown" = "Unknown"
  )
  ) 

# New dataset 
foidata_cleanish <- foidata_orig

##########################################
##### FILTER REDACTED ####################
##########################################
foidata_cleanish$ITEMS<-as.numeric(foidata_cleanish$ITEMS) # Use items not patient number 
sum(foidata_cleanish$ITEMS)
length(which(is.na(foidata_cleanish$ITEMS))) # 1362827
100 * length(which(is.na(foidata_cleanish$ITEMS)))/dim(foidata_orig)[1]  # 52.4%

foi1 <- foidata_cleanish
foi1$ITEMS[is.na(foi1$ITEMS)] <- 1 # I convert the NA's into 1's. ##### ASSUMPTION! ####

100 * length(which(is.na(foidata_cleanish$ITEMS))) / sum(foi1$ITEMS) # 4.5% 

foi4 <- foidata_cleanish
foi4$ITEMS[is.na(foi4$ITEMS)] <- 4 # convert the NA's into 4's. ##### ASSUMPTION! ####
100 * (4*length(which(is.na(foidata_cleanish$ITEMS)))) / sum(foi1$ITEMS) # 17.8% 

##### Set NAs to 1 for ITEMS for now 
foidata_cleanish <- foi1 %>%
  mutate(ITEMS4 = ifelse(ITEMS == 1, 4,ITEMS))

##########################################
##### MULTIPLE LINES #####################
##########################################

# Are there any multiple lines of data? e.g.
foidata_cleanish %>% filter(PRACTICE_CODE == "A81005", GENDER == "Female", BNF_CHEMICAL_SUBSTANCE_CODE == "0501012G0", AGE_BAND == "31-40")
# how many? 
multiple_lines <- foidata_cleanish %>%
  group_by(FINANCIAL_YEAR, PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND) %>%
  #filter(UNIQUE_PATIENT_COUNT != '*') %>% 
  filter(n_distinct(ITEMS) > 1) %>%
  arrange(FINANCIAL_YEAR, PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND)

unique(multiple_lines$BNF_CHEMICAL_SUBSTANCE_CODE) # about half of abx types 

foidata_clean_nomultiples <- foidata_cleanish %>% 
  group_by(FINANCIAL_YEAR, PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND) %>%
  summarise(TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE), TOTAL_ITEMS4 = sum(ITEMS4, na.rm = TRUE),
            .groups = "drop") 

multiple_lines2 <- foidata_clean_nomultiples %>%
  group_by(FINANCIAL_YEAR, PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND) %>%
  #filter(UNIQUE_PATIENT_COUNT != '*') %>% 
  filter(n_distinct(TOTAL_ITEMS) > 1) %>%
  arrange(FINANCIAL_YEAR, PRACTICE_CODE, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND)
multiple_lines2 # should be empty i.e. there aren't any
# issue now gone
foidata_clean_nomultiples %>% filter(PRACTICE_CODE == "A81005", GENDER == "Female", BNF_CHEMICAL_SUBSTANCE_CODE == "0501012G0", AGE_BAND == "31-40")

### 
foidata_clean <- foidata_clean_nomultiples %>%
  rename(ITEMS = TOTAL_ITEMS, ITEMS4 = TOTAL_ITEMS4)

# Check no ITEMS lost: both have 30465527 - yes. 
sum(foidata_clean$ITEMS)
f <- foidata_cleanish %>% filter(ITEMS !='*') 
f$ITEMS <- as.numeric(f$ITEMS)
sum(f$ITEMS)



##########################################
##### FILTER AGE/SEX ####################
##########################################
### Clean ages ########################
foidata_clean_age <- foidata_clean %>% filter(!AGE_BAND=="Unknown")  # Remove rows with excluded ages
dim(foidata_clean)[1] - dim(foidata_clean_age)[1] # 60451
100 * (dim(foidata_clean)[1] - dim(foidata_clean_age)[1])/dim(foidata_clean)[1] # 2% of rows 
sum(foidata_clean$ITEMS) - sum(foidata_clean_age$ITEMS)
100 * (sum(foidata_clean$ITEMS) - sum(foidata_clean_age$ITEMS)) / sum(foidata_clean$ITEMS) # < 1% of items

foidata_clean_age <- foidata_clean %>% filter(!AGE_BAND == "over 100")  # Remove rows with excluded ages
dim(foidata_clean)[1] - dim(foidata_clean_age)[1] # 10600
100 * (dim(foidata_clean)[1] - dim(foidata_clean_age)[1])/dim(foidata_clean)[1] # 0.4% of rows
sum(foidata_clean$ITEMS) - sum(foidata_clean_age$ITEMS)
100 * (sum(foidata_clean$ITEMS) - sum(foidata_clean_age$ITEMS)) / sum(foidata_clean$ITEMS) # < 0.05% of items

### Clean sex ########################
foidata_clean_sex <- foidata_clean[foidata_clean$GENDER %in% c("Male", "Female"), ] # remove those with no gender
dim(foidata_clean)[1] - dim(foidata_clean_sex)[1] # 119028
100 * (dim(foidata_clean)[1] - dim(foidata_clean_sex)[1])/dim(foidata_clean)[1] # 5% of rows 
sum(foidata_clean$ITEMS) - sum(foidata_clean_sex$ITEMS)
100 * (sum(foidata_clean$ITEMS) - sum(foidata_clean_sex$ITEMS)) / sum(foidata_clean$ITEMS) # < 1% of items


#### Include all of these cleaning steps ########
foi_as <- foidata_clean %>% 
  filter(!AGE_BAND=="Unknown") %>%  # Remove rows with unknown ages 
  filter(!AGE_BAND == "over 100") %>% # Remove those over 100 
  filter(GENDER %in% c("Male", "Female")) # Remove those with no sex entry 

dim(foi_as)[1]
100 * (dim(foidata_clean)[1] - dim(foi_as)[1]) / dim(foidata_clean)[1] # 5% smaller 
100 * (sum(foidata_clean$ITEMS) - sum(foi_as$ITEMS)) / sum(foidata_clean$ITEMS) # 1.13% of items

###########################################################
### EDIT DEPRIVATION DATA #################################
###########################################################
## Only have IMD for 2019 - may lose some GP practices thru this?
deprivationdata<-read.csv("data/deprivation_data.csv")%>%
  dplyr::select(-Sex,-Age,-Category,-Category.Type,-Lower.CI.95.0.limit,-Time.period.range,-Compared.to.goal,-New.data,-Time.period.Sortable,-Compared.to.ICB.sub.locations.value.or.percentiles,-Compared.to.England.value.or.percentiles,-Recent.Trend,-Value.note,-Denominator,-Count,-Upper.CI.99.8.limit,-Lower.CI.99.8.limit,-Upper.CI.95.0.limit,-Lower.CI.95.0.limit,-Indicator.ID,-Indicator.Name)
deprivationdata<-subset(deprivationdata, Time.period == "2019") # Select IMD2019
deprivationdata<-subset(deprivationdata,Area.Type=="GPs") # only use GP's
deprivationdata <- deprivationdata %>% rename(PRACTICE_CODE = Area.Code)
#edit deprivationdata into quantiles
deprivationdata$quintile <- cut(
  deprivationdata$Value,
  breaks = quantile(deprivationdata$Value, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  include.lowest = TRUE
)

deprivationdata$split10 <- cut(
  deprivationdata$Value,
  breaks = quantile(deprivationdata$Value, probs = seq(0, 1, 0.1), na.rm = TRUE),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
  include.lowest = TRUE
)

# Number of GP populations
#6315 in all years deprivation data 
length(unique(deprivationdata$PRACTICE_CODE)) # 6239 
range(deprivationdata$Value)

##########################################
##### GP POPULATIONS #################
##########################################
gpdatamale<-read.csv("data/gp-reg-pat-prac-sing-age-male.csv") #As of april 2022.
gpdatafemale<-read.csv("data/gp-reg-pat-prac-sing-age-female (1).csv") #As of april 2022
# chose this date because foidata also starts from april 2022
length(unique(gpdatafemale$ORG_CODE)) # 6518 
# Should be 7,079 in 2022
# https://www.statista.com/statistics/996600/gp-practices-in-england/



###########################################################
# EDIT GPDATA/MALE-FEMALE #################################
###########################################################
gpdatamale <- gpdatamale %>% rename(PRACTICE_CODE = ORG_CODE)
gpdatafemale<-gpdatafemale %>% rename(PRACTICE_CODE = ORG_CODE)
# Define age groups
gpdatamale$AGE<-as.numeric(gpdatamale$AGE)
gpdatamale <- gpdatamale %>% # Divide into the same 5-year agebands as FOI data
  mutate(
    age_group = case_when(
      AGE >= 0 & AGE <= 5  ~ "0-5",
      AGE >= 6 & AGE <= 10 ~ "6-10",
      AGE >= 11 & AGE <= 20 ~ "11-20",
      AGE >= 21 & AGE <= 30 ~ "21-30",
      AGE >= 31 & AGE <= 40 ~ "31-40",
      AGE >= 41 & AGE <= 50 ~ "41-50",
      AGE >= 51 & AGE <= 60 ~ "51-60",
      AGE >= 61 & AGE <= 70 ~ "61-70",
      AGE >= 71 & AGE <= 80 ~ "71-80",
      AGE >= 81 & AGE <= 90 ~ "81-90",
      AGE >= 91 & AGE <= 100 ~ "91-100",
      AGE > 100 ~"over 100",
      TRUE ~ NA_character_  # Exclude other ages
    )
  ) %>%
  filter(!is.na(age_group))  # Remove rows with excluded ages

# Aggregate the number of patients by PRACTICE_CODE and age group
gpdatamale <- gpdatamale %>%
  group_by(PRACTICE_CODE, age_group) %>%
  summarise(
    total_patients = sum(NUMBER_OF_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  )


####### Same for female
gpdatafemale$AGE<-as.numeric(gpdatafemale$AGE)
gpdatafemale <- gpdatafemale %>%
  mutate(
    age_group = case_when(
      AGE >= 0 & AGE <= 5  ~ "0-5",
      AGE >= 6 & AGE <= 10 ~ "6-10",
      AGE >= 11 & AGE <= 20 ~ "11-20",
      AGE >= 21 & AGE <= 30 ~ "21-30",
      AGE >= 31 & AGE <= 40 ~ "31-40",
      AGE >= 41 & AGE <= 50 ~ "41-50",
      AGE >= 51 & AGE <= 60 ~ "51-60",
      AGE >= 61 & AGE <= 70 ~ "61-70",
      AGE >= 71 & AGE <= 80 ~ "71-80",
      AGE >= 81 & AGE <= 90 ~ "81-90",
      AGE >= 91 & AGE <= 100 ~ "91-100",
      AGE > 100 ~"over 100",
      TRUE ~ NA_character_  # Exclude other ages
    )
  ) %>%
  filter(!is.na(age_group))  # Remove rows with excluded ages

# Aggregate the number of patients by PRACTICE_CODE and age group
gpdatafemale <- gpdatafemale %>%
  group_by(PRACTICE_CODE, age_group) %>%
  summarise(
    total_patients = sum(NUMBER_OF_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  )

# Add GENDER column to each dataset
gpdatamale <- gpdatamale %>%  mutate(GENDER = "Male")
gpdatafemale <- gpdatafemale %>% mutate(GENDER = "Female")

# Combine both datasets
gpdata_combined <- bind_rows(gpdatamale, gpdatafemale)
gpdata_combined <- gpdata_combined %>%
  rename(AGE_BAND = age_group)
# 2019 England population = 56,230,100
56230100 - sum(gpdata_combined$total_patients)
100 * sum(gpdata_combined$total_patients)/56230100
100*(56230100 - sum(gpdata_combined$total_patients))/56230100
# 10% more in the data than in the England population... some registered twice? 



###########################################################
### COMBINE ##########################################
###########################################################
foidata <- merge(foi_as, deprivationdata[, c("PRACTICE_CODE", "Value","quintile","split10")], 
                 by = "PRACTICE_CODE", all.x = TRUE) %>%
  rename(IMD = Value)
# doesn't remove those with no deprivation data 

foidata<- foidata %>% 
  dplyr::select(#-UNIQUE_PATIENT_COUNT,
                -FINANCIAL_YEAR) # remove those things we won't use

# combine with population data 
foidata <-  left_join(foidata, gpdata_combined, 
                      by = c("PRACTICE_CODE", "GENDER", "AGE_BAND"))

# No rows have been removed by the merging:
dim(foidata)[1] - dim(foi_as)[1]
# different gp practices in each though
length(unique(foidata_orig$PRACTICE_CODE))
length(unique(foi_as$PRACTICE_CODE))
length(unique(deprivationdata$PRACTICE_CODE))
length(unique(gpdata_combined$PRACTICE_CODE))

# What % of patients removed if no IMD? 1.6% 
100 * foidata %>% filter(is.na(IMD)) %>% summarise(sum(total_patients,na.rm = TRUE)) / sum(foidata$total_patients, na.rm = TRUE)
# What % of gp practices had no data on patient numbers but HAD IMD data: NONE
unique(foidata %>% filter(!is.na(IMD)) %>% filter(is.na(total_patients)) %>% select(PRACTICE_CODE))

# Range of GP practice data
foidata %>% filter(!is.na(IMD), !is.na(total_patients)) %>% 
  group_by(PRACTICE_CODE) %>%
  dplyr::summarise(total = sum(total_patients)) %>%
  dplyr::reframe(range(total))

# GP practice data population in each IMD
foidata %>% filter(!is.na(IMD), !is.na(total_patients)) %>% 
  group_by(quintile) %>%
  dplyr::summarise(total = sum(total_patients)) 



# Percentage women of GP practice data
foidata %>% filter(!is.na(IMD), !is.na(total_patients)) %>% 
  group_by(PRACTICE_CODE, GENDER) %>%
  summarise(total = sum(total_patients)) %>%
  group_by(PRACTICE_CODE) %>%
  pivot_wider(names_from = GENDER, values_from = total) %>%
  mutate(ratio = Female / (Female + Male)) %>%
  ungroup() %>%
  summarise(mean(ratio))

##### make items per patient column
foidata <- foidata %>%
  mutate(items_per_patient = ITEMS / total_patients,
         items_per_patient_na4 = ITEMS4 / total_patients)


###########################################################
### ANTIBIOTICS ###########################################
###########################################################
# Edit and group antibiotics
antibiotics<-read_excel("data/foi02243_reference_tables.xlsx",sheet=2)
antibiotics<-antibiotics%>%
  dplyr::select(BNF_CHEMICAL_SUBSTANCE,CHEMICAL_SUBSTANCE_BNF_DESCR)
antibiotics <- antibiotics %>%
  mutate(ANTIBIOTIC_GROUP = case_when(
    BNF_CHEMICAL_SUBSTANCE %in% c("0501060D0","0501060E0") ~ "C&L",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501023A0","0501021A0",
                                  "0501021B0","0501021L0","0501024A0","0501021C0","0501021D0","0501021E0",
                                  "050102020","0501021F0","0501021M0","0501021H0","0501021G0","0501021K0",
                                  "0501021J0","0501022C0","0501022B0","0501022D0","0501022A0") ~ "Ceph's",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501100H0","0501100J0","0501100C0") ~ "Lep",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501050N0", "0501050H0", "0501050B0", "0501050A0","0501050C0","0501050K0") ~ "Macrolides",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501110C0","0501110G0") ~ "MTO",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501015P0", "0501011P0", "0501012G0", "0501013K0", "0501013B0","0501011J0","0501012H0","0501012U0","0501013C0",
                                  "0501013E0","0501013L0","0501014N0","0501014S0") ~ "Penicillins",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501120P0", "0501120X0", "0501120L0","0501120Y0","0501120Q0","0501120N0") ~ "Quinolones",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501080W0", "0501080D0","0501080V0","0501080T0","0501080J0") ~ "S&T",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501090R0", "0501090K0","0501090V0","0501090U0","0501090S0",
                                  "0501090Q0","0501090N0","0501090H0","0501090E0","0501090C0","0501090A0") ~ "TB",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501030V0", "0501030T0", "0501030P0", "0501030L0", "0501030Z0", "0501030I0","0501030F0","0501030X0","0501030Y0") ~ "Tetracyclines",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501130R0", "0501130H0","0501130S0") ~ "UTIs",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501040C0","0501040H0","0501040N0","0501040U0") ~ "Aminog.",
    BNF_CHEMICAL_SUBSTANCE %in% c("0501070X0", "0501070AE", "0501070I0","0501070F0",
                                  "0501070I0","0501070H0","0501070Y0","0501070AC","0501070AE",
                                  "0501070M0","0501070W0","0501070AB","0501070Z0","0501070X0",
                                  "0501070N0","0501070AA","0501070T0","0501070U0") ~ "Other",
    TRUE ~ NA_character_  # Assign NA if no match
  )) %>%
  dplyr::select(-CHEMICAL_SUBSTANCE_BNF_DESCR)%>%
  rename(BNF_CHEMICAL_SUBSTANCE_CODE = BNF_CHEMICAL_SUBSTANCE) %>%
  filter(!is.na(ANTIBIOTIC_GROUP))  # Remove antibiotics not in these defined groups (only 0501070AD?)


foidata_combined<-foidata%>%
  left_join(antibiotics, by = "BNF_CHEMICAL_SUBSTANCE_CODE") %>%
  filter(!is.na(ANTIBIOTIC_GROUP))  # Remove antibiotics not in these defined groups

# Ensure AGE_BAND is ordered correctly
foidata_combined$AGE_BAND <- factor(foidata_combined$AGE_BAND, 
                           levels = c("0-5", "6-10", "11-20", 
                                      "21-30", "31-40", "41-50", 
                                      "51-60", "61-70", "71-80", 
                                      "81-90", "91-100"))


#### Filter out missing data? 
# If no patients registered in that age group must be registered error? 
f <- foidata_combined %>% filter(total_patients == 0) %>% summarise(IT = unique(ITEMS))
unique(f$IT)
sum(f$IT) # only 312 items

# Filter out missing values
foidata_filtered <- foidata_combined %>%
  filter(!is.na(IMD), !is.na(total_patients), 
         total_patients > 0) # remove those with no patients registered at this age group

dim(foidata_filtered)[1] - dim(foidata_combined)[1]

# How many removed? 
100 * (dim(foidata_orig)[1] - dim(foidata_filtered)[1]) / dim(foidata_orig)[1]
sum(foidata_filtered$ITEMS)[1] # how many items?
100 - 100 * (sum(foidata_clean$ITEMS)[1] - sum(foidata_filtered$ITEMS)[1]) / sum(foidata_clean$ITEMS)[1]

# Average per person? 
summary(foidata_filtered$items_per_patient)
100 * sum(foidata_filtered$ITEMS)/sum(foidata_filtered$total_patients)

# Most commonly prescribed? 
foidata_filtered %>% group_by(ANTIBIOTIC_GROUP) %>%
  summarise(total = sum(ITEMS),
            perc = 100 * total/sum(foidata_filtered$ITEMS)) %>%
  arrange(perc)

# Age group most prescribed to? 
fa <- foidata_filtered %>% group_by(AGE_BAND) %>%
  summarise(total = sum(ITEMS),
            perc = 100 * total/sum(foidata_filtered$ITEMS)) %>%
  arrange(perc)
fa
# Under 10
fa %>% filter(AGE_BAND %in% c("0-5","6-10")) %>% summarise(sum(perc))

# Over 70
fa %>% filter(AGE_BAND %in% c("71-80","81-90","91-100")) %>% summarise(sum(perc))

# Gender
foidata_filtered %>% group_by(GENDER) %>%
  summarise(total = sum(ITEMS),
            perc = 100 * total/sum(foidata_filtered$ITEMS)) %>%
  arrange(perc)

## check population sizes 
foidata_filtered %>% filter(PRACTICE_CODE == "A81001", 
                            GENDER == "Female", 
                            AGE_BAND == "6-10") # should all have same population size

####### Save some population totals for the GPs in final data
totals_gp_bysplit10 <- left_join(gpdata_combined, deprivationdata) %>%
  filter(PRACTICE_CODE %in% foidata_filtered$PRACTICE_CODE)
sum(gpdata_combined$total_patients) # All 
sum(totals_gp_bysplit10$total_patients) # Only those in foidata_filtered
sum(foidata_filtered$total_patients) # Way more as have each antibiotic group here 

totals_gp_bysplit10_gender <- totals_gp_bysplit10 %>% 
  group_by(GENDER, split10) %>%
  summarise(total_population = sum(total_patients))
write_csv(totals_gp_bysplit10_gender, "data/cleaned_totals_gp_bysplit10_gender.csv")
sum(totals_gp_bysplit10_gender$total_population) # Only those in foidata_filtered

totals_gp_bysplit10_agesex <- totals_gp_bysplit10 %>% 
  group_by(GENDER, AGE_BAND, split10) %>%
  summarise(total_population = sum(total_patients))
write_csv(totals_gp_bysplit10_agesex, "data/cleaned_totals_gp_bysplit10_agesex.csv")
sum(totals_gp_bysplit10_gender$total_population) 
sum(totals_gp_bysplit10_agesex$total_population) 

totals_gp_bysplit10_sexgp <- totals_gp_bysplit10 %>% 
  group_by(GENDER, PRACTICE_CODE, split10) %>%
  summarise(total_population = sum(total_patients))
write_csv(totals_gp_bysplit10_sexgp, "data/cleaned_totals_gp_bysplit10_sexgp.csv")
sum(totals_gp_bysplit10_sexgp$total_population) 
sum(totals_gp_bysplit10_agesex$total_population) 


### Need to include those ages with no prescribing: 0s! 
full_grid <- foidata_filtered %>% 
  dplyr::select(PRACTICE_CODE, AGE_BAND, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE) %>%
  complete(PRACTICE_CODE, AGE_BAND, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE) %>% # need to add in all the age and genders possible
  left_join(gpdata_combined,by = c("PRACTICE_CODE", "AGE_BAND", "GENDER")) %>%
  left_join(deprivationdata) %>%
  left_join(foidata_filtered[,c("PRACTICE_CODE", "BNF_CHEMICAL_SUBSTANCE_CODE", "GENDER", "AGE_BAND", 
                                "ITEMS", "ITEMS4",  
                                "items_per_patient", "items_per_patient_na4", "ANTIBIOTIC_GROUP"
  )]) %>%
  rename(IMD = Value)

# If no patients then can remove 
w <- which(full_grid$total_patients ==0)

full_grid <- full_grid %>% filter(total_patients > 0)

# Some still NA: those that have 0 use of this antibiotic 
full_grid %>% dplyr::select(PRACTICE_CODE, AGE_BAND, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE, ITEMS, total_patients, items_per_patient)
w <- which(is.na(full_grid$ITEMS))
full_grid[w, c("ITEMS", "ITEMS4", "items_per_patient", "items_per_patient_na4")] <- 0

full_grid %>% filter(PRACTICE_CODE == "A81005", GENDER == "Female", 
                     BNF_CHEMICAL_SUBSTANCE_CODE == "0501012G0", AGE_BAND == "31-40") %>%
   dplyr::select(PRACTICE_CODE, AGE_BAND, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE, ITEMS, total_patients, items_per_patient)

foidata_filtered %>% filter(PRACTICE_CODE == "A81005", GENDER == "Female", 
                                      BNF_CHEMICAL_SUBSTANCE_CODE == "0501012G0", AGE_BAND == "31-40") 
#bigger as 0s in rows now 
dim(foidata_filtered)
dim(full_grid) 
# but same number of items prescribed
sum(foidata_filtered$ITEMS)
sum(full_grid$ITEMS)



###### Save cleaned data with all information 
## 
write_csv(full_grid, "data/cleaned_combined_data.csv")

