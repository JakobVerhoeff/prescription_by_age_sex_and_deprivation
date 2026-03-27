##### How does population size vs registration vary by age / sex? 
##### Author: Gwen (12/3/26)
##### Code exploring population sizes 
##### 
##### 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet) 
library(stringr)
library(viridis)
library(here)
library(scales)

setwd(here())
theme_set(theme_bw(base_size = 12))

rm(list = ls())


##########################################
##### GP POPULATIONS #################
gpdatamale<-read.csv("data/gp-reg-pat-prac-sing-age-male_2019.csv") #As of Nov 2019
gpdatafemale<-read.csv("data/gp-reg-pat-prac-sing-age-female_2019.csv") #As of Nov 2019
# chose this date because foidata also starts from april 2022
length(unique(gpdatafemale$ORG_CODE)) # 6518 
# Should be 7,079 in 2022
# https://www.statista.com/statistics/996600/gp-practices-in-england/
write_csv(gpdatamale, "data/use_gp-reg-pat-prac-sing-age-male_2019.csv")
write_csv(gpdatafemale, "data/use_gp-reg-pat-prac-sing-age-female_2019.csv")

###########################################################
# EDIT GPDATA/MALE-FEMALE #################################
###########################################################
gpdatamale <- gpdatamale %>% rename(PRACTICE_CODE = ORG_CODE)
gpdatafemale<-gpdatafemale %>% rename(PRACTICE_CODE = ORG_CODE)
# Define age groups
w <- which(gpdatamale$AGE == "95+")
gpdatamale$AGE[w] <- 95
#gpdatamale$AGE<-as.numeric(gpdatamale$AGE)
gpdatamale <- gpdatamale %>% # Divide into the same 5-year agebands as FOI data
  filter(AGE != "ALL") %>% 
  mutate(
    age = as.numeric(AGE),
    age_group = case_when(
      age >= 0 & age <= 5  ~ "0-5",
      age >= 6 & age <= 10 ~ "6-10",
      age >= 11 & age <= 20 ~ "11-20",
      age >= 21 & age <= 30 ~ "21-30",
      age >= 31 & age <= 40 ~ "31-40",
      age >= 41 & age <= 50 ~ "41-50",
      age >= 51 & age <= 60 ~ "51-60",
      age >= 61 & age <= 70 ~ "61-70",
      age >= 71 & age <= 80 ~ "71-80",
      age >= 81 & age <= 90 ~ "81-90",
      age >= 91 & age <= 100 ~ "91-100",
      age == "95+" ~ "91-100",
      age > 100 ~"over 100",
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
w <- which(gpdatafemale$AGE == "95+")
gpdatafemale$AGE[w] <- 95

gpdatafemale <- gpdatafemale %>%
  filter(AGE != "ALL") %>% 
  mutate(
    age = as.numeric(AGE),
    age_group = case_when(
      age >= 0 & age <= 5  ~ "0-5",
      age >= 6 & age <= 10 ~ "6-10",
      age >= 11 & age <= 20 ~ "11-20",
      age >= 21 & age <= 30 ~ "21-30",
      age >= 31 & age <= 40 ~ "31-40",
      age >= 41 & age <= 50 ~ "41-50",
      age >= 51 & age <= 60 ~ "51-60",
      age >= 61 & age <= 70 ~ "61-70",
      age >= 71 & age <= 80 ~ "71-80",
      age >= 81 & age <= 90 ~ "81-90",
      age >= 91 & age <= 100 ~ "91-100",
      age == "95+" ~ "91-100",
      age > 100 ~"over 100",
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
# 7% more in the data than in the England population... some registered twice? 
gpdata_combined$AGE_BAND <- factor(gpdata_combined$AGE_BAND, 
                                levels = c("0-5", "6-10", "11-20", 
                                           "21-30", "31-40", "41-50", 
                                           "51-60", "61-70", "71-80", 
                                           "81-90", "91-100"))
ggplot(gpdata_combined %>% 
         group_by(AGE_BAND, GENDER) %>% 
         summarise(total = sum(total_patients)), 
       aes(x=AGE_BAND, y = total, group = GENDER)) + 
  geom_bar(position = "dodge", stat = "identity", aes(fill = GENDER)) + 
  scale_y_continuous("Number of patients registered")

###### combined FOI IMD and gp registration data
# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

data$split5 <- cut(
  data$IMD,
  breaks = quantile(data$IMD, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Q5", "Q4", "Q3", "Q2", "Q1"),
  include.lowest = TRUE
)

gp_registrations = data %>% 
  # Select first row for each practice code and age band to avoid double counting
  group_by(AGE_BAND, GENDER, PRACTICE_CODE) %>% 
  slice(1) %>% 
  ungroup() %>%
  select(AGE_BAND, GENDER, PRACTICE_CODE, total_patients, split5) %>%
  group_by(split5, AGE_BAND, GENDER) %>% 
  summarise(tot_reg = sum(total_patients)) 
colnames(gp_registrations) <- tolower(colnames(gp_registrations))

# Recode gender
gp_registrations$gender <- dplyr::recode(gp_registrations$gender, "Female" = "females", "Male" = "males")

## Compare
sum(gp_registrations$tot_reg) # 60175830
sum(gpdata_combined$total_patients) #61507475

# 100% in the data => some GPs missing
sum(gp_registrations$tot_reg) / sum(gpdata_combined$total_patients) 


### Population at IMD level 
# 2019 values for England by age and sex
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/12386populationbyindexofmultipledeprivationimdengland2001to2019
imd_pops <- read_csv("data/popsbyimdengland20012019.csv")[,1:23] %>% 
  select(-year, imd:gender) %>%
  pivot_longer(cols = `<1`:`90+`, names_to = "age", values_to = "popn") 
sum(imd_pops$popn) # check = 56mil
old_imd <- imd_pops %>% filter(age %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"))
sum(old_imd$popn)

imd_pops <- imd_pops %>%
  mutate(
    AGE_BAND = dplyr::recode(
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
    )
  )

#colnames(imd_pops) <- c("age","imd_dep","age","popn","AGE_BAND")
colnames(imd_pops) <- tolower(colnames(imd_pops))

deprivationpops = imd_pops %>% 
  group_by(gender, age_band, imd) %>%
  summarise(tot_pop = sum(popn))
deprivationpops$split5 = paste0("Q",deprivationpops$imd)

deprivationpops$age_band <- factor(deprivationpops$age_band,
                                   levels = c("0-5", "6-10", "11-20", 
                                              "21-30", "31-40", "41-50", 
                                              "51-60", "61-70", "71-80", 
                                              "81-90", "91-100"))

### JOIN
demoninators <- left_join(gp_registrations, deprivationpops, by = c("gender", "age_band", "split5")) %>%
  mutate(ratio = tot_reg / tot_pop)
demoninators$split5 <- factor(demoninators$split5, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
# Ensure AGE_BAND is ordered correctly
demoninators$age_band <- factor(demoninators$age_band, 
                                levels = c("0-5", "6-10", "11-20", 
                                           "21-30", "31-40", "41-50", 
                                           "51-60", "61-70", "71-80", 
                                           "81-90", "91-100"))

########## Ratio population to registrations
##### by age / gender
 

dd_age_g <- demoninators %>% group_by(age_band,gender) %>%
  summarise(total_popu = sum(tot_pop), total_reg = sum(tot_reg)) %>%
  mutate(ratio = total_reg / total_popu)

ggplot(dd_age_g, aes(x = age_band, y =ratio, group = gender)) + 
  geom_point(aes(col = gender), size = 3) + 
  geom_line(aes(col = gender)) + 
  scale_y_continuous("Ratio: higher = more patients registered than population", lim = c(0,1.25)) +
  geom_hline(yintercept = 1, linetype = "dashed")



########## By age / gender / IMD 

ggplot(demoninators, aes(x = split5, y = ratio, group = gender)) + 
  geom_point(aes(col = gender)) + 
  geom_line(aes(col = gender)) + 
  scale_y_continuous("Ratio: higher = more patients registered than population") +
  facet_wrap(~age_band) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggtitle("Q1 = most deprived, Q10 = least deprived")

ggplot(demoninators, aes(x = split5, y = ratio, group = age_band)) + 
  geom_point(aes(col = age_band)) + 
  geom_line(aes(col = age_band)) + 
  scale_y_continuous("Ratio: higher = more patients registered than population", lim = c(0,1.5)) +
  facet_wrap(~gender) + 
  scale_x_discrete("IMD quintile") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggtitle("Q1 = most deprived, Q5 = least deprived")

sum(demoninators$tot_reg) / sum(demoninators$tot_pop)



##### Population by IMD
##### 
## Plot distribution 
ggplot(deprivationpops, aes(x=age_band, y = tot_pop, group = gender)) + 
  geom_bar(position = "dodge", stat = "identity", aes(fill = gender)) + 
  scale_y_continuous("Population size") 

# Pyramid plot 
pyramid_data <- deprivationpops %>%
  mutate(pop_plot = ifelse(gender == "males", -tot_pop, tot_pop))

ggplot(pyramid_data, aes(x = age_band, y = pop_plot, fill = gender)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    x = "Age band",
    y = "Population",
    fill = "Gender",
    title = "Population Pyramid"
  ) +
  facet_wrap(~ split5) +
  theme_minimal()

### Or proportions ?
plot_df <- deprivationpops %>%
  group_by(split5, gender) %>%
  mutate(prop = tot_pop / sum(tot_pop)) %>%
  ungroup() %>%
  mutate(
    prop_plot = if_else(gender == "males", -prop, prop)
  )

ggplot(plot_df, aes(x = age_band, y = prop_plot, fill = gender)) +
  geom_col(width = 0.9) +
  coord_flip() +
  facet_wrap(~ split5, nrow = 1) +
  scale_y_continuous(labels = \(x) percent(abs(x))) +
  labs(
    x = "Age band",
    y = "Percent within IMD",
    title = "Age structure by IMD",
    fill = "Gender"
  ) +
  theme_minimal()

## Do it in smaller age bands
age_df <- deprivationpops %>%
  mutate(
    age_group = case_when(
      age_band %in% c("0-5", "6-10", "11-20") ~ "Kids (0-20)",
      age_band %in% c("21-30", "31-40", "41-50", "51-60") ~ "Adults (21-60)",
      age_band %in% c("61-70", "71-80", "81-90","91-100") ~ "Elderly (61+)"
    )
  )


plot_df <- age_df %>%
  group_by(split5, age_group) %>%
  summarise(popn = sum(tot_pop), .groups = "drop") %>%
  group_by(split5) %>%
  mutate(prop = popn / sum(popn))
plot_df$age_group <- factor(
  plot_df$age_group,
  levels = c("Elderly (61+)", "Adults (21-60)", "Kids (0-20)")
)

ggplot(plot_df, aes(x = factor(split5), y = prop, fill = age_group)) +
  geom_col(position = "fill", width = 0.8) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "IMD group",
    y = "Proportion of population",
    fill = "Age group",
    title = "Age structure by IMD"
  ) +
  theme_minimal()

### What about by gender? 

plot_df <- deprivationpops %>%
  mutate(
    age_group = case_when(
      age_band %in% c("0-5","6-10","11-20") ~ "Kids (0-20)",
      age_band %in% c("21-30","31-40","41-50","51-60") ~ "Adults (21-60)",
      age_band %in% c("61-70", "71-80", "81-90","91-100") ~ "Elderly (61+)"
    )
  ) %>%
  group_by(split5, gender, age_group) %>%
  summarise(popn = sum(tot_pop), .groups = "drop") %>%
  group_by(split5, gender) %>%
  mutate(prop = popn / sum(popn)) %>%
  ungroup() %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c("Kids (0-20)", "Adults (21-60)", "Elderly (61+)")
    ),
    prop_plot = if_else(gender == "males", -prop, prop)
  )

ggplot(plot_df, aes(x = factor(split5), y = prop, fill = age_group)) +
  geom_col(width = 0.8) +
  facet_wrap(~ gender) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "IMD",
    y = "Proportion",
    fill = "Age group",
    title = "Age structure by IMD and gender"
  ) +
  theme_minimal()

#### What is the population by IMD is rubbish? 
ons_pop <- read_csv("data/ons_popn_lad_age_sex.csv") |>
  clean_names() |>
  rename(lad_code = ladcode23) |>
  pivot_longer(cols = starts_with("population_"), values_to = "popn") %>%
  mutate(year = str_remove(name, "population_")) %>%
  group_by(sex, age, year) %>%
  summarise(tot_popn = sum(popn)) %>%
  mutate()

myeb <- ons_pop %>%
  mutate(
    age_band = case_when(
      age >= 0  & age <= 5   ~ "0-5",
      age >= 6  & age <= 10  ~ "6-10",
      age >= 11 & age <= 20  ~ "11-20",
      age >= 21 & age <= 30  ~ "21-30",
      age >= 31 & age <= 40  ~ "31-40",
      age >= 41 & age <= 50  ~ "41-50",
      age >= 51 & age <= 60  ~ "51-60",
      age >= 61 & age <= 70  ~ "61-70",
      age >= 71 & age <= 80  ~ "71-80",
      age >= 81 & age <= 89  ~ "81-90",
      age >= 90              ~ "91-100",
      TRUE ~ NA_character_
    ),
    age_band = factor(age_band, levels = c("0-5", "6-10", "11-20",
                                           "21-30", "31-40", "41-50",
                                           "51-60", "61-70", "71-80",
                                           "81-90", "91-100"))
  ) %>%
  filter(!is.na(age_band)) %>%
  group_by(age_band, sex,year) %>%        # adjust column names to match your file
  summarise(tot_pop = sum(tot_popn), .groups = "drop")


## Compare 
old_de <- demoninators %>% filter(age_band %in% c("61-70", "71-80", "71-80", "81-90", "91-100"))
sum(old_de$tot_pop)

myeb %>% filter(year == 2019,
                age_band %in% c("61-70", "71-80", "71-80", "81-90", "91-100")) %>%
  summarise(sum(tot_pop))

## More in IMD? 

# Summarise myeb 2019 by age and sex
myeb_2019_total <- myeb %>%
  filter(year == 2019) %>%
  group_by(age_band, sex) %>%
  mutate(
    source = "MYEB population",
    sex = recode(sex, "f" = "females", "m" = "males")
  ) %>%
  select(-year)

# Summarise denominators by age and gender
denom_totals <- demoninators %>%
  group_by(age_band, gender) %>%
  summarise(
    tot_reg = sum(tot_reg),
    tot_pop = sum(tot_pop),
    .groups = "drop"
  ) %>%
  rename(sex = gender) %>%
  pivot_longer(cols = c(tot_reg, tot_pop),
               names_to = "source",
               values_to = "tot_pop") %>%
  mutate(source = recode(source,
                         "tot_reg" = "GP registrations",
                         "tot_pop" = "IMD population"))

# Combine
plot_df <- bind_rows(myeb_2019_total, denom_totals)
plot_df$source <- factor(plot_df$source,
                         levels = c("MYEB population", "IMD population", "GP registrations"))

# Plot
ggplot(plot_df, aes(x = age_band, y = tot_pop, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Total population / registrations", labels = comma) +
  scale_x_discrete("Age band") +
  scale_fill_viridis_d(option = "C", end = 0.85) +
  facet_wrap(~ sex) +
  labs(title = "Population vs GP registrations by age band and sex (2019)",
       fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Ratio 
plot_df %>% pivot_wider(names_from = source, values_from = tot_pop) %>%
  mutate(ratio_reg_to_pop = `GP registrations` / `MYEB population`) %>%
  ggplot(aes(x = age_band, y = ratio_reg_to_pop, group = sex)) +
  geom_point(aes(col = sex), size = 3) +
  geom_line(aes(col = sex)) + 
  scale_y_continuous("Ratio: higher = more patients registered than population\ndenominator now MYEB", lim = c(0,1.25)) +
  scale_x_discrete("Age band") +
  geom_hline(yintercept = 1, linetype = "dashed")
