# Code for Figure 2

library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(viridis)
library(here)
setwd(here())
theme_set(theme_bw(base_size = 12))

rm(list = ls())
# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

# GP population data: by IMD + gender
totals_gp_bysplit10 <- read_csv("data/cleaned_totals_gp_bysplit10_sexgp.csv")

# imd population by age and sex for England
# from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020
#imd_pops <- read_csv("data/imd_popns.csv") %>% 
#  pivot_longer(cols = `0`:`90+`, names_to = "age", values_to = "popn")
# Updated 2025 populations FOR ENGLAND
#imd_pops <- read_csv("data/File_1_IoD2025 Index of Multiple Deprivation.csv")
# 2019 values for Englan dy age and sex
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/12386populationbyindexofmultipledeprivationimdengland2001to2019
imd_pops <- read_csv("data/popsbyimdengland20012019.csv")[,1:23] %>% 
  select(-year, imd:gender) %>%
  pivot_longer(cols = `<1`:`90+`, names_to = "age", values_to = "popn") 
sum(imd_pops$popn) # check = 56mil
  

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


############## Overall ########################################################
SWITCH_denominator <- "IMD" #"IMD" #or "GP"

if(SWITCH_denominator == "GP"){
  deprivationpops = totals_gp_bysplit10 %>% group_by(split10, PRACTICE_CODE) %>%
    summarise(tot = sum(total_population)) %>% ungroup() %>% group_by(split10) %>%
    summarise(total_patients = sum(tot))
  
  deprivationpops_5 <- as.data.frame(cbind(split5 = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                                          total_patients = matrix(0,5,1)))
  deprivationpops_5[which(deprivationpops_5$split5 == "Q1"),2] <- deprivationpops[which(deprivationpops$split10 == "Q1"),"total_patients"] + deprivationpops[which(deprivationpops$split10 == "Q2"),"total_patients"]
  deprivationpops_5[which(deprivationpops_5$split5 == "Q2"),2] <- deprivationpops[which(deprivationpops$split10 == "Q3"),"total_patients"] + deprivationpops[which(deprivationpops$split10 == "Q4"),"total_patients"]
  deprivationpops_5[which(deprivationpops_5$split5 == "Q3"),2] <- deprivationpops[which(deprivationpops$split10 == "Q5"),"total_patients"] + deprivationpops[which(deprivationpops$split10 == "Q6"),"total_patients"]
  deprivationpops_5[which(deprivationpops_5$split5 == "Q4"),2] <- deprivationpops[which(deprivationpops$split10 == "Q7"),"total_patients"] + deprivationpops[which(deprivationpops$split10 == "Q8"),"total_patients"]
  deprivationpops_5[which(deprivationpops_5$split5 == "Q5"),2] <- deprivationpops[which(deprivationpops$split10 == "Q9"),"total_patients"] + deprivationpops[which(deprivationpops$split10 == "Q10"),"total_patients"]
  colnames(deprivationpops_5) <- c("split5","total_patients")
  deprivationpops_5$total_patients <- as.numeric(deprivationpops_5$total_patients)
  deprivationpops <- deprivationpops_5
}else{ # IMD 
  deprivationpops = imd_pops %>% group_by(imd) %>%
    summarise(total_patients = sum(popn))
  #deprivationpops$split10 = paste0("Q",deprivationpops$imd)
  deprivationpops$split5 = paste0("Q",deprivationpops$imd)
}

data$split5 <- cut(
  data$IMD,
  breaks = quantile(data$IMD, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = c("Q5", "Q4", "Q3", "Q2", "Q1"),
  include.lowest = TRUE
)


final_highlevel = data %>%
  group_by(split5)%>%
  summarise(ITEMS = sum(ITEMS_1)) %>%
  left_join(deprivationpops) %>% #left_join(deprivationpops_5) %>%
  mutate(items_per_10000 = ITEMS / total_patients * 10000,
         items_per_1000_pd = (ITEMS/365) /  (total_patients) * 1000) %>% ungroup()

#final_highlevel$split10 <- factor(final_highlevel$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))
final_highlevel$split5 <- factor(final_highlevel$split5, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

g1 <- ggplot(final_highlevel, aes(x=split5, y = items_per_1000_pd)) + #items_per_10000_imdpop_pd)) + #
  geom_bar(stat="identity") + 
  theme_minimal() + 
  labs(x = "Quintile",
       y = "Items per 1,000 Patients per day") #+ 
g1
# compare to box figure 3.4.1 from ESPAUR => get the same effect with IMD populations sizes but not GP registered denominator 

############## By gender  ########################################################
### If include gender how does it vary? 
# population sizes
if(SWITCH_denominator == "GP"){
  totals_gp_bysplit10_sexgp <- read_csv("data/cleaned_totals_gp_bysplit10_sexgp.csv")
  overall = data %>%
    #filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
    group_by(split10, GENDER, PRACTICE_CODE) %>%
    summarise(ITEMS =  sum(ITEMS_1)) %>%
    left_join(totals_gp_bysplit10_sexgp) %>%
    mutate(items_per_10000 = ITEMS / total_population * 10000)
}else{# IMD
  imd_pops_sex <- imd_pops %>% group_by(sex, imd) %>% summarise(total_population = sum(popn)) %>% rename(GENDER = sex)
  imd_pops_sex$split10 = paste0("Q",imd_pops_sex$imd)
  
  overall = data %>%
    #filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
    group_by(split10, GENDER) %>%
    summarise(ITEMS =  sum(ITEMS_1)) %>% 
    ungroup() %>% 
    left_join(imd_pops_sex) %>%
    mutate(items_per_10000 = ITEMS / total_population * 10000)
  
}

overall$split10 <- factor(overall$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))#c("Q10", "Q9", "Q8", "Q7", "Q6", "Q5", "Q4", "Q3", "Q2", "Q1"))

# Take mean 
overall_mean <- overall %>% group_by(split10, GENDER) %>%
  summarise(mean = mean(items_per_10000),
            sd = sd(items_per_10000))
## Mean over GP level per 10000 by gender
ggplot(overall_mean, aes(x=split10, y = mean, group = GENDER)) + 
  geom_line(aes(colour = GENDER)) + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, group = GENDER, colour = GENDER)) + 
  theme_minimal() + 
  scale_y_continuous(lim = c(0,20000)) + 
  labs(x = "Quintile",
       y = "Items per 10,000 Patients per month",
       linetype = "Gender") 


ggplot(overall %>% filter(items_per_10000 < 20000), aes(x = split10, y = items_per_10000, colour = GENDER, group = GENDER)) + 
  #  geom_jitter() + 
  geom_boxplot(aes(group = interaction(GENDER,split10))) + 
  #geom_line(aes(group = GENDER)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_y_continuous(lim = c(0,500)) + 
  labs(x = "Quintile",
       y = "Items per 10,000 Patients per month",
       linetype = "Gender") + 
  geom_line(data = overall_mean, aes(x=split10, y = mean, group = GENDER)) + 
  scale_y_continuous(lim = c(0,20000))

# Some big outliers - more in later quintiles (more deprived)
overall %>% filter(items_per_10000 > 20000)
# Y02751 also an outlier in Open Prescribing
data %>% filter(PRACTICE_CODE == "Y02751") %>% filter(ITEMS_1 > 0, AGE_BAND == "0-5") %>% dplyr::select(split10, PRACTICE_CODE,AGE_BAND, ITEMS_1, ANTIBIOTIC_GROUP)
sub <- data %>% filter(PRACTICE_CODE %in% c("J82208", "Y02751","A81004")) # random Q7 practices and high one Y02751
sub$AGE_BAND <- factor(sub$AGE_BAND, 
                       levels = c("0-5", "6-10", "11-20", 
                                  "21-30", "31-40", "41-50", 
                                  "51-60", "61-70", "71-80", 
                                  "81-90", "91-100"))
ggplot(sub, aes(x=AGE_BAND, y = items_per_patient, group = interaction(PRACTICE_CODE, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE))) + 
  geom_line(aes(col = PRACTICE_CODE)) + 
  facet_wrap(~ANTIBIOTIC_GROUP) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
sub <- data %>% filter(PRACTICE_CODE %in% c("J82208", "Y02751","A81004","Y02625","Y02494")) # random Q7 practices and high others
sub <- sub %>% mutate(high = ifelse(PRACTICE_CODE %in% c("Y02751","Y02625","Y02494"),1,0))
sub$AGE_BAND <- factor(sub$AGE_BAND, 
                       levels = c("0-5", "6-10", "11-20", 
                                  "21-30", "31-40", "41-50", 
                                  "51-60", "61-70", "71-80", 
                                  "81-90", "91-100"))
ggplot(sub, aes(x=AGE_BAND, y = items_per_patient, group = interaction(PRACTICE_CODE, GENDER, BNF_CHEMICAL_SUBSTANCE_CODE))) + 
  geom_line(aes(col = factor(high))) + 
  facet_wrap(~ANTIBIOTIC_GROUP) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sub %>% filter(high == 1) %>% dplyr::select(c(Area.Name, Parent.Name)) %>% group_by(Parent.Name) %>% slice(1)

############## By gender and antibiotic group ############################
if(SWITCH_denominator == "GP"){
  final = data %>%
    filter(!is.na(ANTIBIOTIC_GROUP)) %>% 
    group_by(split10, GENDER, ANTIBIOTIC_GROUP)%>%
    summarise(ITEMS = sum(ITEMS_1)) %>%
    left_join(totals_gp_bysplit10) %>%
    mutate(items_per_1000 = (ITEMS/30) / total_population * 1000)
}else{ #IMD
  final = data %>%
    filter(!is.na(ANTIBIOTIC_GROUP)) %>% 
    group_by(split10, GENDER, ANTIBIOTIC_GROUP)%>%
    summarise(ITEMS = sum(ITEMS_1)) %>%
    left_join(imd_pops_sex) %>%
    mutate(items_per_1000 = (ITEMS/30) / total_population * 1000)
}

final$split10 <- factor(final$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

# colours
turbo_colours <- c("#30123BFF", "#424AB3FF", "#467EF4FF", "#31AFF5FF", "#18DAC7FF", 
                   "#38F491FF", "#83FF52FF", "#BDF534FF", "#E9D539FF", "#FEAA33FF", 
                   "#F8721CFF", "#E03F08FF", "#B61C02FF","#808")

final$ANTIBIOTIC_GROUP <- as.factor(final$ANTIBIOTIC_GROUP)
final$GENDER <- as.factor(final$GENDER)
g1 <- ggplot(final, aes(x = split10, y = items_per_1000, color = ANTIBIOTIC_GROUP, 
                        group = interaction(GENDER,ANTIBIOTIC_GROUP), linetype = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender",
    x = "Quintile",
    y = "Items per 1,000 Patients per day",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(col=guide_legend(ncol=2), linetype=guide_legend(ncol=2)) + 
  #scale_color_viridis_d(option = "turbo") 
  scale_color_manual(values = turbo_colors) #viridis_d(option = "turbo") 
ggsave("plots/figure2a.jpeg")


ggplot(final, aes(x = split10, y = items_per_1000, color = ANTIBIOTIC_GROUP, 
                  group = interaction(GENDER, ANTIBIOTIC_GROUP), linetype = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  facet_wrap(~GENDER) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender zoomed",
    x = "Quintile",
    y = "Items per 1,000 Patients per day",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(col=guide_legend(ncol=2), linetype=guide_legend(ncol=2)) + 
  scale_color_viridis_d(option = "turbo") +
  ylim(0, 5)

g2 <- ggplot(final %>% filter(!ANTIBIOTIC_GROUP == "Penicillins"), 
             aes(x = split10, y = items_per_1000, fill = ANTIBIOTIC_GROUP, 
                 group = interaction(GENDER, ANTIBIOTIC_GROUP))) +
  geom_area() + 
  #geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  facet_wrap(~GENDER) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender zoomed",
    x = "Quintile",
    y = "Items per 1,000 Patients per day",
    fill = "Antibiotic Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(col=guide_legend(ncol=2), linetype=guide_legend(ncol=2)) + 
  #scale_color_viridis_d(option = "turbo")
  scale_fill_manual(values = turbo_colors) #viridis_d(option = "turbo") 

ggsave("plots/figure2b.jpeg")

#### Age/sex distribution by quintile 
if(SWITCH_denominator == "GP"){
  totals_gp_bysplit10_as <- read_csv("data/cleaned_totals_gp_bysplit10_agesex.csv")
  totals_gp_bysplit10_as$split10 <- factor(totals_gp_bysplit10_as$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))
  totals_gp_bysplit10_as <- as.data.frame(totals_gp_bysplit10_as)
}else{#IMD
  totals_imd_bysplit10_as <- imd_pops
}

# Visualise this
ggplot(totals_gp_bysplit10_as, aes(x = factor(split10), y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Demographic split by IMD Decile and Sex",
    x = "IMD Decile",
    y = "Population proportion"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)
ggsave("plots/age_sex_split_by_IMD.jpeg")

ggplot(totals_gp_bysplit10_as, aes(x = factor(split10), y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Demographic split by IMD Decile and Sex",
    x = "IMD Decile",
    y = "Population size"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)
ggsave("plots/age_sex_split_by_IMD_numbers.jpeg")

g3 <- ggplot(totals_gp_bysplit10_as %>% filter(split10 %in% c("Q1", "Q5", "Q10")), aes(x = GENDER, y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity") +
  #  guides(fill=guide_legend(ncol=2)) + 
  labs(
    x = "Gender",
    y = "Population size"
  ) +
  scale_fill_discrete("Age Band") + 
  theme_minimal() + 
  facet_wrap(~factor(split10), ncol = 3)

ggsave("plots/figure2c.jpeg")

g4 <- ggplot(totals_gp_bysplit10_as %>% filter(split10 %in% c("Q1", "Q5", "Q10")), aes(x = GENDER, y = total_population, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  #  guides(fill=guide_legend(ncol=2)) + 
  labs(
    x = "Gender",
    y = "Population proportions"
  ) +
  scale_fill_discrete("Age Band") + 
  theme_minimal() + 
  facet_wrap(~factor(split10), ncol = 3)

ggsave("plots/figure2d.jpeg")

## Prescription rates by age sex
age_sex_imd = data %>%
  group_by(split10, GENDER, AGE_BAND)%>%
  summarise(ITEMS = sum(ITEMS_1)) %>% 
  #agesexpop = sum(total_patients)) %>% # this doesn't work as doesn't sum those with no prescriptions
  left_join(totals_gp_bysplit10_as) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

age_sex_imd$split10 <- factor(age_sex_imd$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

age_sex_imd$AGE_BAND <- factor(age_sex_imd$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))


ggplot(age_sex_imd, aes(x = factor(split10), y = items_per_10000, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Antibiotic Prescriptions by IMD Decile, Age Band, and Sex",
    x = "IMD Decile",
    y = "Number of Prescriptions"
  ) +
  theme_minimal() + 
  facet_wrap(~GENDER)

### each antibiotic 
age_sex_imd = data %>%
  group_by(split10, GENDER, AGE_BAND, ANTIBIOTIC_GROUP)%>%
  summarise(ITEMS = sum(ITEMS_1)) %>% 
  #agesexpop = sum(total_patients)) %>% # this doesn't work as doesn't sum those with no prescriptions
  left_join(totals_gp_bysplit10_as) %>%
  mutate(items_per_10000 = ITEMS / total_population * 10000)

age_sex_imd$split10 <- factor(age_sex_imd$split10, levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

age_sex_imd$AGE_BAND <- factor(age_sex_imd$AGE_BAND, 
                               levels = c("0-5", "6-10", "11-20", 
                                          "21-30", "31-40", "41-50", 
                                          "51-60", "61-70", "71-80", 
                                          "81-90", "91-100"))


ggplot(age_sex_imd, aes(x = factor(split10), y = items_per_10000, fill = AGE_BAND)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Antibiotic Prescriptions by IMD Decile, Age Band, and Sex",
    x = "IMD Decile",
    y = "Number of Prescriptions"
  ) +
  theme_minimal() + 
  facet_grid(ANTIBIOTIC_GROUP~GENDER)

ggplot(age_sex_imd, aes(x = split10, y = items_per_10000, 
                        color = ANTIBIOTIC_GROUP, group = interaction(GENDER, ANTIBIOTIC_GROUP), 
                        linetype = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position as needed
  #           show.legend = FALSE) +
  labs(#title = "Antibiotic Usage Across Quintiles by Gender",
    x = "Quintile",
    y = "Items per 10,000 Patients per month",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(option = "turbo") + 
  facet_wrap(~AGE_BAND)

#### Join most useful together
library(patchwork) 
(g1 + g2 + plot_layout(guides = "collect", widths = c(2,2))) / (g3 + g4 + plot_layout(guides = "collect", widths = c(2,2))) & plot_annotation(tag_levels = 'A')
ggsave("plots/figure2.jpeg", width = 12, height = 8)






## Do men get more pencillins than women? Does this explain it? 
data %>% filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
  group_by(GENDER) %>%
  summarise(sum(ITEMS_1)) 
# No... 
# Does a certain age group get more penicillins? 
data$AGE_BAND <- factor(data$AGE_BAND, 
                        levels = c("0-5", "6-10", "11-20", 
                                   "21-30", "31-40", "41-50", 
                                   "51-60", "61-70", "71-80", 
                                   "81-90", "91-100"))
totals_age <- totals_gp_bysplit10_as %>%
  group_by(AGE_BAND) %>%
  summarise(total = sum(total_population))
pen_age <- data %>% filter(ANTIBIOTIC_GROUP == "Penicillins") %>%
  group_by(AGE_BAND) %>%
  summarise(total_items = sum(ITEMS_1)) %>%
  left_join(totals_age) %>%
  mutate(items_per_10000 = (total_items / total)*10000) %>%
  arrange(items_per_10000)
pen_age$AGE_BAND <- factor(pen_age$AGE_BAND, 
                           levels = c("0-5", "6-10", "11-20", 
                                      "21-30", "31-40", "41-50", 
                                      "51-60", "61-70", "71-80", 
                                      "81-90", "91-100"))


ggplot(pen_age, aes(x=AGE_BAND, y = items_per_10000)) + 
  geom_point() + 
  geom_line() + 
  theme_minimal()

## yes more in kids and v old... 
## 



## #### Variation between registrations and total 
### GP level registrations summed over IMD
gp_registrations = data %>% 
    # Select first row for each practice code and age band to avoid double counting
    group_by(AGE_BAND, GENDER, PRACTICE_CODE) %>% 
    slice(1) %>% 
    ungroup() %>%
    select(AGE_BAND, GENDER, PRACTICE_CODE, total_patients, split5) %>%
    group_by(split10, AGE_BAND, GENDER) %>% 
    summarise(tot_reg = sum(total_patients)) 
colnames(gp_registrations) <- tolower(colnames(gp_registrations))

# Recode gender
gp_registrations$gender <- dplyr::recode(gp_registrations$gender, "Female" = "females", "Male" = "males")

### Population at IMD level 
#colnames(imd_pops) <- c("age","imd_dep","age","popn","AGE_BAND")
colnames(imd_pops) <- tolower(colnames(imd_pops))

deprivationpops = imd_pops %>% 
  group_by(gender, age_band, imd) %>%
    summarise(tot_pop = sum(popn))
deprivationpops$split5 = paste0("Q",deprivationpops$imd)
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
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggtitle("Q1 = most deprived, Q10 = least deprived")

sum(demoninators$tot_reg) / sum(demoninators$tot_pop)


# Are there more old people in the least deprived? 
# yes 
imd_pops %>% group_by(imd) %>%
  summarise(tot_pop = sum(popn),
            old_pop = sum(popn[age %in% c("65-69","70-74","75-79","80-84", "85-89", "90+")]),
            prop_old = old_pop / tot_pop)

# Are there more women in the least deprived?
# no more in least deprived 
imd_pops %>% group_by(imd, gender) %>%
  summarise(tot_pop = sum(popn)) %>%
  pivot_wider(names_from = gender, values_from = tot_pop) %>%
  mutate(gender_prop = females / (females + males))
            

