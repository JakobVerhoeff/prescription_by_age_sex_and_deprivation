### Add in diabetes and COPD 

data_diab_copd <- read_csv("data/PREVALENCE_1920.csv") %>%
  filter(GROUP_CODE %in% c("COPD", "DM")) %>% # get COPD and Diabetes
  dplyr::select(GROUP_CODE, REGISTER, PRACTICE_CODE) %>% 
  pivot_wider(names_from = GROUP_CODE, values_from = REGISTER)

# What does patient_list mean? vs register? 
# check same as other publications 

data_foi_com <- left_join(data_foi, data_diab_copd)

## Try gender : IMD 
zinb_model5 <- glmmTMB(
  TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + COPD + DM + 
    AGE_BAND_NUM:GENDER + IMD:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
  ziformula = ~ 1,
  family = nbinom2,
  data = data_foi_com
)

AIC(zinb_model5, zinb_model4, nb_all)

data_foi_com$predicted_counts <- predict(zinb_model5, type = "response")
data_foi_com$predicted_rate <- (data_foi_com$predicted_counts / data_foi_com$TOTAL_PATIENTS) * 1000
