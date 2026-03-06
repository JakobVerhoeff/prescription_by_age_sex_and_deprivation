##### Run negative binomial model for ALL antibiotics 

#detach("package:MASS", unload = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)
library(MASS)
library(DescTools) # pseudo R2 calc
library(pscl)
library(jtools) # for summary 
library(stats) # for AIC
library(DHARMa) # for model checking 
library(glmmTMB) # for zero inflation models
setwd(here())

# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

# Change the agebands into numerical midpoints
data$AGE_BAND_NUM <- with(data, 
                          ifelse(AGE_BAND == "0-5", 2.5,
                                 ifelse(AGE_BAND == "6-10", 8,
                                        ifelse(AGE_BAND == "11-20", 15,
                                               ifelse(AGE_BAND == "21-30", 25,
                                                      ifelse(AGE_BAND == "31-40", 35,
                                                             ifelse(AGE_BAND == "41-50", 45,
                                                                    ifelse(AGE_BAND == "51-60", 55,
                                                                           ifelse(AGE_BAND == "61-70", 65,
                                                                                  ifelse(AGE_BAND == "71-80", 75,
                                                                                         ifelse(AGE_BAND == "81-90", 85,
                                                                                                ifelse(AGE_BAND == "91-100", 95, NA))))))))))))

data$AGE_BAND_NUM2 <- data$AGE_BAND_NUM^2  # Create squared age term


#######################################################################################################
### data has usage for all antibiotic classes #########################################################
### data_foi has for ALL classes together  ############################################################
#######################################################################################################

data_foi <- data %>%
  group_by(GENDER, AGE_BAND, AGE_BAND_NUM, AGE_BAND_NUM2, PRACTICE_CODE, IMD) %>%
  summarise(
    TOTAL_ITEMS = sum(ITEMS_1, na.rm = TRUE),
    TOTAL_ITEMS4 = sum(ITEMS_4, na.rm = TRUE),
    TOTAL_PATIENTS = first(total_patients),
    .groups = "drop"
  ) %>% 
  filter(TOTAL_PATIENTS > 0)

# Centre?
data_foi <- data_foi %>%
  mutate(
    AGE_c = AGE_BAND_NUM - mean(AGE_BAND_NUM, na.rm = TRUE),
    AGE_c2 = AGE_c^2
  )

## Check same as prelimnary analysis? yes 
#foidata %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "Age 0-5")
#data_foi %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "0-5")

#######################################################################################################
### data_foi CONTAINS all ITEMS with redacted to 1, or ITEMS4 with redacted to 4, population of GP ####
### IMD (WHICH IS THE IMD2019 VALUE FOR THE CORRESPONDING GP) FIVE-YEAR AGE BANDS AND SEX #############
##### Fit the negative binomial model with the offset manually included ###############################
#######################################################################################################

# All model with build up of age, sex, deprivation up to those with age-sex interaction term and age^2
nb_s<-glm.nb(TOTAL_ITEMS ~  GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_a<-glm.nb(TOTAL_ITEMS ~  AGE_c + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_d<-glm.nb(TOTAL_ITEMS ~  IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_sa<-glm.nb(TOTAL_ITEMS ~  AGE_c + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_sd<-glm.nb(TOTAL_ITEMS ~  GENDER + IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_ad<-glm.nb(TOTAL_ITEMS ~  AGE_c + IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asd<-glm.nb(TOTAL_ITEMS ~  AGE_c + IMD + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asda2<-glm.nb(TOTAL_ITEMS ~  AGE_c + AGE_c2 + IMD + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asa2i<-glm.nb(TOTAL_ITEMS ~  AGE_c + AGE_c2 + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_all<-glm.nb(TOTAL_ITEMS ~  AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
pois_all <- glm(TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), 
                family = poisson, data = data_foi)
nb_all_pos<-glm.nb(TOTAL_ITEMS ~  AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi %>% filter(TOTAL_ITEMS > 0))
pois_all_pos <- glm(TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), 
                family = poisson, data = data_foi%>% filter(TOTAL_ITEMS > 0))

nb_all<-glm.nb(TOTAL_ITEMS ~  AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)

# Check NB ok over Poisson 
AIC(nb_all, pois_all) # yes AIC lower for nb.
AIC(pois_all_pos, nb_all_pos) # yes AIC lower for nb. better to include zero data too?


# Check residuals
sim_res <- simulateResiduals(fittedModel = nb_all)
plot(sim_res)
testOutliers(sim_res, type = "bootstrap")
testDispersion(sim_res)
testZeroInflation(sim_res)
testUniformity(sim_res)

# need to zero inflate? do zi for nb  
zinb_model <- glmmTMB(
  TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + offset(log(TOTAL_PATIENTS)),
  ziformula = ~1,   # simple model for zero-inflation part (only intercept)
  family = nbinom2,
  data = data_foi
)

sim_res_zinb <- simulateResiduals(zinb_model)
plot(sim_res_zinb)
testDispersion(sim_res_zinb)
testZeroInflation(sim_res_zinb)
testUniformity(sim_res_zinb)

# GP random additionally too? 
zinbgp_model <- glmmTMB(
  TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER + AGE_c:GENDER + 
    offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
  ziformula = ~ 1,
  family = nbinom2,
  data = data_foi
)
sim_res_zinbgp <- simulateResiduals(zinbgp_model)
plot(sim_res_zinbgp)
testDispersion(sim_res_zinbgp)
testZeroInflation(sim_res_zinbgp)
testUniformity(sim_res_zinbgp)

AIC(zinbgp_model, nb_all) # zero inflation and random effects at GP practice better fit 

# Extract model summary
coefs <- summary(zinbgp_model)$coefficients$cond

# Create data frame of IRRs
IRR_table <- data.frame(
  Variable = rownames(coefs),
  Estimate = exp(coefs[, "Estimate"]),
  `2.5 %` = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
  `97.5 %` = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]),
  p_value = coefs[, "Pr(>|z|)"]
)

# View table
print(IRR_table, digits = 3)

# PLOT
data_foi$predicted_counts <- predict(zinbgp_model, type = "response")
data_foi$predicted_rate <- (data_foi$predicted_counts / data_foi$TOTAL_PATIENTS) * 1000

plot_data <- data_foi %>%
  group_by(IMD, GENDER) %>%
  summarise(
    predicted_rate = mean(predicted_rate),
    .groups = "drop"
  )

ggplot(plot_data, aes(x = IMD, y = predicted_rate, color = GENDER)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Predicted Antibiotic Prescribing Rates by IMD and Gender",
    x = "IMD Score",
    y = "Prescriptions per 1,000 Patients"
  ) +
  theme_minimal()

## compare
data_foi$observed_rate <- (data_foi$TOTAL_ITEMS / data_foi$TOTAL_PATIENTS) * 1000
plot_comparison <- data_foi %>%
  ungroup() %>%
  mutate(IMD_decile = ntile(IMD, 10)) %>%
  group_by(IMD_decile, GENDER) %>%
  summarise(
    observed_rate = mean(observed_rate),
    predicted_rate = mean(predicted_rate),
    .groups = "drop"
  )

ggplot(plot_comparison, aes(x = IMD_decile)) +
  geom_line(aes(y = observed_rate, color = GENDER), size = 1, linetype = "dashed") +
  geom_line(aes(y = predicted_rate, color = GENDER), size = 1) +
  labs(
    title = "Observed vs Predicted Prescribing Rates by IMD Decile",
    x = "IMD Decile",
    y = "Prescriptions per 1,000 Patients",
    color = "Gender"
  ) +
  scale_linetype_manual(values = c("Predicted" = "solid", "Observed" = "dashed")) +
  theme_minimal()

#### Summarise by IMD / GENDER / AGE BAND
plot_comparison <- data_foi %>%
  ungroup() %>%
  mutate(IMD_decile = ntile(IMD, 10)) %>%
  group_by(IMD_decile, GENDER, AGE_BAND_NUM) %>%
  summarise(
    observed_rate = mean(observed_rate),
    predicted_rate = mean(predicted_rate),
    .groups = "drop"
  )

ggplot(plot_comparison, aes(x = IMD_decile)) +
  geom_line(aes(y = observed_rate, color = GENDER), linetype = "dashed", size = 1) +
  geom_line(aes(y = predicted_rate, color = GENDER), size = 1) +
  facet_wrap(~ AGE_BAND_NUM, ncol = 3) +
  labs(
    title = "Observed vs Predicted Prescribing Rates by IMD Decile and Age Band",
    x = "IMD Decile (1 = Least Deprived, 10 = Most Deprived)",
    y = "Prescriptions per 1,000 Patients",
    color = "Gender"
  ) +
  theme_minimal()

ggplot(plot_comparison, aes(x = IMD_decile)) +
  geom_line(aes(y = observed_rate, color = factor(AGE_BAND_NUM)), linetype = "dashed", size = 1) +
  geom_line(aes(y = predicted_rate, color = factor(AGE_BAND_NUM)), size = 1) +
  facet_wrap(~ GENDER) +
  labs(
    title = "Observed vs Predicted Prescribing Rates by IMD Decile and Age Band",
    x = "IMD Decile (1 = Least Deprived, 10 = Most Deprived)",
    y = "Prescriptions per 1,000 Patients",
    color = "AGE"
  ) +
  theme_minimal()

### Overestimates for women? 
data_foi$rate_residuals <- data_foi$observed_rate - data_foi$predicted_rate
data_foi <- data_foi %>% ungroup() %>% mutate(IMD_decile = ntile(IMD, 10))
ggplot(data_foi, aes(x = IMD_decile, y = rate_residuals, color = GENDER)) +
  geom_boxplot() +
  facet_wrap(~ AGE_BAND_NUM) +
  labs(title = "Prediction Residuals by IMD Decile, Gender, and Age Band",
       y = "Observed - Predicted Prescribing Rate")

## Try gender : IMD 
zinbg_model <- glmmTMB(
  TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER +
    AGE_c:GENDER + IMD:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
  ziformula = ~ 1,
  family = nbinom2,
  data = data_foi
)

AIC(zinbg_model, zinbgp_model, nb_all)

data.frame(
  Model = c("nb_all", "zinbgp_model", "zinbg_model"),
  df = c(attr(logLik(nb_all), "df"),
         attr(logLik(zinbgp_model), "df"),
         attr(logLik(zinbg_model), "df")),
  AIC = AIC(nb_all, zinbgp_model, zinbg_model)$AIC,
  LogLik = c(logLik(nb_all),
             logLik(zinbgp_model),
             logLik(zinbg_model))
)

## only one line per practice? 
anyDuplicated(data_diab_copd$PRACTICE_CODE)


#### What about comorbidities? 
data_diab_copd <- read_csv("data/PREVALENCE_1920.csv") %>%
  filter(GROUP_CODE %in% c("COPD", "DM")) %>% # get COPD and Diabetes
  dplyr::select(GROUP_CODE, REGISTER, PRACTICE_CODE) %>% 
  pivot_wider(names_from = GROUP_CODE, values_from = REGISTER)

# What does patient_list mean? vs register? 
# check same as other publications  ### TODO

data_foi_com <- left_join(data_foi, data_diab_copd, by = "PRACTICE_CODE")

## Try gender : IMD 
zinbcopd_model <- glmmTMB(
  TOTAL_ITEMS ~ AGE_c + AGE_c2 + IMD + GENDER + COPD + DM + 
    AGE_c:GENDER + IMD:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
  ziformula = ~ 1,
  family = nbinom2,
  data = data_foi_com
)

AIC(zinbcopd_model, zinbgp_model, nb_all)

data_foi_com$predicted_counts <- predict(zinbcopd_model, type = "response")
data_foi_com$predicted_rate <- (data_foi_com$predicted_counts / data_foi_com$TOTAL_PATIENTS) * 1000
