##### Run negative binomial model for each antibiotic family separately 

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

#### Comorbidities
data_diab_copd <- read_csv("data/PREVALENCE_1920.csv") %>%
  filter(GROUP_CODE %in% c("COPD", "DM")) %>% # get COPD and Diabetes
  dplyr::select(GROUP_CODE, REGISTER, PRACTICE_CODE) %>% 
  pivot_wider(names_from = GROUP_CODE, values_from = REGISTER)

data <- left_join(data, data_diab_copd)

#######################################################################################################
### data_foi CONTAINS all ITEMS with redacted to 1, or ITEMS4 with redacted to 4, population of GP ####
### IMD (WHICH IS THE IMD2019 VALUE FOR THE CORRESPONDING GP) FIVE-YEAR AGE BANDS AND SEX #############
##### Fit the negative binomial model with the offset manually included ###############################
#######################################################################################################

u <- unique(data$ANTIBIOTIC_GROUP)

for(i in u){
  
  data_foi <- data %>% filter(ANTIBIOTIC_GROUP == i)
  print(i)
  
  # All model with build up of age, sex, deprivation up to those with age-sex interaction term and age^2
  nb_s<-glm.nb(ITEMS_1 ~  GENDER + offset(log(total_patients)), data = data_foi)
  nb_a<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + offset(log(total_patients)), data = data_foi)
  nb_d<-glm.nb(ITEMS_1 ~  IMD + offset(log(total_patients)), data = data_foi)
  nb_sa<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + GENDER + offset(log(total_patients)), data = data_foi)
  nb_sd<-glm.nb(ITEMS_1 ~  GENDER + IMD + offset(log(total_patients)), data = data_foi)
  nb_ad<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + IMD + offset(log(total_patients)), data = data_foi)
  nb_asd<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + IMD + GENDER + offset(log(total_patients)), data = data_foi)
  nb_asda2<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + offset(log(total_patients)), data = data_foi)
  nb_asa2i<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + AGE_BAND_NUM2 + GENDER + AGE_BAND_NUM:GENDER + offset(log(total_patients)), data = data_foi)
  nb_all<-glm.nb(ITEMS_1 ~  AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(total_patients)), data = data_foi)
  
  zinb_model <- glmmTMB(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)),
    ziformula = ~1,   # simple model for zero-inflation part (only intercept)
    family = nbinom2, data = data_foi)
  
  zinbgp_model <- glmmTMB(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + 
                            offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
    ziformula = ~ 1,
    family = nbinom2, data = data_foi)
  
  zinbg_model <- glmmTMB(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + 
                           IMD:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
    ziformula = ~ 1,
    family = nbinom2, data = data_foi)

  zinbcopd_model <- glmmTMB(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + COPD + DM + 
      AGE_BAND_NUM:GENDER + IMD:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_CODE),
    ziformula = ~ 1,
    family = nbinom2, data = data_foi)
  
  #model.set <- c(nb_s, nb_a, nb_d, nb_sa, nb_sd, nb_ad, nb_asd, nb_asda2, nb_asa2i, nb_all)
  
  #aictab(model.set)
  #pR2(nb_s)
  
  # Save outputs
  outputs <- as.data.frame(matrix(0, 10, 4))
  colnames(outputs) <- c("abx", "names","AIC", "pR2")
  outputs$names <- c("sex", "age", "IMD", "sex + age", "sex + IMD","age + IMD",
                     "sex + age + IMD", "sex + age + IMD + age^2", "sex + age + age^2 + age*sex","all",
                     "zero-inflated all", "zero-inflated all + practice effects", 
                     "zero-inflated + IMD*sex", "zero-inflated + IMD*sex + comorbid")
  outputs$AIC <- c(nb_s$aic, nb_a$aic, nb_d$aic, nb_sa$aic, nb_sd$aic, nb_ad$aic, 
                   nb_asd$aic, nb_asda2$aic, nb_asa2i$aic, nb_all$aic,
                   zinb_model$aic, zinbgp_model$aic,zinbg_model$aic,zinbcopd_model$aic)
  outputs$pR2 <- c(pR2(nb_s)["r2CU"], pR2(nb_a)["r2CU"], pR2(nb_d)["r2CU"], pR2(nb_sa)["r2CU"], pR2(nb_sd)["r2CU"], pR2(nb_ad)["r2CU"], 
                   pR2(nb_asd)["r2CU"], pR2(nb_asda2)["r2CU"], pR2(nb_asa2i)["r2CU"], pR2(nb_all)["r2CU"],
                   )
  outputs$abx <- i
  
  outputs <- outputs %>% arrange((AIC))
  
  outputs %>% arrange(desc(pR2))
  
  write_csv(outputs,paste0("output/NB_", i, "_antibiotics.csv"))
  
  ###### plot
  # Get predicted values from the negative binomial model
  predicted_values <- predict(nb_all, type = "response")
  # Create a data frame with actual and predicted values
  plot_data <- data.frame(
    Actual = data_foi$ITEMS_1,
    Predicted = predicted_values
  )
  
  # Create the plot
  ggplot(plot_data, aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue") +  # Plot the actual vs predicted points
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add perfect fit line
    geom_smooth(method = "lm", color = "green", se = FALSE) +  # Add green linear regression line
    labs(title = "Actual vs Predicted Values negative binomial", 
         x = "Actual Values", 
         y = "Predicted Values") +
    theme_minimal()
  ggsave(paste0("plots/nb_",i,".pdf"))
}

###########################################################################################
### Read back in and compare ##############################################################
###########################################################################################
files <- list.files("output",pattern="NB")
combine <- c()
for(i in files){
  file <- read_csv(paste0("output/",i))
  combine <- rbind(combine, file)
}

combine %>% filter(names == "all")

combine %>% group_by(abx) %>% 
  arrange(AIC) %>%
  filter(row_number()==1)


  