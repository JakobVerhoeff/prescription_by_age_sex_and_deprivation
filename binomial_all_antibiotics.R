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

data_foi <- data %>% group_by(GENDER, AGE_BAND, AGE_BAND_NUM, AGE_BAND_NUM2, PRACTICE_CODE, IMD) %>% 
  summarise(TOTAL_ITEMS = sum(ITEMS), 
            TOTAL_ITEMS4 = sum(ITEMS4),
            TOTAL_PATIENTS = min(total_patients)) 

## Check same as Jakob's yup 
#foidata %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "Age 0-5")
#data_foi %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "0-5")

#######################################################################################################
### data_foi CONTAINS all ITEMS with redacted to 1, or ITEMS4 with redacted to 4, population of GP ####
### IMD (WHICH IS THE IMD2019 VALUE FOR THE CORRESPONDING GP) FIVE-YEAR AGE BANDS AND SEX #############
##### Fit the negative binomial model with the offset manually included ###############################
#######################################################################################################

# All model with build up of age, sex, deprivation up to those with age-sex interaction term and age^2
nb_s<-glm.nb(TOTAL_ITEMS ~  GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_a<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_d<-glm.nb(TOTAL_ITEMS ~  IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_sa<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_sd<-glm.nb(TOTAL_ITEMS ~  GENDER + IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_ad<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + IMD + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asd<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + IMD + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asda2<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_asa2i<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + AGE_BAND_NUM2 + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
nb_all<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)
pois_all <- glm(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)), 
                family = poisson, data = data_foi)

# Check NB ok over Poisson 
AIC(nb_all, pois_all) # yes AIC lower for nb

# Centre age
data_foi$AGE_BAND_NUM_c <- scale(data_foi$AGE_BAND_NUM, center = TRUE, scale = FALSE)
data_foi$AGE_BAND_NUM2_c <- data_foi$AGE_BAND_NUM_c^2
nb_all_c<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM_c + AGE_BAND_NUM2_c + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)), data = data_foi)

# Residuals
library(DHARMa)
sim_res <- simulateResiduals(fittedModel = nb_all_c)
plot(sim_res)
testOutliers(sim_res, type = "bootstrap")

# Residuals
par(mfrow=c(2,2))
plot(nb_all)
dev.off()
resid_pearson <- residuals(nb_all, type = "pearson")
plot(fitted(nb_all), resid_pearson,
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
library(DHARMa)
sim_res <- simulateResiduals(nb_all)
plot(sim_res)
#model.set <- c(nb_s, nb_a, nb_d, nb_sa, nb_sd, nb_ad, nb_asd, nb_asda2, nb_asa2i, nb_all)
library(glmmTMB)
nb_mixed <- glmmTMB(TOTAL_ITEMS ~ AGE_BAND_NUM + AGE_BAND_NUM2 + IMD + GENDER + AGE_BAND_NUM:GENDER + offset(log(TOTAL_PATIENTS)) + (1 | PRACTICE_ID), 
                    family = nbinom2, data = data_foi)

sim_mixed <- simulateResiduals(nb_mixed)
plot(sim_mixed)
AIC(nb_all, pois_all, nb_mixed) 
#aictab(model.set)
#pR2(nb_s)

outputs <- as.data.frame(matrix(0, 10, 4))
colnames(outputs) <- c("abx","names","AIC", "pR2")
outputs$names <- c("sex", "age", "IMD", "sex + age", "sex + IMD","age + IMD",
                   "sex + age + IMD", "sex + age + IMD + age^2", "sex + age + age^2 + age*sex","all")
outputs$AIC <- c(nb_s$aic, nb_a$aic, nb_d$aic, nb_sa$aic, nb_sd$aic, nb_ad$aic, 
                 nb_asd$aic, nb_asda2$aic, nb_asa2i$aic, nb_all$aic)
outputs$pR2 <- c(pR2(nb_s)["r2CU"], pR2(nb_a)["r2CU"], pR2(nb_d)["r2CU"], pR2(nb_sa)["r2CU"], pR2(nb_sd)["r2CU"], pR2(nb_ad)["r2CU"], 
                 pR2(nb_asd)["r2CU"], pR2(nb_asda2)["r2CU"], pR2(nb_asa2i)["r2CU"], pR2(nb_all)["r2CU"])
outputs$abx <- "All"

outputs <- outputs %>% arrange((AIC))

outputs %>% arrange(desc(pR2))

write_csv(outputs,"output/NB_all_antibiotics.csv")

###### plot
# Get predicted values from the negative binomial model
predicted_values <- predict(nb_all, type = "response")
# Create a data frame with actual and predicted values
plot_data <- data.frame(
  Actual = data_foi$TOTAL_ITEMS,
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
ggsave("plots/nb_all.pdf")
