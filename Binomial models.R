##### Run negative binomial model 

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
library(performance)
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
foidata %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "Age 0-5")
data_foi %>% filter(PRACTICE_CODE == "A81001", GENDER == "Female", AGE_BAND == "0-5")

#######################################################################################################
### data_foi CONTAINS all ITEMS with redacted to 1, or ITEMS4 with redacted to 4, population of GP ####
### IMD (WHICH IS THE IMD2019 VALUE FOR THE CORRESPONDING GP) FIVE-YEAR AGE BANDS AND SEX #############
##### Fit the negative binomial model with the offset manually included ###############################
#######################################################################################################

# FULL model with age-sex interaction term and age^2
negative_binomial_model<-glm.nb(TOTAL_ITEMS ~  AGE_BAND_NUM + AGE_BAND_NUM2 + IMD +
                                  GENDER + AGE_BAND_NUM:GENDER + 
                                  offset(log(TOTAL_PATIENTS)), 
                                data = data_foi)
##############################
# Summary of the model
summary(negative_binomial_model)

summ(negative_binomial_model, exp = T) 

##########################################################
### PSEUDO R SQUARED #####################################
##########################################################

pR2(negative_binomial_model) # need to check this!
PseudoR2(negative_binomial_model, c("McFadden", "Nagel","Tjur"))
PseudoR2(negative_binomial_model, c("AIC"))
r2_tjur(negative_binomial_model)
#nagelkerke()

#################################################################
### THE PLOTS BELOW ARE WAYS TO VISUALISE THE FIT OF THE MODEL###
#################################################################
#######################################
### PREDICTION MODEL ##################
#######################################
# Get predicted values from the negative binomial model
predicted_values <- predict(negative_binomial_model, type = "response")
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



################################################
### FIT ########################################
################################################
deviance_fitted <- deviance(negative_binomial_model)
null_model <- glm.nb(TOTAL_ITEMS ~ 1 + offset(log(TOTAL_PATIENTS)), data = data_foi)
deviance_null <- deviance(null_model)
R2_dev <- 1 - (deviance_fitted / deviance_null)
print(R2_dev)


################################################
### FREQUENCY DISTRIBUTION PLOT TOTAL_ITEMS ####
################################################

# Plot the distribution of TOTAL_ITEMS
ggplot(data_foi, aes(x = TOTAL_ITEMS)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # Histogram with bin width of 1
  labs(title = "Distribution of TOTAL_ITEMS",
       x = "TOTAL_ITEMS",
       y = "Count") +
  theme_minimal() +
  xlim(0, 1000)  # Limit the x-axis to 2000

################################################
### RESIDUAL PLOTS #############################
################################################
residuals_nb <- residuals(negative_binomial_model, type = "pearson")
fitted_nb <- fitted(negative_binomial_model)

ggplot(data.frame(Fitted = fitted_nb, Residuals = residuals_nb), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

### Q-Q plot

qqnorm(residuals_nb, main = "Q-Q Plot of Residuals")
qqline(residuals_nb, col = "red")

###

sqrt_abs_residuals <- sqrt(abs(residuals_nb))

ggplot(data.frame(Fitted = fitted_nb, SqrtAbsResiduals = sqrt_abs_residuals), 
       aes(x = Fitted, y = SqrtAbsResiduals)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "√|Residuals|") +
  theme_minimal()

###

ggplot(data.frame(Predictor = as.factor(data_foi$AGE_BAND_NUM), Residuals = residuals_nb), aes(x = Predictor, y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predictor: AGE_BAND_NUM", x = "AGE_BAND_NUM", y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggplot(data.frame(Predictor = as.factor(data_foi$AGE_BAND_NUM2), Residuals = residuals_nb), aes(x = Predictor, y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predictor: AGE_BAND_NUM", x = "AGE_BAND_NUM", y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


ggplot(data.frame(Predictor = data_foi$IMD, Residuals = residuals_nb), aes(x = Predictor, y = Residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predictor: deprivation", x = "Value", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Predictor = data_foi$GENDER, Residuals = residuals_nb), aes(x = Predictor, y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predictor: Gender", x = "GENDER", y = "Residuals") +
  theme_minimal()
###############################
### INTERACTION AGE AND SEX ###
###############################
ggplot(data_foi, aes(x = AGE_BAND_NUM, y = TOTAL_ITEMS)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  facet_wrap(~GENDER) +
  labs(title = "Effect of Age on TOTAL_ITEMS by Gender",
       x = "AGE_BAND_NUM",
       y = "TOTAL_ITEMS") +
  theme_minimal()


