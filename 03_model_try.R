# #####################################
# model building and comparison  script             #
# Author: Gwen Knight            #
# Date: Mon March 16 2026               #
# #####################################
library(glmmTMB)
library(broom.mixed)
library(tidyverse)
library(marginaleffects)
library(AICcmodavg)  # for clean AIC comparison table

### Baseline use 
### LAD denominator linkage
link = "lad" #"lsoa"

if(link == "lad"){combined_data <- read_csv("data/combined_data.csv")}
if(link == "lsoa"){
  ### Could use LSOA data linkage
  combined_data <- read_csv("data/combined_data_lsoa.csv") %>%
    rename(pop_a_s = population)
}

# Prepare modelling data
# Aggregate to the level needed: total items and population by LAD, IMD quintile, age, gender
model_data <- combined_data |>
  filter(!is.na(imd_quintile), !is.na(pop_a_s), !is.na(lad_code)) |>
  mutate(
    imd_quintile = factor(imd_quintile),
    age_band     = factor(age_band),
    gender       = factor(gender), 
    year         = factor(year)
  ) |>
  group_by(lad_code, imd_quintile, age_band, gender, year) |>
  summarise(
    total_items = sum(total_items, na.rm = TRUE),
    pop         = sum(pop_a_s,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(pop > 0)

## Make refernece men middle age
model_data <- model_data |>
mutate(
  age_band = relevel(factor(age_band), ref = "31-40"),
  gender   = relevel(factor(gender),   ref = "male"),
  year   = relevel(factor(year),   ref = "2019")
)

########## RUNNING THE MODELS ##########
# 0. Null: offset + random LAD only - no year fixed effect
m0 <- glmmTMB(
  total_items ~ offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 1. Null: offset + random LAD only
m1 <- glmmTMB(
  total_items ~ year + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 2. + IMD quintile
m2 <- glmmTMB(
  total_items ~ year + imd_quintile + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 3. + IMD + age
m3 <- glmmTMB(
  total_items ~ year + imd_quintile + age_band + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 4. + IMD + gender
m4 <- glmmTMB(
  total_items ~ year + imd_quintile + gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 5. + IMD + age + gender 
m5 <- glmmTMB(
  total_items ~ year + imd_quintile + age_band + gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 6. + IMD + age + gender + age*gender 
m6 <- glmmTMB(
  total_items ~ year + imd_quintile + age_band + gender + age_band*gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 7. + IMD + age + gender + age*gender + IMD*age + IMD*gender
m7 <- glmmTMB(
  total_items ~ year + imd_quintile + age_band + gender + age_band*gender + 
    imd_quintile * age_band + imd_quintile * gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 8.  + age + gender + age*gender (NO IMD)
m8 <- glmmTMB(
  total_items ~ year + age_band + gender + age_band*gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)



# AIC comparison table
aic_table <- aictab(
  cand.set  = list(m0, m1, m2, m3, m4, m5, m6, m7, m8),
  modnames  = c("null minus year", "null", "IMD", "IMD+age", "IMD+gender", 
                "IMD+age+gender","IMD+age+gender+age:gender", "IMD+age+gender+age:gender+IMD:age+IMD:gender",
                "age+gender+age:gender")
)

print(aic_table)
# Save AIC table
write.csv(aic_table, paste0("aic_table_",link,".csv"))

# Extract coefficients from all models
# if do m7 then get a lot of imd:age coefficients - remove for simpler plots 
coef_table <- bind_rows(
  tidy(m0, effects = "fixed", conf.int = TRUE) |> mutate(model = "0: null-year"),
  tidy(m1, effects = "fixed", conf.int = TRUE) |> mutate(model = "1: null"),
  tidy(m2, effects = "fixed", conf.int = TRUE) |> mutate(model = "2: IMD"),
  tidy(m3, effects = "fixed", conf.int = TRUE) |> mutate(model = "3: IMD+age"),
  tidy(m4, effects = "fixed", conf.int = TRUE) |> mutate(model = "4: IMD+gender"),
  tidy(m5, effects = "fixed", conf.int = TRUE) |> mutate(model = "5: IMD+age+gender"),
  tidy(m6, effects = "fixed", conf.int = TRUE) |> mutate(model = "6: IMD+age+gender+age:gender"),
 # tidy(m7, effects = "fixed", conf.int = TRUE) |> mutate(model = "7: IMD+age+gender+IMD:age+IMD:gender"),
 tidy(m8, effects = "fixed", conf.int = TRUE) |> mutate(model = "8: age+gender+age:gender"),
) |>
  filter(term != "(Intercept)") |>
  mutate(
    irr      = exp(estimate),
    irr_low  = exp(conf.low),
    irr_high = exp(conf.high)
  )

# Plot IMD coefficients only across models to see how they shift
coef_table |>
  filter(str_detect(term, "imd_quintile")) |>
  ggplot(aes(x = term, y = irr, ymin = irr_low, ymax = irr_high, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "IMD quintile (reference = Q1, most deprived)",
    y = "Incidence rate ratio",
    colour = NULL,
    title = paste0("IMD coefficients across models by model using ", link, " linkage"),
    subtitle = "Shows how IMD effect changes after adjusting for age and gender"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# IMD has no real different effect across the models 
# ? less of an effect with more complex models? (for Q4&5 at least)
ggsave(paste0("IMD_across_models_by_model_",link,".pdf"))

coef_table |>
  filter(str_detect(term, "imd_quintile")) |>
  ggplot(aes(x = model, y = irr, ymin = irr_low, ymax = irr_high, colour = term)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "IMD quintile (reference = Q1, most deprived)",
    y = "Incidence rate ratio",
    colour = NULL,
    title = paste0("IMD coefficients across models by IMD using ", link, " linkage"),
    subtitle = "Shows how IMD effect changes after adjusting for age and gender"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
### q4 and q5 have a bigger difference / effect but otherwise model effect is small? 
ggsave(paste0("IMD_across_models_by_IMD_",link,".pdf"))



# Plot year coefficients only across models to see how they shift
coef_table |>
  filter(str_detect(term, "year")) |>
  ggplot(aes(x = term, y = irr, ymin = irr_low, ymax = irr_high, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "Year (reference = 2019)",
    y = "Incidence rate ratio",
    colour = NULL,
    title = "Year coefficients across models",
    subtitle = "Shows how YEAR effect changes after adjusting for age and gender"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# IMD has no real different effect across the models 
# ? less of an effect with more complex models? (for Q4&5 at least)
ggsave(paste0("Year_across_models_",link,".pdf"))


# Compare magnitude of all predictors in m6
coef_table |>
  filter(model == "6: IMD+age+gender+age:gender") |>
  ggplot(aes(x = reorder(term, irr), y = irr, ymin = irr_low, ymax = irr_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(lim = c(0,5.5)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Incidence rate ratio",
    title = "All coefficients from model 5",
    subtitle = "IMD vs age vs gender effects on antibiotic prescribing"
  ) +
  theme_minimal()
ggsave(paste0("Predictors_m6_",link,".pdf"))

##### EXPLORATIONS ##########
### IMD OR AGE IMPORTANT?
# ============================================================
# 1. AIC / DEVIANCE VARIANCE PARTITIONING BAR CHART
# ============================================================

# Fit age-only and IMD-only models for comparison
m_age_only <- glmmTMB(
  total_items ~ year + age_band + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)
m_imd_only <- glmmTMB(
  total_items ~ year + imd_quintile + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

m_gender_only <- glmmTMB(
  total_items ~ year + gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

m_all_three <- m5

# Extract log-likelihoods and compute deviance explained relative to null (m1)
ll_null    <- logLik(m1)
ll_age     <- logLik(m_age_only)
ll_imd     <- logLik(m_imd_only)
ll_both    <- logLik(m5)
ll_gender  <- logLik(m_gender_only)
ll_all     <- logLik(m_all_three)
dev_gender_only <- 2 * (as.numeric(ll_gender) - as.numeric(ll_null))
dev_all         <- 2 * (as.numeric(ll_all)    - as.numeric(ll_null))
dev_age_only  <- 2 * (as.numeric(ll_age)  - as.numeric(ll_null))
dev_imd_only  <- 2 * (as.numeric(ll_imd)  - as.numeric(ll_null))
dev_both      <- 2 * (as.numeric(ll_both) - as.numeric(ll_null))

# Unique contributions (each vs all other two)
unique_age    <- dev_all - 2*(as.numeric(logLik(glmmTMB(total_items ~ year + imd_quintile + gender   + offset(log(pop)) + (1|lad_code), family=nbinom2, data=model_data))) - as.numeric(ll_null))
unique_imd    <- dev_all - 2*(as.numeric(logLik(glmmTMB(total_items ~ year + age_band     + gender   + offset(log(pop)) + (1|lad_code), family=nbinom2, data=model_data))) - as.numeric(ll_null))
unique_gender <- dev_all - 2*(as.numeric(logLik(glmmTMB(total_items ~ year + imd_quintile + age_band + offset(log(pop)) + (1|lad_code), family=nbinom2, data=model_data))) - as.numeric(ll_null))
shared_all    <- dev_all - unique_age - unique_imd - unique_gender

### How to summarise? 
cat("Age explains", round(unique_age / unique_imd, 1), 
    "times more unique variance than IMD\n")
cat("Gender explains", round(unique_gender / unique_imd, 1), 
    "times more unique variance than IMD\n")

## Visuliase
vp_df <- data.frame(
  component = c("Age (unique)", "IMD (unique)", "Shared"),
  deviance  = c(unique_age, unique_imd, shared)
) %>%
  mutate(
    pct = deviance / dev_both * 100,
    component = factor(component, levels = c("Age (unique)", "Shared", "IMD (unique)"))
  )

ggplot(vp_df, aes(x = "", y = pct, fill = component)) +
  geom_col(width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = NULL,
    y = "% of explained deviance",
    fill = NULL,
    title = "Variance partitioning: Age vs IMD",
    subtitle = "Relative contribution to model fit over null (year + random LAD)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

vp_df3 <- data.frame(
  component = c("Age (unique)", "IMD (unique)", "Gender (unique)", "Shared"),
  deviance  = c(unique_age, unique_imd, unique_gender, shared_all)
) %>%
  mutate(
    pct = deviance / dev_all * 100,
    component = factor(component, levels = c("Age (unique)", "IMD (unique)", 
                                             "Gender (unique)", "Shared"))
  )

ggplot(vp_df3, aes(x = "", y = pct, fill = component)) +
  geom_col(width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(
    x = NULL,
    y = "% of explained deviance",
    fill = NULL,
    title = "Variance partitioning: Age vs IMD vs Gender",
    subtitle = "Relative contribution to model fit over null (year + random LAD)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

vp_df3 %>%
  filter(component != "Shared") %>%
  ggplot(aes(x = reorder(component, pct), y = pct, fill = component)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, max(vp_df3$pct) * 1.2),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    x = NULL, 
    y = "% of total explained deviance (unique contribution)",
    title = "Unique contributions to model fit",
    subtitle = "Excluding shared variance — how much does each predictor\nadd that the others cannot explain?",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# How much does age add over IMD alone?
lrt_age_over_imd <- anova(m_imd_only, m_all_three)

# How much does IMD add over age alone?
lrt_imd_over_age <- anova(m_age_only, m_all_three)

# How much does gender add over age+IMD?
lrt_gender <- anova(m_all_three, m5)  # m5 has age+IMD+gender+age:gender

print(lrt_age_over_imd)
print(lrt_imd_over_age)
print(lrt_gender)

### USING LAD 
### Age was the dominant predictor of prescribing rates (LRT χ²=82,304, df=10), 
### explaining approximately three times more variance than deprivation (LRT χ²=27,828, df=5), 
### though both were highly significant (p<0.001). 
### The interaction between age and gender also made a substantial contribution 
### (χ²=24,444, df=9), indicating that age-related prescribing patterns differ by sex


# ============================================================
# 2. MARGINAL EFFECTS: predicted rates by IMD x age band
# ============================================================

# Predict from m5 across IMD quintile and age band combinations
marg <- predictions(
  m5,
  newdata = datagrid(
    imd_quintile = levels(model_data$imd_quintile),
    age_band     = levels(model_data$age_band),
    gender       = "male",      # hold gender constant at reference
    year         = "2019",      # hold year constant at reference
    pop          = 1000         # per 1000 population
  )
)

as.data.frame(marg) %>%
  mutate(age_band = factor(age_band, levels = c("0-5","6-10","11-20",
                                                "21-30","31-40","41-50",
                                                "51-60","61-70","71-80",
                                                "81-90","91-100"))) %>%
  ggplot(aes(x = imd_quintile, y = estimate, 
             ymin = conf.low, ymax = conf.high,
             colour = age_band, group = age_band)) +
  geom_line() +
  geom_pointrange(position = position_dodge(width = 0.2)) +
  scale_colour_viridis_d(option = "C") +
  labs(
    x     = "IMD quintile (Q1 = most deprived)",
    y     = "Predicted items per 1,000 population",
    colour = "Age band",
    title  = "Marginal effects: predicted rate by IMD and age band",
    subtitle = "Males, year = 2019, per 1,000 population"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

## age matters much more than where you live (IMD) for prescribing rates, 
## but deprivation still has a small, consistent independent effect across all age groups.


# ============================================================
# 3. HEATMAP of predicted rates: age band x IMD quintile
# ============================================================

as.data.frame(marg) %>%
  mutate(age_band = factor(age_band, levels = c("0-5","6-10","11-20",
                                                "21-30","31-40","41-50",
                                                "51-60","61-70","71-80",
                                                "81-90","91-100"))) %>%
  ggplot(aes(x = imd_quintile, y = age_band, fill = estimate)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(estimate, 1)), size = 3, colour = "white") +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(
    x    = "IMD quintile (Q1 = most deprived)",
    y    = "Age band",
    fill = "Predicted\nitems per 1,000",
    title    = "Heatmap of predicted prescribing rates",
    subtitle = "Males, year = 2019 — darker = higher rate"
  ) +
  theme_minimal()
