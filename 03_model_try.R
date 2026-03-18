# #####################################
# model building and comparison  script             #
# Author: Gwen Knight            #
# Date: Mon March 16 2026               #
# #####################################
library(glmmTMB)
library(broom.mixed)
library(tidyverse)
library(AICcmodavg)  # for clean AIC comparison table

# Prepare modelling data
# Aggregate to the level needed: total items and population by LAD, IMD quintile, age, sex
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
  gender   = relevel(factor(year),   ref = "2019")
)

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

# 4. + IMD + sex
m4 <- glmmTMB(
  total_items ~ year + imd_quintile + gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 5. + IMD + age + sex + age*sex
m5 <- glmmTMB(
  total_items ~ year + imd_quintile + age_band * gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# 6. + IMD + age + sex + age*sex + IMD*age + IMD*sex
m6 <- glmmTMB(
  total_items ~ year + imd_quintile * age_band + imd_quintile * gender + offset(log(pop)) + (1 | lad_code),
  family = nbinom2, data = model_data
)

# AIC comparison table
aic_table <- aictab(
  cand.set  = list(m0, m1, m2, m3, m4, m5, m6),
  modnames  = c("null minus year", "null", "IMD", "IMD+age", "IMD+sex", "IMD+age+sex+age:sex", "IMD+age+sex+IMD:age+IMD:sex")
)

print(aic_table)

# Extract coefficients from all models
# if do m6 then get a lot of imd:age coefficients - remove for simpler plots 
coef_table <- bind_rows(
  tidy(m0, effects = "fixed", conf.int = TRUE) |> mutate(model = "0: null-year"),
  tidy(m1, effects = "fixed", conf.int = TRUE) |> mutate(model = "1: null"),
  tidy(m2, effects = "fixed", conf.int = TRUE) |> mutate(model = "2: IMD"),
  tidy(m3, effects = "fixed", conf.int = TRUE) |> mutate(model = "3: IMD+age"),
  tidy(m4, effects = "fixed", conf.int = TRUE) |> mutate(model = "4: IMD+sex"),
  tidy(m5, effects = "fixed", conf.int = TRUE) |> mutate(model = "5: IMD+age+sex+age:sex"),
 # tidy(m6, effects = "fixed", conf.int = TRUE) |> mutate(model = "6: IMD+age+sex+IMD:age+IMD:sex")
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
    title = "IMD coefficients across models",
    subtitle = "Shows how IMD effect changes after adjusting for age and sex"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# IMD has no real different effect across the models 
# ? less of an effect with more complex models? (for Q4&5 at least)

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
    subtitle = "Shows how YEAR effect changes after adjusting for age and sex"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# IMD has no real different effect across the models 
# ? less of an effect with more complex models? (for Q4&5 at least)



# Compare magnitude of all predictors in m5
coef_table |>
  filter(model == "5: IMD+age+sex+age:sex") |>
  ggplot(aes(x = reorder(term, irr), y = irr, ymin = irr_low, ymax = irr_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Incidence rate ratio",
    title = "All coefficients from model 5",
    subtitle = "IMD vs age vs sex effects on antibiotic prescribing"
  ) +
  theme_minimal()

