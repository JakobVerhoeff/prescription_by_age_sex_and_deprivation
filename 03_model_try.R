# #####################################
# model building and comparison script #
# Author: Gwen Knight                  #
# Date: Mon March 16 2026              #
# Updated: Mon Jun 09 2026             #
# Runs same nested model set for all   #
# antibiotics combined + each family   #
# #####################################
library(glmmTMB)
library(broom.mixed)
library(tidyverse)
library(marginaleffects)
library(AICcmodavg)

rm(list = ls())

### Baseline use
### LAD denominator linkage
link <- "lad" # "lsoa" or "lad"

if (link == "lad") { combined_data <- read_csv("data/combined_data.csv") }
if (link == "lsoa") {
  combined_data <- read_csv("data/combined_data_lsoa.csv") %>%
    rename(pop_a_s = population)
}

# =========================================================================
# 1. Prepare modelling data  ---------------------------------------------
# =========================================================================
# "All" = aggregate across all BNF codes (as before)
# Per-family = aggregate within each antibiotic_group
base_data <- combined_data |>
  filter(!is.na(imd_quintile), !is.na(pop_a_s), !is.na(lad_code)) |>
  mutate(
    imd_quintile = factor(imd_quintile),
    age_band     = factor(age_band),
    gender       = factor(gender),
    year         = factor(year)
  )

# Population denominators (independent of antibiotic group)
pop_data <- base_data |>
  distinct(lad_code, imd_quintile, age_band, gender, year, pop_a_s)

# Build model dataset for a given family (or "All")
build_model_data <- function(data, pop, family = "All") {
  if (family == "All") {
    items <- data |>
      group_by(lad_code, imd_quintile, age_band, gender, year) |>
      summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop")
  } else {
    items <- data |>
      filter(antibiotic_group == family) |>
      group_by(lad_code, imd_quintile, age_band, gender, year) |>
      summarise(total_items = sum(total_items, na.rm = TRUE), .groups = "drop")
  }

  items |>
    left_join(pop |>
      group_by(lad_code, imd_quintile, age_band, gender, year) |>
      summarise(pop = sum(pop_a_s, na.rm = TRUE), .groups = "drop"),
      by = c("lad_code", "imd_quintile", "age_band", "gender", "year")
    ) |>
    filter(pop > 0) |>
    mutate(
      age_band = relevel(factor(age_band), ref = "31-40"),
      gender   = relevel(factor(gender),   ref = "man"),
      year     = relevel(factor(year),     ref = "2019")
    )
}

# =========================================================================
# 2. Model fitting function  ---------------------------------------------
# =========================================================================
ctrl <- glmmTMBControl(optCtrl = list(iter.max = 1000, eval.max = 1000))

fit_models <- function(md) {
  list(
    "0: null-year"  = glmmTMB(total_items ~ offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md),
    "1: null"       = glmmTMB(total_items ~ year + offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md),
    "2: IMD"        = glmmTMB(total_items ~ year + imd_quintile + offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md),
    "3: IMD+age"    = glmmTMB(total_items ~ year + imd_quintile + age_band + offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md),
    "4: IMD+gender" = glmmTMB(total_items ~ year + imd_quintile + gender + offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md),
    "5: IMD+age+gender" = glmmTMB(total_items ~ year + imd_quintile + age_band + gender + offset(log(pop)) + (1 | lad_code),
                                  family = nbinom2, data = md),
    "6: IMD+age+gender+age:gender" = glmmTMB(total_items ~ year + imd_quintile + age_band + gender + age_band:gender + offset(log(pop)) + (1 | lad_code),
                                             family = nbinom2, data = md, control = ctrl),
    "7: IMD+age+gender+age:gender+IMD:age+IMD:gender" = glmmTMB(total_items ~ year + imd_quintile + age_band + gender + age_band:gender +
                                                                  imd_quintile:age_band + imd_quintile:gender + offset(log(pop)) + (1 | lad_code),
                                                                family = nbinom2, data = md, control = ctrl),
    "8: age+gender+age:gender" = glmmTMB(total_items ~ year + age_band + gender + age_band:gender + offset(log(pop)) + (1 | lad_code),
                                         family = nbinom2, data = md, control = ctrl),
    "9: IMD*year"   = glmmTMB(total_items ~ year + imd_quintile + year:imd_quintile + age_band + gender + age_band:gender + offset(log(pop)) + (1 | lad_code),
                              family = nbinom2, data = md, control = ctrl)
  )
}

# =========================================================================
# 3. Extract results function  --------------------------------------------
# =========================================================================
extract_results <- function(models, family_name) {
  # AIC table
  aic <- aictab(
    cand.set = models,
    modnames  = names(models)
  )

  # Coefficients (skip m7 and m9 for cleaner plots — too many interaction terms)
  simple_models <- models[!names(models) %in% c(
    "7: IMD+age+gender+age:gender+IMD:age+IMD:gender",
    "9: IMD*year"
  )]

  coefs <- map2_dfr(simple_models, names(simple_models), ~ {
    tidy(.x, effects = "fixed", conf.int = TRUE) |> mutate(model = .y)
  }) |>
    filter(term != "(Intercept)") |>
    mutate(
      irr      = exp(estimate),
      irr_low  = exp(conf.low),
      irr_high = exp(conf.high),
      family   = family_name
    )

  list(aic = aic |> mutate(family = family_name), coefs = coefs)
}

# =========================================================================
# 4. Run for all families  ------------------------------------------------
# =========================================================================
families <- c("All", sort(unique(na.omit(base_data$antibiotic_group))))

all_aic   <- list()
all_coefs <- list()
all_models <- list()

for (fam in families) {
  cat("Fitting models for:", fam, "\n")

  md <- build_model_data(base_data, pop_data, fam)
  models <- fit_models(md)
  results <- extract_results(models, fam)

  all_aic[[fam]]   <- results$aic
  all_coefs[[fam]] <- results$coefs
  all_models[[fam]] <- models

  # Save AIC table per family
  write.csv(results$aic, paste0("output/aic_table_", fam, "_", link, ".csv"), row.names = FALSE)
}

aic_combined   <- bind_rows(all_aic)
coef_combined  <- bind_rows(all_coefs)

write.csv(aic_combined, paste0("output/aic_table_all_families_", link, ".csv"), row.names = FALSE)
write.csv(coef_combined, paste0("output/coef_table_all_families_", link, ".csv"), row.names = FALSE)

# =========================================================================
# 5. Plots: "All" antibiotics (as before)  --------------------------------
# =========================================================================
coef_all <- coef_combined |> filter(family == "All")

# IMD coefficients across models
coef_all |>
  filter(str_detect(term, "imd_quintile")) |>
  ggplot(aes(x = term, y = irr, ymin = irr_low, ymax = irr_high, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "IMD quintile (reference = Q1, most deprived)",
    y = "Incidence rate ratio",
    colour = NULL,
    title = paste0("IMD coefficients across models (All antibiotics, ", link, " linkage)")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("plots/IMD_across_models_by_model_", link, ".pdf"))

coef_all |>
  filter(str_detect(term, "imd_quintile")) |>
  ggplot(aes(x = model, y = irr, ymin = irr_low, ymax = irr_high, colour = term)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "Model",
    y = "Incidence rate ratio",
    colour = NULL,
    title = paste0("IMD coefficients across models by IMD (", link, " linkage)")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("plots/IMD_across_models_by_IMD_", link, ".pdf"))

# Year coefficients across models
coef_all |>
  filter(str_detect(term, "year")) |>
  ggplot(aes(x = term, y = irr, ymin = irr_low, ymax = irr_high, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    x = "Year (reference = 2019)",
    y = "Incidence rate ratio",
    colour = NULL,
    title = "Year coefficients across models"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0("plots/Year_across_models_", link, ".pdf"))

# All predictors from m6
coef_all |>
  filter(model == "6: IMD+age+gender+age:gender") |>
  ggplot(aes(x = reorder(term, irr), y = irr, ymin = irr_low, ymax = irr_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(lim = c(0, 5.5)) +
  coord_flip() +
  labs(x = NULL, y = "Incidence rate ratio",
       title = "All coefficients from model 6") +
  theme_minimal()
ggsave(paste0("plots/Predictors_m6_", link, ".pdf"))

# Predicted vs observed (m6, All)
md_all <- build_model_data(base_data, pop_data, "All")
md_all %>%
  mutate(predicted = fitted(all_models[["All"]][["6: IMD+age+gender+age:gender"]])) %>%
  ggplot(aes(x = predicted, y = total_items)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  labs(x = "Model predicted", y = "Observed",
       title = "Predicted vs observed (m6, All antibiotics)") +
  theme_minimal()
ggsave(paste0("plots/Predicted_vs_obs_", link, ".pdf"))

md_all %>%
  mutate(predicted = fitted(all_models[["All"]][["6: IMD+age+gender+age:gender"]]),
         residual  = total_items - predicted) %>%
  ggplot(aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  theme_minimal()
ggsave(paste0("plots/Residuals_", link, ".pdf"))

# =========================================================================
# 6. Plots: comparing IMD effect across families  -------------------------
# =========================================================================

# Forest plot: IMD IRRs from m6 across all families
coef_combined |>
  filter(model == "6: IMD+age+gender+age:gender",
         str_detect(term, "imd_quintile")) |>
  ggplot(aes(x = family, y = irr, ymin = irr_low, ymax = irr_high, colour = term)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Incidence rate ratio (ref = Q1, most deprived)",
    colour = "IMD Quintile",
    title = "IMD effect on prescribing by antibiotic family",
    subtitle = "Model 6: adjusted for age, gender, age:gender interaction, year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(paste0("plots/IMD_by_family_forest_", link, ".pdf"), width = 10, height = 8)

# LRT: does IMD matter for each family? (compare m8 no-IMD vs m6 with-IMD)
lrt_results <- map_dfr(families, function(fam) {
  m_with    <- all_models[[fam]][["6: IMD+age+gender+age:gender"]]
  m_without <- all_models[[fam]][["8: age+gender+age:gender"]]
  test <- anova(m_without, m_with)
  tibble(
    family   = fam,
    chi_sq   = test$Chisq[2],
    df       = test$`Chi Df`[2],
    p_value  = test$`Pr(>Chisq)`[2]
  )
})

print("LRT for IMD effect by antibiotic family (m8 vs m6):")
print(lrt_results |> arrange(p_value))
write.csv(lrt_results, paste0("output/lrt_imd_by_family_", link, ".csv"), row.names = FALSE)

# Relative effect sizes: age vs gender vs IMD (All antibiotics)
coef_combined |>
  filter(family == "All", model == "6: IMD+age+gender+age:gender") |>
  mutate(category = case_when(
    str_detect(term, "imd") ~ "IMD",
    str_detect(term, "age_band") & str_detect(term, "gender") ~ "Age x Gender",
    str_detect(term, "age_band") ~ "Age",
    str_detect(term, "gender") ~ "Gender",
    TRUE ~ "Year"
  )) |>
  filter(category != "Year") |>
  mutate(term_clean = str_remove_all(term, "imd_quintile|age_band|gender"),
         term_clean = str_replace(term_clean, ":", " x ")) |>
  ggplot(aes(x = reorder(term_clean, irr), y = irr,
             ymin = irr_low, ymax = irr_high, colour = category)) +
  geom_pointrange(size = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  scale_colour_manual(values = c("Age" = "#E76F51", "Gender" = "#264653",
                                  "IMD" = "#2A9D8F", "Age x Gender" = "#E9C46A")) +
  labs(x = NULL,
       y = "Incidence Rate Ratio (reference: man, age 31-40, Q1)",
       colour = "Predictor type",
       title = "Relative effect sizes: age and gender dominate over deprivation",
       subtitle = "Model 6, all antibiotics combined") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(paste0("plots/irr_comparison_age_gender_imd_", link, ".pdf"), width = 10, height = 8)

# Same plot per antibiotic family
for (fam in families) {
  p <- coef_combined |>
    filter(family == fam, model == "6: IMD+age+gender+age:gender") |>
    mutate(category = case_when(
      str_detect(term, "imd") ~ "IMD",
      str_detect(term, "age_band") & str_detect(term, "gender") ~ "Age x Gender",
      str_detect(term, "age_band") ~ "Age",
      str_detect(term, "gender") ~ "Gender",
      TRUE ~ "Year"
    )) |>
    filter(category != "Year") |>
    mutate(term_clean = str_remove_all(term, "imd_quintile|age_band|gender"),
           term_clean = str_replace(term_clean, ":", " x ")) |>
    ggplot(aes(x = reorder(term_clean, irr), y = irr,
               ymin = irr_low, ymax = irr_high, colour = category)) +
    geom_pointrange(size = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    coord_flip() +
    scale_colour_manual(values = c("Age" = "#E76F51", "Gender" = "#264653",
                                    "IMD" = "#2A9D8F", "Age x Gender" = "#E9C46A")) +
    labs(x = NULL,
         y = "Incidence Rate Ratio (reference: man, age 31-40, Q1)",
         colour = "Predictor type",
         title = paste0("Relative effect sizes: ", fam),
         subtitle = "Model 6") +
    theme_minimal() +
    theme(legend.position = "bottom")
  ggsave(paste0("plots/irr_comparison_", fam, "_", link, ".pdf"), width = 10, height = 8)
}

# AIC comparison: best model per family
best_models <- aic_combined |>
  group_by(family) |>
  slice_min(AICc, n = 1) |>
  select(family, Modnames, AICc, Delta_AICc)
print("Best model per family:")
print(best_models)
