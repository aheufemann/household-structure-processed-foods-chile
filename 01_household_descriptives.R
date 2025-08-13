# ===============================================================================
# SCRIPT 1: HOUSEHOLD DESCRIPTIVE ANALYSIS
# ===============================================================================

library(tidyverse)
library(srvyr)
library(survey)

# Load data
households <- read.csv2('data/household_database_expenditure_final.csv', sep = ',')

# Convert variables
households$fe <- as.numeric(households$fe)
households$hhincome <- as.numeric(households$hhincome)
households$upf_expenditure_pc <- as.numeric(households$upf_expenditure_pc)  # ADD THIS LINE
households$total_food_expenditure_pc <- as.numeric(households$total_food_expenditure_pc)
households$at_home_expenditure_pc <- as.numeric(households$at_home_expenditure_pc)
households$away_expenditure_pc <- as.numeric(households$away_expenditure_pc)
households$processing_intensity <- as.numeric(households$processing_intensity)
households$upf_efficiency <- as.numeric(households$upf_efficiency)

# Create UPF share correctly
households$upf_expenditure_share <- households$upf_expenditure_pc / households$total_food_expenditure_pc

# Create analysis variables
households$household_type <- factor(households$household_type, 
                                    levels = c("Small household", "Single-person", "Large household"))
households$single_person <- ifelse(households$hhsize == 1, 1, 0)
households$female_head <- ifelse(households$head_gender == 2, 1, 0)
households$quintil <- factor(households$quintil, levels = c("I", "II", "III", "IV", "V"), ordered = TRUE)

# Survey design
household_survey <- households %>%
  filter(!is.na(quintil)) %>%
  as_survey_design(ids = cluster, strata = strata, weights = fe, nest = FALSE)

cat("=== HOUSEHOLD DESCRIPTIVE ANALYSIS ===\n")
cat("Sample:", nrow(households), "households\n")

# ===============================================================================
# SAMPLE CHARACTERISTICS BY HOUSEHOLD TYPE
# ===============================================================================

cat("\n=== SAMPLE CHARACTERISTICS ===\n")

table1_characteristics <- household_survey %>%
  group_by(household_type) %>%
  summarise(
    n_weighted = survey_total(),
    percent = survey_mean() * 100,
    avg_hhsize = survey_mean(hhsize),
    avg_head_age = survey_mean(head_age),
    pct_female_head = survey_mean(female_head) * 100,
    avg_income = survey_mean(hhincome),
    total_food_pc = survey_mean(total_food_expenditure_pc),
    .groups = "drop"
  )

print(table1_characteristics)

# ===============================================================================
# FOOD EXPENDITURE PATTERNS (THE SPENDING PARADOX)
# ===============================================================================

cat("\n===  FOOD EXPENDITURE PATTERNS ===\n")

table2_expenditure <- household_survey %>%
  group_by(household_type) %>%
  summarise(
    total_food_pc = survey_mean(total_food_expenditure_pc, na.rm = TRUE),
    at_home_pc = survey_mean(at_home_expenditure_pc, na.rm = TRUE),
    away_pc = survey_mean(away_expenditure_pc, na.rm = TRUE),
    upf_share = survey_mean(upf_expenditure_share, na.rm = TRUE) * 100,  
    upf_exp_pc = survey_mean(upf_expenditure_pc, na.rm = TRUE),
    processing_intensity = survey_mean(processing_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    away_share = away_pc / (at_home_pc + away_pc) * 100
  )

print(table2_expenditure)


cat("\n=== SCRIPT 1 COMPLETE ===\n")
cat("Next step: Run 02_household_modeling.R to test hypotheses\n")