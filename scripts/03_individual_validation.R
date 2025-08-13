# ===============================================================================
# SCRIPT 3: INDIVIDUAL-LEVEL VALIDATION ANALYSIS
# ===============================================================================

library(tidyverse)

# Load individual database
individual <- read.csv2('data/individual_database_final.csv', sep = ',')

# Convert key variables (fe must be numeric for weighting)
individual$expenditure_pesos <- as.numeric(individual$expenditure_pesos)
individual$fe <- as.numeric(individual$fe)
individual$hhincome <- as.numeric(individual$hhincome)
individual$total_expenditure <- as.numeric(individual$total_expenditure)
individual$single_person <- ifelse(individual$hhsize == 1, 1, 0)

# Create household type categories
individual$household_type <- case_when(
  individual$hhsize == 1 ~ "Single-person",
  individual$hhsize %in% 2:4 ~ "Small household", 
  individual$hhsize >= 5 ~ "Large household"
)

cat("=== INDIVIDUAL-LEVEL VALIDATION ANALYSIS (WEIGHTED) ===\n")
cat("Raw sample:", nrow(individual), "food purchase records\n")

# ===============================================================================
# NOVA CLASSIFICATION VARIABILITY (WEIGHTED)
# ===============================================================================

cat("=== NOVA CLASSIFICATION VARIABILITY (WEIGHTED) ===\n")

# Function to analyze NOVA distribution for specific products (MANUALLY WEIGHTED)
analyze_nova_distribution_weighted <- function(ccif_code, product_name) {
  sample_data <- individual %>% filter(ccif == ccif_code)
  raw_sample_size <- nrow(sample_data)
  
  result <- sample_data %>%
    group_by(nova_category) %>%
    summarise(
      weighted_count = sum(fe, na.rm = TRUE),
      weighted_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_count = round(weighted_count / sum(weighted_count) * 100, 1),
      pct_expenditure = round(weighted_expenditure / sum(weighted_expenditure) * 100, 1),
      product = product_name,
      raw_sample_size = raw_sample_size
    ) %>%
    select(product, nova_category, pct_expenditure, weighted_count, raw_sample_size)
  
  return(result)
}

# Representative away-from-home product classifications (WEIGHTED)
sandwiches <- analyze_nova_distribution_weighted("11.1.1.01.01", "Sandwiches/Pizzas")
meals <- analyze_nova_distribution_weighted("11.1.1.01.13", "Meals")  
main_course <- analyze_nova_distribution_weighted("11.1.1.01.11", "Main Course")

# Combine for Table 
table5_classification <- bind_rows(sandwiches, meals, main_course) %>%
  arrange(product, nova_category) %>%
  group_by(product) %>%
  mutate(raw_sample_size = first(raw_sample_size)) %>%
  ungroup()

print(table5_classification)


# ===============================================================================
# TOP UPF PRODUCTS BY HOUSEHOLD TYPE - PER CAPITA ANALYSIS (WEIGHTED)
# ===============================================================================

cat("\n=== TOP UPF PRODUCTS BY HOUSEHOLD TYPE - PER CAPITA ANALYSIS ===\n")

# Exclude "GASTOS NO DESGLOSADOS" category
exclude_category <- "GASTOS NO DESGLOSADOS EN ALIMENTOS Y BEBIDAS NO ALCOHÓLICAS"

# Calculate household counts 
household_counts <- individual %>%
  group_by(folio, household_type, hhsize, fe) %>%  # One record per household
  slice_head(n = 1) %>%  # Keep only one record per household
  group_by(household_type) %>%
  summarise(
    total_households = sum(fe, na.rm = TRUE),  # This is correct
    avg_hhsize = mean(hhsize),
    .groups = "drop"
  )
print("Household counts for per capita calculations:")
print(household_counts)

# CORRECTED: Per capita UPF expenditure by household type
upf_comparison <- individual %>%
  filter(nova_category == 4 & product_category != exclude_category) %>%
  mutate(expenditure_pc = expenditure_pesos / hhsize) %>%  # Per capita per record
  group_by(household_type, product_category) %>%
  summarise(
    total_expenditure_pc = sum(expenditure_pc * fe, na.rm = TRUE),  # Sum all per capita spending
    total_purchases = sum(fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join with correct household counts
  left_join(household_counts %>% select(household_type, total_households), by = "household_type") %>%
  mutate(
    # Average per capita monthly expenditure per household
    avg_expenditure_pc_monthly = total_expenditure_pc / total_households,
    expenditure_pc_monthly = round(avg_expenditure_pc_monthly, 0)
  ) %>%
  group_by(household_type) %>%
  arrange(desc(avg_expenditure_pc_monthly)) %>%
  slice_head(n = 10) %>%
  ungroup()

# Display results - SIMPLE WAY (NO group_walk)
cat("\nSINGLE-PERSON HOUSEHOLDS:\n")
print(upf_comparison %>% filter(household_type == "Single-person") %>% 
        select(product_category, expenditure_pc_monthly))

cat("\nSMALL HOUSEHOLDS:\n") 
print(upf_comparison %>% filter(household_type == "Small household") %>% 
        select(product_category, expenditure_pc_monthly))

cat("\nLARGE HOUSEHOLDS:\n")
print(upf_comparison %>% filter(household_type == "Large household") %>% 
        select(product_category, expenditure_pc_monthly))


# ===============================================================================
# AWAY-FROM-HOME NOVA DISTRIBUTION (WEIGHTED)
# ===============================================================================

cat("\n=== AWAY-FROM-HOME NOVA DISTRIBUTION (WEIGHTED) ===\n")

away_raw_sample <- individual %>% filter(away_from_home == 1) %>% nrow()
away_weighted_sample <- individual %>% 
  filter(away_from_home == 1) %>% 
  summarise(total = sum(fe, na.rm = TRUE)) %>% 
  pull(total)

cat("Raw sample size for away-from-home analysis:", away_raw_sample, "records\n")
cat("Weighted sample represents:", round(away_weighted_sample), "household purchases\n")

away_nova_distribution <- individual %>%
  filter(away_from_home == 1) %>%
  group_by(nova_category) %>%
  summarise(
    weighted_count = sum(fe, na.rm = TRUE),
    weighted_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_count = round(weighted_count / sum(weighted_count) * 100, 1),
    pct_expenditure = round(weighted_expenditure / sum(weighted_expenditure) * 100, 1)
  ) %>%
  arrange(nova_category)

print(away_nova_distribution)

nova3_pct <- away_nova_distribution %>% filter(nova_category == 3) %>% pull(pct_expenditure)
nova4_pct <- away_nova_distribution %>% filter(nova_category == 4) %>% pull(pct_expenditure)

if(length(nova3_pct) > 0 & length(nova4_pct) > 0) {
  cat("\n=== RESTAURANT MECHANISM EVIDENCE (POPULATION-WEIGHTED) ===\n")
  cat("Away-from-home expenditure composition:\n")
  cat("- NOVA 3 (processed):", nova3_pct, "%\n")
  cat("- NOVA 4 (ultra-processed):", nova4_pct, "%\n")
  cat("- Ratio NOVA 3:NOVA 4 =", round(nova3_pct/nova4_pct, 2), ":1\n")
}

# ===============================================================================
# CLASSIFICATION CONSISTENCY BY HOUSEHOLD TYPE (WEIGHTED)
# ===============================================================================

cat("\n=== CLASSIFICATION CONSISTENCY CHECK (WEIGHTED) ===\n")

# Test for systematic bias in restaurant meal classification (WEIGHTED)
restaurant_raw_sample <- individual %>% filter(ccif == "11.1.1.01.13") %>% nrow()
restaurant_weighted_sample <- individual %>% 
  filter(ccif == "11.1.1.01.13") %>% 
  summarise(total = sum(fe, na.rm = TRUE)) %>% 
  pull(total)

cat("Restaurant meals raw sample size:", restaurant_raw_sample, "records\n")
cat("Restaurant meals weighted sample represents:", round(restaurant_weighted_sample), "household purchases\n")

restaurant_classification_check <- individual %>%
  filter(ccif == "11.1.1.01.13") %>%  # Restaurant meals
  group_by(single_person, nova_category) %>%
  summarise(
    weighted_count = sum(fe, na.rm = TRUE),
    weighted_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(single_person) %>%
  mutate(
    pct_expenditure = round(weighted_expenditure / sum(weighted_expenditure) * 100, 1),
    household_label = ifelse(single_person == 1, "Singles", "Multi-person")
  ) %>%
  ungroup() %>%
  select(household_label, nova_category, pct_expenditure, weighted_count) %>%
  arrange(household_label, nova_category)

print(restaurant_classification_check)

# Statistical comparison (WEIGHTED)
singles_nova3 <- restaurant_classification_check %>% 
  filter(household_label == "Singles" & nova_category == 3) %>% 
  pull(pct_expenditure)
others_nova3 <- restaurant_classification_check %>% 
  filter(household_label == "Multi-person" & nova_category == 3) %>% 
  pull(pct_expenditure)

if(length(singles_nova3) > 0 & length(others_nova3) > 0) {
  difference <- round(singles_nova3 - others_nova3, 1)
  cat("\nRestaurant meals classified as NOVA 3 (WEIGHTED):\n")
  cat("- Singles:", singles_nova3, "%\n") 
  cat("- Multi-person:", others_nova3, "%\n")
  cat("- Difference:", difference, "percentage points\n")
} else {
  cat("\nInsufficient data for NOVA 3 comparison in restaurant meals\n")
}

# ===============================================================================
#  CLASSIFICATION PERFORMANCE METRICS (WEIGHTED)
# ===============================================================================

cat("\n===  BETO CLASSIFICATION PERFORMANCE (WEIGHTED) ===\n")

# Overall classification distribution in EPF data (WEIGHTED)
classification_performance <- individual %>%
  group_by(nova_category) %>%
  summarise(
    weighted_sample_size = sum(fe, na.rm = TRUE),
    weighted_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = round(weighted_sample_size / sum(weighted_sample_size) * 100, 1),
    expenditure_share = round(weighted_expenditure / sum(weighted_expenditure) * 100, 1)
  ) %>%
  arrange(nova_category)

# Add BETO validation performance (from your 6K training sample)
table15_performance <- classification_performance %>%
  mutate(
    nova_label = case_when(
      nova_category == 0 ~ "Unclassified",
      nova_category == 1 ~ "Unprocessed", 
      nova_category == 2 ~ "Processed culinary",
      nova_category == 3 ~ "Processed foods",
      nova_category == 4 ~ "Ultra-processed"
    )
  ) %>%
  select(nova_label, weighted_sample_size, percentage, expenditure_share)

print(table15_performance)

total_weighted_records <- sum(classification_performance$weighted_sample_size)

cat("\nBETO Model Performance (from 6K validation sample):\n")
cat("- Overall Accuracy: 95.44%\n") 
cat("- Weighted F1 Score: 0.9546\n")
cat("- Calibration Error: 0.0170\n")
cat("- Raw EPF records:", nrow(individual), "\n")
cat("- Weighted household representation:", round(total_weighted_records), "\n")


# ===============================================================================
# UPF BY HOUSEHOLD TYPE
# ===============================================================================

# Load required packages
library(dplyr)
library(survey)

# Step 1: Pre-aggregate the data to reduce size
agg_data <- individual %>%
  group_by(household_type, nova_category, cluster, strata) %>%
  summarise(
    weighted_sample_size = sum(fe, na.rm = TRUE),
    weighted_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Define the survey design on aggregated data
# If upm or strata are unavailable, omit them for faster computation
svy_design <- svydesign(
  ids = ~cluster,              # Clustering variable
  strata = ~strata,        # Stratification variable
  weights = ~weighted_sample_size,  # Use aggregated weights
  data = agg_data
)

# Step 3: Calculate totals for sample size and expenditure
totals <- svyby(
  ~weighted_sample_size + weighted_expenditure,
  by = ~household_type + nova_category,
  design = svy_design,
  FUN = svytotal,
  vartype = c("se", "ci"),
  na.rm = TRUE
)

# Step 4: Compute percentages and expenditure shares with CIs
table2_nova_household <- totals %>%
  group_by(household_type) %>%
  mutate(
    percentage = round(weighted_sample_size / sum(weighted_sample_size) * 100, 1),
    se_percentage = round(se.weighted_sample_size / sum(weighted_sample_size) * 100, 1),
    ci_l_percentage = round(ci_l.weighted_sample_size / sum(weighted_sample_size) * 100, 1),
    ci_u_percentage = round(ci_u.weighted_sample_size / sum(weighted_sample_size) * 100, 1),
    expenditure_share = round(weighted_expenditure / sum(weighted_expenditure) * 100, 1),
    se_share = round(se.weighted_expenditure / sum(weighted_expenditure) * 100, 1),
    ci_l_share = round(ci_l.weighted_expenditure / sum(weighted_expenditure) * 100, 1),
    ci_u_share = round(ci_u.weighted_expenditure / sum(weighted_expenditure) * 100, 1)
  ) %>%
  ungroup() %>%
  # Add NOVA labels
  mutate(
    nova_label = case_when(
      nova_category == 0 ~ "Unclassified",
      nova_category == 1 ~ "Unprocessed",
      nova_category == 2 ~ "Processed culinary",
      nova_category == 3 ~ "Processed foods",
      nova_category == 4 ~ "Ultra-processed"
    )
  ) %>%
  # Select and reorder columns
  select(
    household_type,
    nova_label,
    weighted_sample_size,
    percentage,
    se_percentage,
    ci_l_percentage,
    ci_u_percentage,
    weighted_expenditure,
    expenditure_share,
    se_share,
    ci_l_share,
    ci_u_share
  ) %>%
  arrange(household_type, nova_label)

# Step 5: Print the table
print(table2_nova_household)


# ===============================================================================
# TOP UPF PRODUCTS AWAY-FROM-HOME BY HOUSEHOLD TYPE
# ===============================================================================

cat("\n===  TOP UPF PRODUCTS AWAY-FROM-HOME BY HOUSEHOLD TYPE ===\n")

# Top 5 away-from-home UPF products - Singles
cat("SINGLES - Top 5 Away-from-Home UPF Products (by weighted expenditure):\n")
away_upf_singles <- individual %>%
  filter(nova_category == 4 & single_person == 1 & away_from_home == 1) %>%
  group_by(ccif, product_category) %>%
  summarise(
    total_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    weighted_count = sum(fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_expenditure)) %>%
  head(5) %>%
  mutate(
    expenditure_millions = round(total_expenditure / 1000000, 1),
    household_type = "Singles"
  )

print(away_upf_singles[c("product_category", "expenditure_millions", "weighted_count")])

# Top 5 away-from-home UPF products - Multi-person
cat("\nMULTI-PERSON - Top 5 Away-from-Home UPF Products (by weighted expenditure):\n")
away_upf_others <- individual %>%
  filter(nova_category == 4 & single_person == 0 & away_from_home == 1) %>%
  group_by(ccif, product_category) %>%
  summarise(
    total_expenditure = sum(expenditure_pesos * fe, na.rm = TRUE),
    weighted_count = sum(fe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_expenditure)) %>%
  head(5) %>%
  mutate(
    expenditure_millions = round(total_expenditure / 1000000, 1),
    household_type = "Multi-person"
  )

print(away_upf_others[c("product_category", "expenditure_millions", "weighted_count")])

# Combined table for export
table7_away_upf_products <- bind_rows(
  away_upf_singles %>% select(household_type, product_category, expenditure_millions, weighted_count),
  away_upf_others %>% select(household_type, product_category, expenditure_millions, weighted_count)
)

cat("\n=== AWAY-FROM-HOME UPF PORTFOLIO COMPARISON ===\n")
away_singles_total <- sum(away_upf_singles$total_expenditure) / 1000000
away_others_total <- sum(away_upf_others$total_expenditure) / 1000000
away_volume_ratio <- round(away_others_total / away_singles_total, 1)
cat("Total away-from-home UPF expenditure in top 5 products:\n")
cat("- Singles:", round(away_singles_total, 1), "million pesos\n")
cat("- Multi-person:", round(away_others_total, 1), "million pesos\n") 
cat("- Away-from-home volume ratio:", away_volume_ratio, ":1 (multi-person vs singles)\n")


