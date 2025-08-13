# ===============================================================================
# LSE CAPSTONE: HOUSEHOLD STRUCTURE AND UPF CONSUMPTION IN CHILE
# ===============================================================================

# ===============================================================================
# SCRIPT 1: HOUSEHOLD DESCRIPTIVE ANALYSIS
# ===============================================================================

source("01_household_descriptives.R")

print("=== SCRIPT 1 COMPLETE: EMPIRICAL PUZZLE ESTABLISHED ===")
print("Research Question: Why do singles spend more but consume similar UPF levels?")

# ===============================================================================
# SCRIPT 2: HOUSEHOLD STATISTICAL MODELING  
# Purpose: Test hypotheses econometrically and identify mechanisms
# ===============================================================================

source("02_household_modeling.R")

print("=== SCRIPT 2 COMPLETE: HYPOTHESES TESTED ===")
print("Key Finding: Age drives UPF patterns, not household structure")
print("Mechanism: Restaurant substitution explains spending-consumption paradox")

# ===============================================================================
# SCRIPT 3: INDIVIDUAL VALIDATION ANALYSIS
# Purpose: Validate mechanisms through product-level evidence
# ===============================================================================

source("03_individual_validation.R")

print("=== SCRIPT 3 COMPLETE: MECHANISMS VALIDATED ===")
print("Key Finding: Restaurant meals classified as NOVA 3, not NOVA 4")
print("Validation: Product-level evidence confirms household-level patterns")

print("=== ALL SCRIPTS COMPLETE ===")
