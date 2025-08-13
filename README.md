# Household Structure and Ultra-Processed Food Consumption in Chile

LSE MSc Capstone Project analyzing the relationship between household structure and consumption of ultra-processed foods using Chile's 2022 Household Budget Survey (EPF).

## Project Overview

This project uses BETO (Spanish BERT model) to classify 900,000 food descriptions into NOVA categories, then applies statistical models to examine consumption patterns by household type. The research addresses the gap between Chile's changing household composition (21.8% one-person households) and food processing consumption patterns using nationally representative data.

## Code Structure

### Notebooks
- `01_sample_selection.ipynb` - Sample selection of 5000 cases
- `02_add_1000_cases.ipynb` - Add 1000 additional cases  
- `03_training_dataset_creation.ipynb` - Creates training_dataset_6000.csv
- `04_capstone_training_beto_nova.ipynb` - BETO model fine-tuning
- `05_capstone_production_pipeline.ipynb` - Production classification pipeline with reproducibility controls

### R Analysis
- `00_master.R` - Master script
- `01_household_descriptives.R` - Descriptive analysis
- `02_household_modeling.R` - Statistical modeling 
- `03_individual_validation.R` - Individual-level validation

## Methodology

**Data Processing**: Over 900,000 raw food descriptions from EPF 2022 processed using semantic classification based on NOVA framework (unprocessed, minimally processed, processed culinary ingredients, processed foods, ultra-processed foods).

**Model**: Fine-tuned BETO (Spanish-language BERT) for automated NOVA classification with manual validation of uncertain cases.

**Analysis**: Construction of household-level indicators (ultra-processed food share, NOVA diversity scores) merged with demographic variables for statistical modeling.

**Reproducibility**: All model predictions use fixed seeds and deterministic processing to ensure identical results across runs.
