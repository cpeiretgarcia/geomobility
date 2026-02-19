############################################################################
############################################################################
###                                                                      ###
###                             INTRODUCTION                             ###
###                                                                      ###
############################################################################
############################################################################

# This code maps the necessary variables to be extracted from the 
# NTS data for generating demographics + mobility clusters.

##################################################################
##                         DATA LOADING                         ##
##################################################################

# Load individual NTS data
nts_individual <- read.table(
  "./data/UKDA-5340-tab/tab/individual_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

nts_household <- read.table(
  "./data/UKDA-5340-tab/tab/household_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

##################################################################
##                         DATA FILTERING                       ##
##################################################################

# Filter all source tables to 2024 at point of loading
# SurveyYear is the documented year identifier in the NTS
# Using this rather than extracting from PSUID, which is an 
# undocumented implementation detail of the SQL database

nts_individual  <- nts_individual  %>% filter(SurveyYear == 2024)
nts_household   <- nts_household   %>% filter(SurveyYear == 2024)

##################################################################
##                      VARIABLE SELECTION                      ##
##################################################################

# The variable selection is based  on Wyszomierski et al. (2022), 
# who used  Census data to generate geodemographic profiles.  
# This project uses the same demographic variables.


# These variables have GOOD or EXCELLENT matches to census variables
individual_vars <- nts_individual %>%
  select(
    # === IDENTIFIERS ===
    IndividualID,      # Individual unique ID
    HouseholdID,       # Household unique ID
    PSUID,             # PSU unique ID
    PersNo,            # Person number within household
    
    # === DEMOGRAPHICS ===
    # Age groups
    Age_B01ID,         # Age of person - banded age - 21 categories
    
    # Marital status
    MaritalStat_B01ID, # Marital status
    
    # === ETHNICITY & ORIGINS (LIMITED IN NTS) ===
    # Ethnic groups
    EthGroupTS_B02ID,  # Ethnic Group - 2 categories only (White/Non-White)
        
    # === UNPAID CARE ===
    # Whether carer for others    
    Caretime_B01ID,    # Hours a week as carer
    
    # === EDUCATION ===
    # Highest level of qualification (Level 1-2, Level 3, Level 4+)
    EdAttn3_B01ID,     # Level of highest qualification
    
    # === EMPLOYMENT ===
    # Working status
    EcoStat_B02ID,     # Working status - 6 categories
    
    # === SEX ===
    Sex_B01ID          # Sex of person
                       # Not in your census list but useful for analysis
  )

# ==============================================================================
# SELECT VARIABLES FROM HOUSEHOLD TABLE
# ==============================================================================

household_vars <- nts_household %>%
  select(
    # === IDENTIFIERS ===
    HouseholdID,       # Household unique ID
    PSUID,             # PSU unique ID
    
    # === GEOGRAPHIC ===
    HHoldGOR_B02ID,    # Household Region
    
    # === HOUSEHOLD COMPOSITION ===
    HHoldStruct_B02ID, # Household structure - 6 categories
    
    HHoldNumChildren,  # Number of children in household (actual count)
    
    HHoldNumPeople,    # Total number of people in household
    
    # === HOUSING ===
    AddressType_B01ID, # Type of property
    
    Ten1_B02ID,        # Type of tenancy - 3 categories
    
    HLongAN_B01ID,     # How long lived at address
    
    # === VEHICLES ===
    NumCar,            # Number of cars only (excludes vans)
                       # Bonus variable for detailed analysis
    
    # === INCOME ===
    HHIncQDS2008_B01ID, # Household income - Year-specific income quintiles
    
    # === WEIGHTS ===
    W2,                # Weighted diary sample
                       # Use for weighted analysis
    
    W3                 # Weighted interview sample
                       # Use for weighted analysis
  )


# ==============================================================================
# MERGE INDIVIDUAL AND HOUSEHOLD DATA
# ==============================================================================

# Merge individual and household variables
nts_merged <- individual_vars %>%
  left_join(household_vars, by = c("HouseholdID", "PSUID"))

# ==============================================================================
# CREATING CENSUS-COMPARABLE CATEGORIES
# ==============================================================================

# Create age categories to reduce the number of factor levels.
nts_merged <- nts_merged |> mutate(
  # Census v02: Aged 0-4 years
  census_age_0_4 = case_when(
    Age_B01ID %in% c(1, 2, 3) ~ 1,
    Age_B01ID >= 4 & Age_B01ID <= 21 ~ 0,
    TRUE ~ NA_real_
  ),
  
  # Census v03: Aged 5-14 years
  census_age_5_14 = case_when(
    Age_B01ID %in% c(4, 5) ~ 1,
    Age_B01ID %in% c(1:3, 6:21) ~ 0,
    TRUE ~ NA_real_
  ),
  
  # Census v04: Aged 25-44 years
  census_age_25_44 = case_when(
    Age_B01ID %in% c(11, 12, 13, 14) ~ 1,
    Age_B01ID %in% c(1:10, 15:21) ~ 0,
    TRUE ~ NA_real_
  ),
  
  # Census v05: Aged 45-64 years
  census_age_45_64 = case_when(
    Age_B01ID %in% c(14, 15, 16) ~ 1,
    Age_B01ID %in% c(1:13, 17:21) ~ 0,
    TRUE ~ NA_real_
  ),
  
  # Census v06: Aged 65-84 years
  census_age_65_84 = case_when(
    Age_B01ID %in% c(17, 18, 19, 20) ~ 1,
    Age_B01ID %in% c(1:16, 21) ~ 0,
    TRUE ~ NA_real_
  ),
  
  # Census v07: Aged 85+ years
  census_age_85plus = case_when(
    Age_B01ID == 21 ~ 1,
    Age_B01ID >= 1 & Age_B01ID <= 20 ~ 0,
    TRUE ~ NA_real_
  )
)

# ==============================================================================
# VARIABLES NOT AVAILABLE IN NTS (FROM CENSUS LIST)
# ==============================================================================

# The following census variables have NO MATCH in NTS:
# - v01: Population density
# - v12-v19: Detailed ethnic groups (NTS only has 2 categories)
# - v20: English language proficiency
# - v21, v23: Religion
# - v31: Communal establishments
# - v40-v41: Occupancy rating
# - v42: SIR (Standardized Illness Ratio)
# - v51-v59: Standard Occupational Classification (SOC) major groups

# Consider dropping these from your analysis or finding proxy variables


# ==============================================================================
# SAVE SELECTED VARIABLES
# ==============================================================================

# Create directory to save the data
dir.create("./data/processed")

# Save to parquet
write_parquet(nts_merged, "./data/processed/nts_demo_variables.parquet")

# ==============================================================================
# IMPORTANT NOTES
# ==============================================================================

# 1. CHECK CATEGORY CODES: The actual numeric codes for categories (e.g., 
#    what value represents "married" in MaritalStat_B01ID) are in the 
#    response levels lookup table. You need to check these before recoding.

# 2. YEAR AVAILABILITY: Some variables were added or dropped in certain years.
#    Check the main lookup table to see which years each variable is available.
#    Variables with "0" in year columns are not available that year.

# 3. WEIGHTS: Always use the appropriate weight (W2, W3, W4, W5) when 
#    calculating statistics to ensure representative results.

# 4. HOUSEHOLD vs INDIVIDUAL: Make sure you're joining the right tables.
#    Household variables apply to all household members.
#    Individual variables are person-specific.

# 5. PARTIAL MATCHES: For variables marked as "Partial Match", carefully
#    compare the NTS categories with census categories to ensure they align
#    before combining them in analysis.