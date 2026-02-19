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
    # Census v02-v07: Age groups (4 and under, 5-14, 25-44, 45-64, 65-84, 85+)
    Age_B01ID,         # Age of person - banded age - 21 categories
                       # Use this for detailed age analysis
                       # Alternative: Age_B04ID has 9 broader categories
    
    # Census v24-v26: Marital status (never married, married/civil partnership, separated/divorced)
    MaritalStat_B01ID, # Marital status
                       # Includes all marital status categories
                       # Matches census categories well
    
    # === ETHNICITY & ORIGINS (LIMITED IN NTS) ===
    # Census v08-v11: Country of birth (PARTIAL MATCH - less detail in NTS)
    COB_B01ID,         # Country of Birth - 7 categories
                       # NTS has less geographic detail than census
                       # Categories may need aggregation
    
    # Census v12-v19: Ethnic groups (POOR MATCH - only 2 categories in NTS)
    EthGroupTS_B02ID,  # Ethnic Group - 2 categories only (White/Non-White)
                       # Much less detailed than census
                       # Use with caution for ethnicity analysis
    
    # === HOUSEHOLD & LIVING ARRANGEMENTS ===
    # Census v24-v26: Marital status covered above
    
    # Census v27-v30: Household structure (PARTIAL MATCH)
    # Note: This is in the Household table, see below
    
    # === UNPAID CARE ===
    # Census v43: Provides unpaid care
    Carer_B01ID,       # Whether carer for family or friends
                       # Good match to census variable
    
    Caretime_B01ID,    # Hours a week as carer (bonus variable)
                       # Provides additional detail not in census
    
    # === EDUCATION ===
    # Census v45-v47: Highest level of qualification (Level 1-2, Level 3, Level 4+)
    EdAttn3_B01ID,     # Level of highest qualification
                       # Good match to census categories
                       # Also available: EdAttn1_B01ID (any certificated qualifications)
                       #                 EdAttn2_B01ID (professional qualifications)
                       #                 EdAttn4_B02ID (grouped qualifications)
    
    # === EMPLOYMENT ===
    # Census v48-v49: Hours worked (Part-time/Full-time) (PARTIAL MATCH)
    # Census v60: Economically active: Unemployed
    EcoStat_B02ID,     # Working status - 6 categories
                       # Includes employed FT, employed PT, unemployed, etc.
                       # Partial match as categories may differ from census
    
    # Alternative employment variables:
    # EcoStat_B03ID,   # Working status - 4 categories (broader)
    # ES2020_B01ID,    # Employment Status - 2020 bandings
    
    # Census v50: NS-SeC Full-time students
    NSSec_B03ID,       # National Statistics Socio-Economic Classification
                       # 5 categories high level breakdown
                       # Good match to census NS-SeC
    
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
                       # Useful for geographic analysis
    
    HHoldCountry_B01ID, # Country of residence
                        # England, Scotland, Wales
    
    # === HOUSEHOLD COMPOSITION ===
    # Census v27-v30: Household structure (one-person, families with/without children)
    HHoldStruct_B02ID, # Household structure - 6 categories
                       # PARTIAL MATCH - categories may not align perfectly
                       # Compare categories before use
    
    HHoldNumAdults,    # Number of adults in household (actual count)
                       # Useful for creating custom household types
    
    HHoldNumChildren,  # Number of children in household (actual count)
                       # Useful for creating custom household types
    
    HHoldNumPeople,    # Total number of people in household
                       # Useful for household size analysis
    
    # === HOUSING ===
    # Census v33-v36: Property type (Detached, Semi-detached, Terraced, Flat)
    AddressType_B01ID, # Type of property
                       # Good match to census categories
    
    # Census v37-v39: Tenure (Owned, Social rented, Private rented)
    Ten1_B02ID,        # Type of tenancy - 3 categories
                       # Good match to census categories
                       # Owned/shared ownership, social rented, private rented
    
    # Census v32: Length of residence (PARTIAL MATCH)
    HLongAN_B01ID,     # How long lived at address
                       # Available in some years only (check year coverage)
                       # May not align exactly with census "1 year ago" measure
    
    # === VEHICLES ===
    # Census v44: 2 or more cars/vans in household (EXCELLENT MATCH)
    NumCarVan,         # Number of cars/vans - actual count
                       # Can create "2+" category to match census
    
    NumCarVan_B02ID,   # Number of cars/vans - 3 categories
                       # Pre-banded, may include a "2+" category
    
    NumCar,            # Number of cars only (excludes vans)
                       # Bonus variable for detailed analysis
    
    NumBike,           # Number of bicycles
                       # Bonus variable - not in census
    
    NumMCycle,         # Number of motorcycles
                       # Bonus variable - not in census
    
    # === INCOME ===
    HHIncQDS2008_B01ID, # Household income - Year-specific income quintiles
                        # Note: Household income - 3 categories also available
    
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
mutate(
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

# Save to CSV
# write.csv(nts_merged, "nts_selected_variables.csv", row.names = FALSE)

# Save to RDS (preserves R data types)
# saveRDS(nts_merged, "nts_selected_variables.rds")


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