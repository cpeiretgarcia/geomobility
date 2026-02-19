############################################################################
############################################################################
###                                                                      ###
###                     TRAVEL BEHAVIOUR VARIABLES                       ###
###                                                                      ###
############################################################################
############################################################################

# This code builds individual-level travel behaviour variables from the 
# NTS trip and stage diaries for use in cluster analysis.
# Variables are designed to answer:
#   - What is your main mode of transport?
#   - How heterogeneous are the transport modes you use?
#   - What activities do you do throughout the day?
#   - How diverse are they?
#   - How much do you travel, and how far?
#   - When do you travel?
#   - How car-dependent are you?
#   - How much do you travel by active modes?
#   - How do you travel to work, and do you work from home?
#   - Do you substitute travel with digital alternatives?
#
# Output is joined to the demographic variables created in the companion
# script (nts_demo_variables.parquet) using IndividualID as the key.

##################################################################
##                         DATA LOADING                         ##
##################################################################

# Load trip diary data
nts_trip <- read.table(
  "./data/UKDA-5340-tab/tab/trip_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

# Load stage diary data
# Needed for party size (social travel) and stage-level mode detail
nts_stage <- read.table(
  "./data/UKDA-5340-tab/tab/stage_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

# Load individual data
# Needed for self-reported mode frequencies, WFH, and digital substitution
nts_individual <- read.table(
  "./data/UKDA-5340-tab/tab/individual_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

# Load day-level data
# Needed for day-of-week variables (TravelWeekDay_B03ID) which are not in
# the trip table. Joined to nts_trip_clean via DayID after filtering.
nts_day <- read.table(
  "./data/UKDA-5340-tab/tab/day_eul_2002-2024.tab",
  sep = "\t",
  header = TRUE
)

# Load the demographic variables created in the companion script
nts_demographics <- read_parquet("./data/processed/nts_demo_variables.parquet")


##################################################################
##                        DATA FILTERING                        ##
##################################################################

# Keep only 2024 data
nts_individual  <- nts_individual  %>% filter(SurveyYear == 2024)
nts_household   <- nts_household   %>% filter(SurveyYear == 2024)
nts_trip        <- nts_trip        %>% filter(SurveyYear == 2024)
nts_stage       <- nts_stage       %>% filter(SurveyYear == 2024)
nts_day         <- nts_day         %>% filter(SurveyYear == 2024)

# Remove invalid / dead trips before building any derived variables.
# Negative values (-8 = NA, -9 = DNA, -10 = DEAD) are used throughout NTS.
# We filter on the key variables used in construction below.

nts_trip_clean <- nts_trip %>%
  filter(
    MainMode_B04ID  > 0,        # Valid mode
    TripPurpose_B04ID > 0,      # Valid purpose
    TripDisExSW >= 0,           # Valid distance (0 is possible for very short trips)
    TripTotalTime > 0,          # Valid duration
    TripStart >= 0              # Valid start time
  ) %>%
  # Join day-of-week from the Day table via DayID.
  # We select only the variables needed to keep the join lightweight.
  # TravelWeekDay_B03ID: 1 = Weekday, 2 = Weekend
  # TravelWeekDay_B01ID: actual day of week (Mon-Sun), retained for future use
  # TravelDayType_B01ID: includes bank holidays (2008 onwards)
  left_join(
    nts_day %>% select(DayID, TravelWeekDay_B01ID, TravelWeekDay_B03ID, TravelDayType_B01ID),
    by = "DayID"
  )

nts_stage_clean <- nts_stage %>%
  filter(
    StageMode_B04ID > 0,        # Valid mode
    NumParty > 0                # Valid party size
  )


##################################################################
##                      MODAL PROFILE                           ##
##################################################################

# NOTE ON MODE GROUPINGS (MainMode_B04ID):
#   1  = Walk
#   2  = Pedal cycle
#   3  = Car / van driver
#   4  = Car / van passenger
#   5  = Motorcycle
#   6  = Other private transport
#   7  = Bus in London
#   8  = Other local bus
#   9  = Non-local bus
#   10 = London Underground
#   11 = Surface Rail
#   12 = Taxi / minicab
#   13 = Other public transport

# We aggregate into 5 groups for share calculation:
#   - Walk
#   - Cycle
#   - Car (driver + passenger combined; split further below)
#   - Public transport (all bus, rail, underground, taxi)
#   - Other

modal_profile <- nts_trip_clean %>%
  mutate(
    mode_group = case_when(
      MainMode_B04ID == 1              ~ "walk",
      MainMode_B04ID == 2              ~ "cycle",
      MainMode_B04ID %in% c(3, 4)     ~ "car",
      MainMode_B04ID %in% c(7:13)     ~ "pt",
      MainMode_B04ID %in% c(5, 6)     ~ "other",
      TRUE                            ~ NA_character_
    ),
    is_car_driver   = as.integer(MainMode_B04ID == 3),
    is_car_pass     = as.integer(MainMode_B04ID == 4),
    is_walk         = as.integer(MainMode_B04ID == 1),
    is_cycle        = as.integer(MainMode_B04ID == 2),
    is_pt           = as.integer(MainMode_B04ID %in% c(7:13))
  ) %>%
  group_by(IndividualID) %>%
  summarise(
    # Total valid trips in the diary week (denominator for shares)
    total_trips = n(),
    
    # Mode shares - proportion of all trips by each mode group
    # Shares sum to 1 across walk + cycle + car + pt + other
    walk_share        = sum(is_walk)       / total_trips,
    cycle_share       = sum(is_cycle)      / total_trips,
    car_share         = sum(mode_group == "car",  na.rm = TRUE) / total_trips,
    pt_share          = sum(is_pt)         / total_trips,
    other_share       = sum(mode_group == "other", na.rm = TRUE) / total_trips,
    
    # Car driver vs passenger split - useful for distinguishing independent 
    # car users from those dependent on others to drive them
    car_driver_share  = sum(is_car_driver) / total_trips,
    car_pass_share    = sum(is_car_pass)   / total_trips,
    
    # Active travel share - walk + cycle combined
    # Used as a sustainable travel indicator
    active_share      = (sum(is_walk) + sum(is_cycle)) / total_trips,
    
    # Main mode - the single mode used most frequently across the diary week
    # Ties are broken by taking the first modal value (rare in practice)
    main_mode = names(sort(table(mode_group), decreasing = TRUE))[1],
    
    .groups = "drop"
  )


##################################################################
##                     MODE DIVERSITY (ENTROPY)                 ##
##################################################################

# Shannon entropy measures how spread trips are across modes.
# H = -Σ (p_i × ln(p_i)), where p_i is the share of trips by mode i.
# H = 0: all trips by a single mode (maximum concentration)
# H increases as trips spread more evenly across more modes.
#
# We compute entropy over the 5 mode groups defined above so that
# it is comparable across individuals.

mode_entropy <- modal_profile %>%
  select(IndividualID, walk_share, cycle_share, car_share, pt_share, other_share) %>%
  pivot_longer(
    cols      = -IndividualID,
    names_to  = "mode",
    values_to = "share"
  ) %>%
  filter(share > 0) %>%   # log(0) is undefined; exclude modes not used
  group_by(IndividualID) %>%
  summarise(
    mode_entropy = -sum(share * log(share)),
    .groups = "drop"
  )


##################################################################
##                     ACTIVITY PROFILE                         ##
##################################################################

# NOTE ON PURPOSE GROUPINGS (TripPurpose_B04ID):
#   1 = Commuting
#   2 = Business
#   3 = Education / escort education
#   4 = Shopping
#   5 = Other escort
#   6 = Personal business
#   7 = Leisure
#   8 = Other including just walk

activity_profile <- nts_trip_clean %>%
  mutate(
    is_commute    = as.integer(TripPurpose_B04ID == 1),
    is_business   = as.integer(TripPurpose_B04ID == 2),
    is_education  = as.integer(TripPurpose_B04ID == 3),
    is_shopping   = as.integer(TripPurpose_B04ID == 4),
    is_escort     = as.integer(TripPurpose_B04ID == 5),
    is_personal   = as.integer(TripPurpose_B04ID == 6),
    is_leisure    = as.integer(TripPurpose_B04ID == 7),
    is_other      = as.integer(TripPurpose_B04ID == 8)
  ) %>%
  group_by(IndividualID) %>%
  summarise(
    # Purpose shares - proportion of all trips by each activity type
    commute_share   = sum(is_commute)   / n(),
    business_share  = sum(is_business)  / n(),
    education_share = sum(is_education) / n(),
    shopping_share  = sum(is_shopping)  / n(),
    escort_share    = sum(is_escort)    / n(),
    personal_share  = sum(is_personal)  / n(),
    leisure_share   = sum(is_leisure)   / n(),
    other_share_act = sum(is_other)     / n(),  # suffix _act avoids clash with mode other_share
    
    # Dominant activity - the single purpose accounting for the most trips
    dominant_activity = names(sort(table(TripPurpose_B04ID), decreasing = TRUE))[1],
    
    .groups = "drop"
  )


##################################################################
##                   ACTIVITY DIVERSITY (ENTROPY)               ##
##################################################################

# Same Shannon entropy approach applied to trip purpose shares.
# High entropy = varied activity programme across the week.
# Low entropy = trips concentrated on one or two purposes
# (e.g. heavy commuter, or retired person with mostly leisure trips).

activity_entropy <- activity_profile %>%
  select(IndividualID, commute_share, business_share, education_share,
         shopping_share, escort_share, personal_share, leisure_share, other_share_act) %>%
  pivot_longer(
    cols      = -IndividualID,
    names_to  = "purpose",
    values_to = "share"
  ) %>%
  filter(share > 0) %>%
  group_by(IndividualID) %>%
  summarise(
    activity_entropy = -sum(share * log(share)),
    .groups = "drop"
  )


##################################################################
##                   TRIP RATE AND COMPLEXITY                   ##
##################################################################

# Trip rate is calculated over the full 7-day diary week.
# We use diary days actually observed per individual to compute 
# a per-day average, which handles partial diary weeks.

trip_rate <- nts_trip_clean %>%
  group_by(IndividualID) %>%
  summarise(
    # Total trips across the diary week
    total_trips_week  = n(),
    
    # Number of distinct travel days (TravDay 1-7)
    n_travel_days     = n_distinct(TravDay),
    
    # Average trips per day across the diary week
    # Denominator is 7 (full week) for comparability across individuals
    mean_trips_per_day = n() / 7,
    
    # Average number of stages per trip - proxy for journey complexity
    # More stages = more interchanges / multi-modal journeys
    mean_stages_per_trip = mean(NumStages, na.rm = TRUE),
    
    .groups = "drop"
  )

# Flag non-travellers - individuals in the individual file who made 
# zero trips in the diary week. Important to handle explicitly in clustering.
non_travellers <- nts_individual %>%
  select(IndividualID) %>%
  anti_join(nts_trip_clean %>% select(IndividualID) %>% distinct(),
            by = "IndividualID") %>%
  mutate(non_traveller = 1L)


##################################################################
##                       SPATIAL RANGE                          ##
##################################################################

# Distance variables are derived from TripDisExSW (excludes short walk legs)
# which is more stable for longer trips. TripDisIncSW is used for total
# distance as it captures all movement.

spatial_range <- nts_trip_clean %>%
  group_by(IndividualID) %>%
  summarise(
    # Mean trip distance - central tendency of how far individuals travel
    mean_trip_dist_miles = mean(TripDisExSW, na.rm = TRUE),
    
    # Total distance travelled in the diary week
    total_dist_week_miles = sum(TripDisIncSW, na.rm = TRUE),
    
    # Local travel share - proportion of trips under 2 miles
    # Captures hyperlocal vs wider-ranging travellers
    local_trip_share = sum(TripDisExSW < 2, na.rm = TRUE) / n(),
    
    # Long distance share - proportion of trips over 25 miles
    # Captures individuals who regularly make longer journeys
    long_trip_share  = sum(TripDisExSW > 25, na.rm = TRUE) / n(),
    
    .groups = "drop"
  )


##################################################################
##                       TEMPORAL PATTERNS                      ##
##################################################################

# TripStart is minutes past midnight (0-1439).
# AM peak = 0700-0900 (420-539 min), PM peak = 1600-1800 (960-1079 min).
# Weekend trips are identified via TravelWeekDay_B03ID (1=Weekday, 2=Weekend).

temporal_patterns <- nts_trip_clean %>%
  mutate(
    in_am_peak  = as.integer(TripStart >= 420  & TripStart <= 539),
    in_pm_peak  = as.integer(TripStart >= 960  & TripStart <= 1079),
    in_peak     = as.integer(in_am_peak == 1 | in_pm_peak == 1),
    is_weekend  = as.integer(TravelWeekDay_B03ID == 2)
  ) %>%
  group_by(IndividualID) %>%
  summarise(
    # Proportion of trips made during AM or PM peak hours
    peak_share    = sum(in_peak)   / n(),
    
    # Proportion of trips made at the weekend
    weekend_share = sum(is_weekend) / n(),
    
    # Average trip start time in minutes past midnight
    # Captures whether someone is an early or late traveller
    mean_start_time_mins = mean(TripStart, na.rm = TRUE),
    
    .groups = "drop"
  )


##################################################################
##                       TRAVEL TIME                            ##
##################################################################

# Total time spent travelling per day and average trip duration.
# TripTotalTime includes waiting and walking time; TripTravTime is 
# in-vehicle / in-motion time only.

travel_time <- nts_trip_clean %>%
  group_by(IndividualID) %>%
  summarise(
    # Average total time per trip (minutes) - includes waiting
    mean_trip_time_mins = mean(TripTotalTime, na.rm = TRUE),
    
    # Total travel time in the diary week (minutes)
    total_travel_time_week = sum(TripTotalTime, na.rm = TRUE),
    
    # Average daily travel time - divided by 7 for weekly comparability
    mean_daily_travel_time_mins = sum(TripTotalTime, na.rm = TRUE) / 7,
    
    .groups = "drop"
  )


##################################################################
##                       SOCIAL TRAVEL                          ##
##################################################################

# NumParty from the stage table captures total party size per stage.
# We take the maximum party size across stages within each trip as
# a proxy for the trip-level party size, then average across the week.

social_travel <- nts_stage_clean %>%
  group_by(IndividualID, TripID = TripID) %>%
  summarise(
    trip_party_size = max(NumParty, na.rm = TRUE),  # Max across stages per trip
    .groups = "drop"
  ) %>%
  group_by(IndividualID) %>%
  summarise(
    # Average party size - values above 1 indicate travelling with others
    mean_party_size = mean(trip_party_size, na.rm = TRUE),
    
    # Share of trips made alone (party size = 1)
    solo_travel_share = sum(trip_party_size == 1) / n(),
    
    .groups = "drop"
  )


##################################################################
##                    SELF-REPORTED MODE FREQUENCIES            ##
##################################################################

# These complement the diary-derived variables with self-reported
# habitual behaviour. Useful as they cover typical behaviour rather than
# just the diary week.
# Recoded to a 0-6 numeric scale (higher = more frequent).
#
# Original coding (applies to PrivCar2_B01ID, OrdBus2Freq_B01ID, etc.):
#   1 = 3 or more days a week   -> 6
#   2 = 1-2 days a week         -> 4
#   3 = Less than once a week   -> 2
#   4 = Never                   -> 0
#   Negative values             -> NA

self_reported_modes <- nts_individual %>%
  select(
    IndividualID,
    PrivCar2_B01ID,       # How frequently travel by private car
    OrdBus2Freq_B01ID,    # How frequently use local buses
    Train2Freq_B01ID,     # How frequently use a train
    Walk2Freq_B01ID,      # How frequently walk for 20+ mins
    Bicycle3Freq_B01ID,   # How frequently use a bicycle
    TaxiCab2Freq_B01ID,   # How frequently use a taxi/minicab
    PHVFreq2_B01ID        # How frequently use app-based taxi (e.g. Uber)
  ) %>%
  mutate(across(
    c(PrivCar2_B01ID, OrdBus2Freq_B01ID, Train2Freq_B01ID,
      Walk2Freq_B01ID, Bicycle3Freq_B01ID, TaxiCab2Freq_B01ID, PHVFreq2_B01ID),
    ~ case_when(
      .x == 1  ~ 6L,   # 3+ days a week
      .x == 2  ~ 4L,   # 1-2 days a week
      .x == 3  ~ 2L,   # Less than once a week
      .x == 4  ~ 0L,   # Never
      .x <  0  ~ NA_integer_,
      TRUE     ~ NA_integer_
    )
  )) %>%
  rename(
    freq_car         = PrivCar2_B01ID,
    freq_bus         = OrdBus2Freq_B01ID,
    freq_train       = Train2Freq_B01ID,
    freq_walk        = Walk2Freq_B01ID,
    freq_cycle       = Bicycle3Freq_B01ID,
    freq_taxi        = TaxiCab2Freq_B01ID,
    freq_phv         = PHVFreq2_B01ID
  )


##################################################################
##                  WORK FROM HOME & DIGITAL SUBSTITUTION       ##
##################################################################

# These capture the degree to which individuals substitute travel
# with digital activity - an increasingly important dimension
# of travel behaviour.
#
# OftHome_B01ID coding (frequency of WFH):
#   1 = 3+ times a week  -> 6
#   2 = Once or twice a week -> 4
#   3 = Less than once a week, more than twice a month -> 3
#   4 = Once or twice a month -> 2
#   5 = Less than once a month, more than twice a year -> 1
#   6 = Once or twice a year -> 0.5 (rare)
#   7 = Less than once a year or never -> 0

digital_substitution <- nts_individual %>%
  select(
    IndividualID,
    WkHome_B01ID,     # Whether worked from home in diary week (1=Yes, 2=No)
    OftHome_B01ID,    # Frequency of working from home
  ) %>%
  mutate(
    # Binary: did work from home at all in the diary week
    wfh_any = case_when(
      WkHome_B01ID == 1 ~ 1L,
      WkHome_B01ID == 2 ~ 0L,
      TRUE              ~ NA_integer_
    ),
    
    # Numeric WFH frequency (0 = never, 6 = 3+ times a week)
    wfh_frequency = case_when(
      OftHome_B01ID == 1 ~ 6L,
      OftHome_B01ID == 2 ~ 4L,
      OftHome_B01ID == 3 ~ 3L,
      OftHome_B01ID == 4 ~ 2L,
      OftHome_B01ID == 5 ~ 1L,
      OftHome_B01ID == 6 ~ 0L,
      OftHome_B01ID == 7 ~ 0L,
      OftHome_B01ID <  0 ~ NA_integer_,
      TRUE               ~ NA_integer_
    )
  ) %>%
  select(IndividualID, wfh_any, wfh_frequency)


##################################################################
##                        MERGE ALL COMPONENTS                  ##
##################################################################

# Join all behaviour components onto the full individual spine.
# We left-join from nts_demographics so that every individual in the
# demographic file is retained, even non-travellers.
# Non-travellers will have NA for all diary-derived variables - 
# use the non_traveller flag to handle them explicitly.

nts_behaviour <- nts_demographics %>%
  select(IndividualID) %>%          # Use demographic file as spine
  
  left_join(modal_profile,          by = "IndividualID") %>%
  left_join(mode_entropy,           by = "IndividualID") %>%
  left_join(activity_profile,       by = "IndividualID") %>%
  left_join(activity_entropy,       by = "IndividualID") %>%
  left_join(trip_rate,              by = "IndividualID") %>%
  left_join(spatial_range,          by = "IndividualID") %>%
  left_join(temporal_patterns,      by = "IndividualID") %>%
  left_join(travel_time,            by = "IndividualID") %>%
  left_join(social_travel,          by = "IndividualID") %>%
  left_join(self_reported_modes,    by = "IndividualID") %>%
  left_join(digital_substitution,   by = "IndividualID") %>%
  left_join(non_travellers,         by = "IndividualID") %>%
  
  # Replace NA in non_traveller flag for confirmed travellers
  mutate(non_traveller = replace_na(non_traveller, 0L))


##################################################################
##              JOIN BEHAVIOUR TO DEMOGRAPHIC VARIABLES         ##
##################################################################

# Final combined dataset: demographics + behaviour at individual level.
# All variables from the companion demographic script are retained.
# Behavioural variables are added as additional columns.

nts_combined <- nts_demographics %>%
  left_join(
    nts_behaviour %>% select(-IndividualID, everything(), IndividualID),
    by = "IndividualID"
  )


##################################################################
##                           SAVE OUTPUT                        ##
##################################################################

write_parquet(nts_behaviour,  "./data/processed/nts_behaviour_variables.parquet")
write_parquet(nts_combined,   "./data/processed/nts_combined_variables.parquet")


##################################################################
##                        IMPORTANT NOTES                       ##
##################################################################

# 1. NON-TRAVELLERS: Individuals with non_traveller == 1 made zero diary trips.
#    For clustering, decide whether to: (a) exclude them and treat as a 
#    separate group, (b) impute zeros for share variables and NA for 
#    entropy/distance, or (c) include them with a zero-trip profile.
#    Option (a) is generally recommended.

# 2. VARIABLE COLLINEARITY: Several variables are mathematically related.
#    walk_share + cycle_share + car_share + pt_share + other_share = 1,
#    so drop one before clustering (e.g. other_share).
#    active_share = walk_share + cycle_share, so use one or the other.
#    car_driver_share + car_pass_share = car_share, so use sparingly together.
#    Check a correlation matrix before finalising variable selection.

# 3. ENTROPY AND SHARES: mode_entropy is correlated with mode shares.
#    Include entropy OR shares in the cluster model, not both.
#    Entropy summarises diversity in a single variable; shares provide 
#    more granular information but require more dimensions.

# 4. YEAR COVERAGE: Self-reported mode frequency variables 
#    (e.g. PrivCar2_B01ID, Walk2Freq_B01ID) were not collected in all years.
#    Check availability in the lookup table before pooling years.

# 5. STANDARDISATION: Before clustering, standardise all continuous variables
#    (e.g. z-score or min-max) so that variables on different scales 
#    (e.g. total_dist_week_miles vs mode_entropy) contribute equally.

# 6. MIXED VARIABLE TYPES: main_mode and dominant_activity are character 
#    variables. For k-means, one-hot encode these. For PAM/k-medoids with 
#    Gower distance, they can be used as-is as factors.

# 7. WEIGHTS: Behavioural variables derived from the trip diary should use 
#    W5 (weighted travel sample) when computing population-level statistics.
#    For clustering on individual profiles (not population estimation), 
#    weights are not applied to the variable construction itself.