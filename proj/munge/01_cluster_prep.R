# Load libraries
library(here)
library(readr)
library(dplyr)

# Load data
nts_trip_raw <- read_tsv(here('proj/data/UKDA-5340-tab/tab/trip_eul_2002-2024.tab'))

# Select needed variables
trip_df <- nts_trip_raw |>
  dplyr::select(
    TripID,
    IndividualID,
    HouseholdID,
    SurveyYear,
    MainMode_B04ID,    # main mode of transport 13 categories
    TripPurpose_B02ID, # trip purpose 14 categories
    TripDisIncSW,      # trip distance (including short walk)
    TripTotalTime,     # trip time
    TravDay,           # day of week (1=Monday ... 7=Sunday)
    TripStartHours     # departure hour (0-23)
  ) |>
  filter(SurveyYear %in% c(2022, 2023, 2024))

# Drop invalid records upstream so all summaries use the same trips
trip_df <- trip_df |>
  filter(MainMode_B04ID > 0, TripPurpose_B02ID > 0, TripDisIncSW > 0)

# Trip volume and distance per individual
trip_vol_dist <- trip_df |>
  group_by(IndividualID) |>
  summarise(
    n_trips          = n(),
    total_distance   = sum(TripDisIncSW),
    share_long_trips = mean(TripDisIncSW > 25)
  )

# Modal choice: mode used on the most trips per individual
# Modes grouped into 7 categories before finding the dominant mode:
#   1=Walk, 2=Cycle, 3=Car driver, 4=Car passenger,
#   5=Bus (any: London/local/non-local), 6=Rail (underground+surface), 7=Other
modal_choice <- trip_df |>
  mutate(mode_group = case_when(
    MainMode_B04ID == 1            ~ 1L, # Walk
    MainMode_B04ID == 2            ~ 2L, # Cycle
    MainMode_B04ID == 3            ~ 3L, # Car driver
    MainMode_B04ID == 4            ~ 4L, # Car passenger
    MainMode_B04ID %in% c(7, 8, 9) ~ 5L, # Bus
    MainMode_B04ID %in% c(10, 11)  ~ 6L, # Rail
    TRUE                           ~ 7L  # Other
  )) |>
  count(IndividualID, mode_group) |>
  slice_max(n, n = 1, with_ties = FALSE, by = IndividualID) |>
  select(IndividualID, modal_choice = mode_group)

# Mode share per individual
# Shares mirror the 7 modal choice groups:
#   1=Walk, 2=Cycle, 3=Car driver, 4=Car passenger,
#   5=Bus (any), 6=Rail, 7=Other (motorcycle, other private, taxi, other PT)
trip_mode_share <- trip_df |>
  group_by(IndividualID) |>
  summarise(
    share_walk       = mean(MainMode_B04ID == 1),
    share_cycle      = mean(MainMode_B04ID == 2),
    share_car_driver = mean(MainMode_B04ID == 3),
    share_car_pass   = mean(MainMode_B04ID == 4),
    share_bus        = mean(MainMode_B04ID %in% c(7, 8, 9)),
    share_rail       = mean(MainMode_B04ID %in% c(10, 11)),
    share_other      = mean(MainMode_B04ID %in% c(5, 6, 12, 13))
  )

# Trip purpose mix per individual
# Based on TripPurpose_B02ID (14 categories):
#   Commute & work:       1=Commuting, 2=Business
#   Education:            3=Education
#   Education escort:     4=Escort education
#   Shopping & errands:   5=Shopping, 7=Personal business
#   Escort (non-educ):    6=Other escort
#   Leisure & social:     8=Visiting friends (home), 9=Visiting friends (elsewhere),
#                         10=Entertainment, 11=Sport, 13=Day trip
#   Other:                12=Holiday base, 14=Other incl. just walk
trip_purpose_mix <- trip_df |>
  group_by(IndividualID) |>
  summarise(
    share_work          = mean(TripPurpose_B02ID %in% c(1, 2)),
    share_education     = mean(TripPurpose_B02ID == 3),
    share_escort_educ   = mean(TripPurpose_B02ID == 4),
    share_shopping      = mean(TripPurpose_B02ID %in% c(5, 7)),
    share_escort        = mean(TripPurpose_B02ID == 6),
    share_leisure       = mean(TripPurpose_B02ID %in% c(8, 9, 10, 11, 13))
  )

# Weekend and night worker flags
# Weekend worker: commute trip on Saturday (6) or Sunday (7)
# Night worker: commute trip departing before 06:00 or after 20:00
atypical_worker <- trip_df |>
  filter(TripPurpose_B02ID == 1) |>  # commute trips only
  group_by(IndividualID) |>
  summarise(
    weekend_worker = any(TravDay %in% c(6, 7)),
    night_worker   = any(TripStartHours < 6 | TripStartHours >= 20)
  )

# Combine all behavioural variables into one individual-level dataset
indiv_cluster_df <- trip_vol_dist |>
  left_join(modal_choice, by = "IndividualID") |>
  left_join(trip_mode_share, by = "IndividualID") |>
  left_join(trip_purpose_mix, by = "IndividualID") |>
  left_join(atypical_worker, by = "IndividualID") |>
  mutate(
    weekend_worker = coalesce(weekend_worker, FALSE),
    night_worker   = coalesce(night_worker, FALSE)
  )
