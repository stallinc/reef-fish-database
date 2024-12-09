library(tidyverse)

# Import the data
fishes  <- read.csv("fishes.csv", check.names = FALSE)
surveys <- read.csv("surveys.csv", check.names = FALSE)
habitat <- read.csv("habitat.csv", check.names = FALSE)

dat <- 
  # Reference imported data
  fishes %>%
  # Exclude individual size data
  select(-contains("size")) %>%
  # Calculate total abundance for each species, in each survey
  summarize(
    number = sum(number),
    .by = c(survey_ID, sample_ID, sample_unit, species_code)
  ) %>%
  # Pivot into a species x observation matrix (abundances)
  pivot_wider(
    names_from  = species_code,
    values_from = number,
    # Add "0" for any species not observed in that survey
    values_fill = 0
  ) %>%
  # Join diver-reported visibility data for each survey. 
  left_join(
    select(
      surveys,
      sample_ID,
      visibility,
      reef_ID  # Include reef_ID in the join
    ),
    by = "sample_ID"
  ) %>%
  # Move visibility and reef_ID columns to the front of the data frame. 
  relocate(
    visibility,
    .after = sample_unit
  ) %>%
  relocate(
    reef_ID,
    .after = visibility
  ) %>%
  mutate(
    # Calculate survey area
    survey_area = pi * (visibility^2),
    .after = visibility
  ) %>%
  # Remove any surveys without survey radii
  filter(
    !is.na(visibility)
  ) %>%
  # Pivot table longer for density calculations
  pivot_longer(
    cols = -c(survey_ID, sample_ID, sample_unit, visibility, reef_ID, survey_area),  # Exclude non-species columns
    names_to  = "species_code",
    values_to = "number"
  ) %>%
  # Calculate densities
  mutate(
    density = number / survey_area
  ) %>%
  # Summarize densities at the level of 'survey_ID', which is the true replicate
  summarize(
    density      = mean(density),
    survey_count = n(),
    .by = c(survey_ID, species_code)
  ) %>%
  # Pivot into a species x observation matrix (densities)
  pivot_wider(
    names_from  = species_code,
    values_from = density,
  ) %>%
  # Join reef_ID from surveys dataframe
  left_join(
    select(
      surveys,
      survey_ID,
      reef_ID
    ),
    multiple = "first",
    by       = "survey_ID"
  ) %>%
  # Move reef_ID column to the front of the data frame
  relocate(
    reef_ID,
    .after = survey_ID
  ) %>%
  left_join(
    select(
      habitat,
      reef_ID,
      type
    ),
    by = "reef_ID"
  ) %>%
  relocate(
    type,
    .after = reef_ID
  )

# Hyper-abundant groups which may have included mixed taxa
# The user may want to exclude them from analyses due to lack of valid taxonomic information
# and due to the risk of species masking that can occur at the community level
HypAbn  <- c("BAIT","LARV")

# One-off species codes that include single-count observations at family level
# (e.g. CAFM - Carangidae) when species level representation is far greater
# throughout the data, thus not constituting a unique observation.
Drop <- c("CAFM","CALF","CLFM","EPSP","GOFM","GYSP","HOSP",
          "LAFM","LUSP","OPSP","OSFM","SCPP","SPPP","TEFM","UNKN")