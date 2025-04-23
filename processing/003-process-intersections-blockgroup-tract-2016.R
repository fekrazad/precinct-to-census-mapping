library(data.table)
library(tidyverse)


col_types_spec <- cols(
  bg_COUNTYFP = col_character(),
  bg_STATEFP = col_character(),
  bg_GEOID = col_character()
)

file_path <- "/Users/amir/Downloads/Data/intersections/"
files_2016 <- list.files(path = file_path, pattern = "2016", full.names = TRUE)

process_file <- function(file) {
  read_csv(file, col_types = col_types_spec) %>%
    select(precinct_unique_id, precinct_area, starts_with('bg_'), starts_with('G16'),
           fraction_area, habitability_sum, starts_with('lc_'))
}


intersections <- files_2016 %>%
  map_dfr(process_file)



intersections2 <- intersections %>% mutate(
  tract_GEOID = str_sub(bg_GEOID, 1, -2),
  precinct_unique_id = str_c(
    bg_STATEFP,
    '_',
    as.character(precinct_unique_id)
  )
)


# ACS tracts 2016
acs_tract <- read_csv( '/Users/amir/Downloads/Data/ACSCensusTract/ACSDP5Y2016.DP05-Data.csv' ) %>%
  slice(-1) %>%
  mutate(tract_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    tract_GEOID,
    tract_population = DP05_0001E,
  ) %>%
  mutate(
    tract_population = as.numeric(tract_population)
  )

# ACS tracts hh 2016
acs_tract_hh <- read_csv( 'data/houshold-pop-tracts/ACSDT5Y2016.B11002-Data.csv' ) %>%
  slice(-1) %>%
  mutate(tract_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    tract_GEOID,
    tract_hh_population = B11002_001E,
  ) %>%
  mutate(
    tract_hh_population = as.numeric(tract_hh_population)
  )

acs_tract2 <- acs_tract %>% inner_join(acs_tract_hh, by="tract_GEOID")


acs_bg <- read_csv( '/Users/amir/Downloads/Data/ACSBlockGroup/ACSDT5Y2016.B01003-Data.csv' ) %>%
  slice(-1) %>%
  mutate(year = 2016, bg_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    bg_GEOID,
    bg_population = B01003_001E,
  ) %>%
  mutate(
    bg_population = as.numeric(bg_population)
  )

# block group hh population
acs_bg_hh <- read_csv( 'data/household-pop/ACSDT5Y2016.B11002-Data.csv' ) %>%
  slice(-1) %>%
  mutate(bg_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    bg_GEOID,
    bg_hh_population = B11002_001E,
  ) %>%
  mutate(
    bg_hh_population = as.numeric(bg_hh_population)
  )

acs_bg2 <- acs_bg %>% inner_join(acs_bg_hh)


df <- intersections2 %>% inner_join(acs_tract2, by = "tract_GEOID")
df <- df %>% inner_join(acs_bg2, by = "bg_GEOID") 

remove(intersections, intersections2, acs_tract, acs_bg, acs_tract_hh, acs_bg_hh, acs_tract2, acs_bg2)

#View(df %>% filter(is.na(tract_population)))


temp <- df %>% group_by(tract_GEOID, bg_GEOID) %>% summarise(
  tract_population = first(tract_population),
  bg_population = first(bg_population)
) %>% summarise(
  tract_population = first(tract_population),
  tract_population2 = sum(bg_population)
) %>% mutate(dif = tract_population2 - tract_population)
temp <- df %>% group_by(bg_GEOID) %>% summarise()




tract_areas <- df %>% group_by(tract_GEOID, bg_GEOID) %>% summarise(
  bg_ALAND = first(bg_ALAND),
  bg_AWATER = first(bg_AWATER)
) %>% summarise(tract_ALAND = sum(bg_ALAND), tract_AWATER = sum(bg_AWATER)) 

df_with_area <- df %>% inner_join(tract_areas, by = "tract_GEOID")




bg_lc_estimates <- read_csv("block-group-landcover-estimates-2016.csv") %>% rename(bg_GEOID = GEOID)


df_with_lc_estimates <- df_with_area %>% inner_join(bg_lc_estimates, by=c("bg_GEOID")) %>%
  mutate(
    w1 = fraction_area,
    w2 = habitability_sum,
    lc_82 = ifelse( is.na(lc_82), 0, lc_82 ), # Entire DC does not have any lc_82 so it shows as NA. Change to 0.
    w3 = 
      coef_lc_21 * lc_21 +
      coef_lc_22 * lc_22 +
      coef_lc_23 * lc_23 +
      coef_lc_24 * lc_24 +
      coef_lc_52 * lc_52 +
      coef_lc_81 * lc_81 +
      coef_lc_82 * lc_82 +
      coef_lc_41 * lc_41 +
      coef_lc_42 * lc_42 +
      coef_lc_43 * lc_43 +
      coef_lc_31 * lc_31 +
      coef_lc_71 * lc_71
  )

df_with_lc_estimates_relative_weights <- df_with_lc_estimates %>% group_by(bg_GEOID) %>% mutate(
  w1_total = sum(w1),
  w2_total = sum(w2),
  w3_total = sum(w3),
  
  w1_r = w1 / w1_total,
  w2_r = ifelse(w2_total > 0, w2 / w2_total, w1_r),
  w3_r = ifelse(w3_total > 0, w3 / w3_total, w2_r),
  
  pop1 = w1_r * bg_hh_population,
  pop2 = w2_r * bg_hh_population,
  pop3 = w3_r * bg_hh_population,
  
) %>% ungroup()



df_dist_precinct <- df_with_lc_estimates_relative_weights %>% group_by(precinct_unique_id) %>%
  mutate(
    precinct_pop1 = sum(pop1),
    precinct_pop2 = sum(pop2),
    precinct_pop3 = sum(pop3),
    vote_allocation_weight1 = ifelse(precinct_pop1 > 0, pop1/precinct_pop1, fraction_area/precinct_area),
    vote_allocation_weight2 = ifelse(precinct_pop2 > 0, pop2/precinct_pop2, vote_allocation_weight1),
    vote_allocation_weight3 = ifelse(precinct_pop3 > 0, pop3/precinct_pop3, vote_allocation_weight2)
  ) %>% ungroup()

remove(bg_lc_estimates, df_with_lc_estimates, df_with_lc_estimates_relative_weights)

remove(df)

write_rds(df_dist_precinct, "bg-before-agg-2016.rds")
df_dist_precinct <- read_rds("bg-before-agg-2016.rds")

vote_cols_all <- grep("^G16", names(df_dist_precinct), value = TRUE)
vote_cols_pre <- grep("^G16PRE", names(df_dist_precinct), value = TRUE)
vote_cols_uss <- grep("^G16USS", names(df_dist_precinct), value = TRUE)
vote_cols_hse <- grep("^G16H", names(df_dist_precinct), value = TRUE)
vote_cols_gov <- grep("^G16GOV", names(df_dist_precinct), value = TRUE)
vote_cols_other <- sort( setdiff(vote_cols_all, c(vote_cols_pre, vote_cols_uss, vote_cols_hse, vote_cols_gov) ) )
vote_cols <- c(vote_cols_pre, vote_cols_uss, vote_cols_hse, vote_cols_gov, vote_cols_other)

# areal
df_weight1_applied <- df_dist_precinct %>% mutate(
    across(starts_with('G16'), ~ .x * vote_allocation_weight1)
)

setDT(df_weight1_applied)

votes_bg_agg1 <- df_weight1_applied[
  , c(
    .(bg_state_fp = first(bg_STATEFP),
      bg_county_fp = first(bg_COUNTYFP),
      bg_population = first(bg_population),
      bg_hh_population = first(bg_hh_population),
      bg_land_area = first(bg_ALAND),
      bg_water_area = first(bg_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(bg_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_bg_agg1, 
  file = "REVISE/produced-data/election-results-block-groups-2016-areal-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)


# --------------------------------------

votes_tract_agg1 <- df_weight1_applied[
  , c(
    .(tract_state_fp = first(bg_STATEFP),
      tract_county_fp = first(bg_COUNTYFP),
      tract_population = first(tract_population),
      tract_hh_population = first(tract_hh_population),
      tract_land_area = first(tract_ALAND),
      tract_water_area = first(tract_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(tract_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_tract_agg1, 
  file = "REVISE/produced-data/election-results-census-tracts-2016-areal-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

remove(votes_bg_agg1,df_weight1_applied)

# --------------------------------------


# imperviousness

df_weight2_applied <- df_dist_precinct %>% mutate(
  across(starts_with('G16'), ~ .x * vote_allocation_weight2)
)

setDT(df_weight2_applied)

votes_bg_agg2 <- df_weight2_applied[
  , c(
    .(bg_state_fp = first(bg_STATEFP),
      bg_county_fp = first(bg_COUNTYFP),
      bg_population = first(bg_population),
      bg_hh_population = first(bg_hh_population),
      bg_land_area = first(bg_ALAND),
      bg_water_area = first(bg_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(bg_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_bg_agg2, 
  file = "REVISE/produced-data/election-results-block-groups-2016-imperviousness-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

remove(votes_bg_agg2)

# --------------------------------------

votes_tract_agg2 <- df_weight2_applied[
  , c(
    .(tract_state_fp = first(bg_STATEFP),
      tract_county_fp = first(bg_COUNTYFP),
      tract_population = first(tract_population),
      tract_hh_population = first(tract_hh_population),
      tract_land_area = first(tract_ALAND),
      tract_water_area = first(tract_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(tract_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_tract_agg2, 
  file = "REVISE/produced-data/election-results-census-tracts-2016-imperviousness-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

remove(votes_tract_agg2,df_weight2_applied)

# --------------------------------------


# land cover

df_weight3_applied <- df_dist_precinct %>% mutate(
  across(starts_with('G16'), ~ .x * vote_allocation_weight3)
)

setDT(df_weight3_applied)

votes_bg_agg3 <- df_weight3_applied[
  , c(
    .(bg_state_fp = first(bg_STATEFP),
      bg_county_fp = first(bg_COUNTYFP),
      bg_population = first(bg_population),
      bg_hh_population = first(bg_hh_population),
      bg_land_area = first(bg_ALAND),
      bg_water_area = first(bg_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(bg_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_bg_agg3, 
  file = "REVISE/produced-data/election-results-block-groups-2016-landcover-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

# --------------------------------------

votes_tract_agg3 <- df_weight3_applied[
  , c(
    .(tract_state_fp = first(bg_STATEFP),
      tract_county_fp = first(bg_COUNTYFP),
      tract_population = first(tract_population),
      tract_hh_population = first(tract_hh_population),
      tract_land_area = first(tract_ALAND),
      tract_water_area = first(tract_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(tract_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_tract_agg3, 
  file = "REVISE/produced-data/election-results-census-tracts-2016-landcover-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

