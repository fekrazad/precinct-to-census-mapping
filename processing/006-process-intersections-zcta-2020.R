library(data.table)
library(tidyverse)


col_types_spec <- cols(
  zg_GEOID20 = col_character(),
  zg_GEOID = col_character(),
  zg_COUNTYFP = col_character(),
  zg_ZCTA5CE20 = col_character(),
  zg_STATEFP = col_character()
)

file_path <- "/Users/amir/Downloads/Data/intersections-zcta/"
files_2020 <- list.files(path = file_path, pattern = "2020", full.names = TRUE)

process_file <- function(file) {
  read_csv(file, col_types = col_types_spec) %>%
    select(precinct_unique_id, precinct_area, starts_with('zg_'), starts_with('zcta_'), starts_with('G20'),
           fraction_area, habitability_sum, starts_with('lc_'))
}


intersections <- files_2020 %>%
  map_dfr(process_file) 

intersections2 <- intersections %>% rename(
  zcta_GEOID = zg_GEOID20,
  zcta_ALAND = zg_ALAND20,
  zcta_AWATER= zg_AWATER20,
  bg_GEOID = zg_GEOID,
  bg_ALAND = zg_ALAND,
  bg_AWATER= zg_AWATER,
) %>% mutate(
  precinct_unique_id = str_c(
    zg_STATEFP,
    '_',
    as.character(precinct_unique_id)
  )
)

acs_zcta <- read_csv( '/Users/amir/Downloads/Data/ACS-ZCTA/ACSDP5Y2020.DP05-Data.csv' ) %>%
  slice(-1) %>%
  mutate(zcta_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    zcta_GEOID,
    zcta_population = DP05_0001E,
  ) %>%
  mutate(
    zcta_GEOID = as.character(zcta_GEOID),
    zcta_population = as.numeric(zcta_population)
  )

acs_bg <- read_csv( '/Users/amir/Downloads/Data/ACSBlockGroup/ACSDT5Y2020.B01003-Data.csv' ) %>%
  slice(-1) %>%
  mutate(year = 2020, bg_GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    bg_GEOID,
    bg_population = B01003_001E,
  ) %>%
  mutate(
    bg_population = as.numeric(bg_population)
  )


df <- intersections2 %>% inner_join(acs_zcta, by = "zcta_GEOID")
df <- df %>% inner_join(acs_bg, by = "bg_GEOID")

remove(intersections, intersections2, acs_zcta)

#View(df %>% filter(is.na(tract_population)))

# number of unique zip codes
temp <- df %>% group_by(zcta_GEOID) %>% summarise()


bg_lc_estimates <- read_csv("block-group-landcover-estimates.csv")


df_with_lc_estimates <- df %>% inner_join(bg_lc_estimates, by=c("bg_GEOID")) %>%
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
  
  pop1 = w1_r * bg_population,
  pop2 = w2_r * bg_population,
  pop3 = w3_r * bg_population,
  
) %>% ungroup()



df_dist_precinct <- df_with_lc_estimates_relative_weights %>% group_by(precinct_unique_id) %>%
  mutate(
    precinct_pop1 = sum(pop1),
    precinct_pop2 = sum(pop2),
    precinct_pop3 = sum(pop3),
    votes_allocation_weight1 = ifelse(precinct_pop1 > 0, pop1/precinct_pop1, fraction_area/precinct_area),
    votes_allocation_weight2 = ifelse(precinct_pop2 > 0, pop2/precinct_pop2, votes_allocation_weight1),
    votes_allocation_weight3 = ifelse(precinct_pop3 > 0, pop3/precinct_pop3, votes_allocation_weight2)
  ) %>% ungroup()

remove(bg_lc_estimates, df_with_lc_estimates, df_with_lc_estimates_relative_weights)

remove(df)

write_rds(df_dist_precinct, "zcta-before-agg.rds")
df_dist_precinct <- read_rds("zcta-before-agg.rds")

vote_cols_all <- grep("^G20", names(df_dist_precinct), value = TRUE)
vote_cols_pre <- grep("^G20PRE", names(df_dist_precinct), value = TRUE)
vote_cols_uss <- grep("^G20USS", names(df_dist_precinct), value = TRUE)
vote_cols_hse <- grep("^G20H", names(df_dist_precinct), value = TRUE)
vote_cols_gov <- grep("^G20GOV", names(df_dist_precinct), value = TRUE)
vote_cols_other <- sort( setdiff(vote_cols_all, c(vote_cols_pre, vote_cols_uss, vote_cols_hse, vote_cols_gov) ) )
vote_cols <- c(vote_cols_pre, vote_cols_uss, vote_cols_hse, vote_cols_gov, vote_cols_other)

# areal
df_weight1_applied <- df_dist_precinct %>% mutate(
    across(starts_with('G20'), ~ .x * votes_allocation_weight1)
)

setDT(df_weight1_applied)

votes_zcta_agg1 <- df_weight1_applied[
  , c(
    .(
      zcta_population = first(zcta_population),
      zcta_land_area = first(zcta_ALAND),
      zcta_water_area = first(zcta_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(zcta_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_zcta_agg1, 
  file = "Produced Datasets/election-results-zcta-2020-areal-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)


# imperviousness
df_weight2_applied <- df_dist_precinct %>% mutate(
  across(starts_with('G20'), ~ .x * votes_allocation_weight2)
)

setDT(df_weight2_applied)

votes_zcta_agg2 <- df_weight2_applied[
  , c(
    .(
      zcta_population = first(zcta_population),
      zcta_land_area = first(zcta_ALAND),
      zcta_water_area = first(zcta_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(zcta_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_zcta_agg2, 
  file = "Produced Datasets/election-results-zcta-2020-imperviousness-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

# land cover
df_weight3_applied <- df_dist_precinct %>% mutate(
  across(starts_with('G20'), ~ .x * votes_allocation_weight3)
)

setDT(df_weight3_applied)

votes_zcta_agg3 <- df_weight3_applied[
  , c(
    .(
      zcta_population = first(zcta_population),
      zcta_land_area = first(zcta_ALAND),
      zcta_water_area = first(zcta_AWATER),
      num_precincts_contributing = uniqueN(precinct_unique_id)),
    lapply(.SD, sum)  # Sum columns
  ), 
  by = .(zcta_GEOID),
  .SDcols = vote_cols  # Specify which columns to include
]

fwrite(
  votes_zcta_agg3, 
  file = "Produced Datasets/election-results-zcta-2020-landcover-method.csv", 
  sep = ",",          # Use a comma as the separator
  quote = TRUE,       # Quote character strings to handle special characters
  bom = TRUE,         # Add BOM for compatibility with Excel
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)
