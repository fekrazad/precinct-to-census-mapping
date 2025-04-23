library(fs)
library(purrr)
library(dplyr)
library(tidyverse)

input_dir <- "/Users/amir/Dropbox/Research/Precinct-Census Match/REVISE/validation-data/geocoded/"

# 1. your batch files
files <- dir_ls(input_dir, regexp = "batch_[0-9]+\\.csv$")

# 2. the 8 column names you expect
col_names <- c(
  "ncid",
  "input_address",
  "match_status",
  "match_type",
  "matched_address",
  "coordinates",
  "geoid",
  "side"
)

# 3. read & bind, padding short rows with NA
g <- map_dfr(files, function(f) {
  utils::read.csv(
    file        = f,
    header      = FALSE,              # no header row in these files
    stringsAsFactors = FALSE,         # keep everything character
    col.names   = col_names,          # force these 8 names
    colClasses  = "character",        # read every column as character
    fill        = TRUE                # pad short rows with NA
  ) %>%
    mutate(source = basename(f))
})


input_dir2 <- "/Users/amir/Dropbox/Research/Precinct-Census Match/REVISE/validation-data/geocoded/additional/"
files2 <- dir_ls(input_dir2, regexp = "batch_[0-9]+\\.csv$")

# 3. read & bind, padding short rows with NA
g2 <- map_dfr(files2, function(f) {
  utils::read.csv(
    file        = f,
    header      = FALSE,              # no header row in these files
    stringsAsFactors = FALSE,         # keep everything character
    col.names   = col_names,          # force these 8 names
    colClasses  = "character",        # read every column as character
    fill        = TRUE                # pad short rows with NA
  ) %>%
    mutate(source = basename(f))
})

g_updated <- g %>%
  anti_join(g2, by = "ncid") %>%  # Keep rows in g that don't match g2's ncid
  bind_rows(g2)     


v  <- read_csv("REVISE/validation-data/voter-addresses-corrected.csv") 

# geocoded voters
gv <- g_updated %>% inner_join(v, by = c("ncid"))

gv %>%
  count(match_status, name = "count") %>%
  mutate(
    pct = round(count / sum(count) * 100, 2)
  ) %>%
  arrange(desc(pct))

#match_status   count   pct
#1        Match 5311350 96.19
#2     No_Match  187587  3.40
#3          Tie   22766  0.41

gv2 <- gv %>% separate(
  col   = coordinates,
  into  = c("x", "y"),
  sep   = ",",
  convert = TRUE
)


write_rds(gv2, "REVISE/validation-data/geocoded.rds")


######### analysis
library(sf)


gv <- read_rds("REVISE/validation-data/geocoded.rds")
gv2 <- gv %>% filter(match_status == 'Match')

nc_tract_file <- '/Users/amir/Downloads/Data/tracts 2020/nc_2020_tract.zip'
nc_precinct_file <- '/Users/amir/Downloads/Data/precincts/Elections 2020/nc_2020.zip'

tract_layer    <- 'tl_2020_37_tract.shp'
precinct_layer <- 'nc_2020.shp'

nc_tract <- st_read(
  dsn = paste0("/vsizip/", normalizePath(nc_tract_file), "/", tract_layer),
  quiet = TRUE
)
nc_tract <- st_transform(nc_tract, crs = 4269)


nc_precincts <- st_read(
  dsn = paste0("/vsizip/", normalizePath(nc_precinct_file), "/", precinct_layer),
  quiet = TRUE
) %>% mutate(
  precinct_id = paste(COUNTY_ID, PREC_ID, sep = "_"), 
  precinct_unique_id = str_c('37','_', row_number()),
  total_votes = rowSums(across(starts_with('G20PRE')), na.rm = T)
) %>% select(-starts_with('G20'))
nc_precincts <- st_transform(nc_precincts, crs = 4269)


gv_sf <- st_as_sf(gv2 , coords = c("x", "y"), crs = 4269, remove = FALSE)
gv_sf <- gv_sf %>% mutate(precinct_id = paste(county_id, pct_label, sep = "_"))


voter_precinct <- st_join(gv_sf, nc_precincts, join = st_within)

voter_precinct_no_geom <- voter_precinct %>% st_drop_geometry() 

voter_precinct_count_all <- voter_precinct_no_geom %>% group_by(precinct_unique_id) %>% summarise(total_votes = n())
sum(voter_precinct_count_all$total_votes)

write_rds(voter_precinct_count_all, "REVISE/validation-data/nc-voter-per-precinct-all.rds")

voter_precinct_count_cosistent <- voter_precinct_no_geom %>% filter(precinct_id.x == precinct_id.y) %>%
  group_by(precinct_unique_id) %>% summarise(total_votes = n())
sum(voter_precinct_count_cosistent$total_votes)

write_rds(voter_precinct_count_all, "REVISE/validation-data/nc-voter-per-precinct-consistent.rds")


voter_tract <- st_join(voter_precinct, nc_tract, join = st_within)

voter_tract_no_geom <- voter_tract %>% st_drop_geometry()

vote_tract_count_all <- voter_tract_no_geom %>% group_by(GEOID) %>% summarise(total_votes_gt = n()) %>% drop_na()
write_rds(vote_tract_count_all, "REVISE/validation-data/nc-ground-truth-all.rds")

temp <- voter_precinct_no_geom %>% filter(is.na(precinct_id.y)) %>% nrow()

vote_tract_count_consistent <- voter_tract_no_geom %>% filter(precinct_id.x==precinct_id.y) %>% 
  group_by(GEOID) %>% summarise(total_votes_gt = n())

write_rds(vote_tract_count_consistent, "REVISE/validation-data/nc-ground-truth-consistent.rds")

