library(readr)
library(fs)
library(tidyverse)

## Voter registration Snapshot is a very large file so we process it in chunks

# ---- Settings ----
file_path <- "/Users/amir/Downloads/VR_Snapshot_20201103.txt"
output_dir <- "/Users/amir/Downloads/split_voter_file/"
chunk_size <- 100000  # number of rows per chunk

# ---- Create output dir ----
dir_create(output_dir)


# ---- Open connection ----
con <- file(file_path, open = "r", encoding = "UTF-16LE")

# ---- Read header only ----
header <- readLines(con, n = 1)  #header line

chunk_num <- 1
repeat {
  # Read next chunk
  lines <- readLines(con, n = chunk_size)
  
  if (length(lines) == 0) break
  
  # Combine header and chunk lines
  out_lines <- c(header, lines)
  
  # Write to file
  chunk_file <- file.path(output_dir, sprintf("split_chunk_%03d.tsv", chunk_num))
  write_lines(out_lines, chunk_file)
  
  cat("Wrote", chunk_file, "\n")
  chunk_num <- chunk_num + 1
}

close(con)


chunk_path <- "/Users/amir/Downloads/split_voter_file/split_chunk_100.tsv"

df <- read_tsv(
  file = chunk_path,
  show_col_types = FALSE
)



#Source: https://www.ncsbe.gov/results-data/voter-history-data
# Statewide Voter History (ZIP) https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvhis_Statewide.zip

voting_hist <- read_delim("/Users/amir/Downloads/ncvhis_Statewide.txt", delim="\t")

voting_hist_2020 <- voting_hist %>% filter(election_desc == "11/03/2020 GENERAL")

temp <- voting_hist_2020 %>% group_by(ncid) %>% mutate(n=n()) %>% filter(n > 1) %>% select(ncid, voter_reg_num, county_id, voted_county_id, n)

voting_hist_2020_unique <- voting_hist_2020 %>% distinct(ncid, .keep_all = TRUE)

write_rds(voting_hist_2020_unique, "REVISE/validation-data/nc-2020-votes.rds")

voting_hist_2020_unique <- read_rds("REVISE/validation-data/nc-2020-votes.rds")

############################# voter file for the 2020 election


input_dir <- "/Users/amir/Downloads/split_voter_file/"
output_dir <- "REVISE/nc_voter_file_election_2020/"

# ---- Get list of input files ----
files <- dir_ls(input_dir, regexp = "\\.tsv$")

#dfn <- tibble(chunk = c(), joined = c(), joined2 = c(), joined3 = c())

# ---- Process each file ----
for (file in files) {
  cat("Processing", file, "\n")
  
  # Read chunk
  voter_chunk <- read_tsv(file, show_col_types = FALSE, col_types = cols(.default = "c"))
  #%>%
  #  filter(voter_status_desc!='REMOVED', voter_status_desc!='DENIED') %>%
  #  arrange(desc(voter_status_desc=='ACTIVE'), desc(voter_status_desc=='TEMPORARY')) %>%
  #  distinct(ncid, .keep_all = TRUE)
  
  # Join
  joined <- inner_join(voter_chunk, voting_hist_2020_unique, by = "ncid", suffix = c("", "_hist"))
  #joined2 <- inner_join(voter_chunk, voting_hist, by = c("voter_reg_num"), suffix = c("", "_hist"))
  #joined3 <- inner_join(voter_chunk, voting_hist, by = c("voter_reg_num", "ncid"), suffix = c("", "_hist"))
  
  #temp <- tibble(chunk = nrow(voter_chunk), joined = nrow(joined), joined2 = nrow(joined2), joined3 = nrow(joined3))
  #dfn <- dfn %>% bind_rows(temp)
  
  
  # Output filename
  out_file <- path_ext_set(path(output_dir, path_file(file)), "rds")
  
  # Write joined file
  write_rds(joined, out_file)
  
  cat("→ Saved to", out_file, "\n")
  #break
}


#dfn %>% summarise( across(everything(), sum) )


# now load all 
input_dir <- "REVISE/nc_voter_file_election_2020/"

to_all_character <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

files <- dir_ls(input_dir, regexp = "\\.rds$")

v <- files %>%
  map(read_rds) %>%
  map(to_all_character) %>%
  bind_rows()

#table(v$voter_status_desc)

v2 <- v %>%
  mutate(status_priority = case_when(
    voter_status_desc == "ACTIVE" ~ 1,
    voter_status_desc == "INACTIVE" ~ 2,
    voter_status_desc == "TEMPORARY" ~ 3,
    voter_status_desc == "DECLINED" ~ 4,
    TRUE ~ 5
  ))

v3 <- v2 %>%
  group_by(ncid) %>%
  slice_min(status_priority, n=1, with_ties = FALSE)

v4 <- v3 %>% filter(status_priority < 4)

#temp <- v %>% group_by(ncid) %>% mutate(n=n())
#max(temp$n)

write_rds(v4, "REVISE/validation-data/nc-2020-voter-addrs.rds")

v <- read_rds("REVISE/validation-data/nc-2020-voter-addrs.rds")
  
v2 <- v %>%
  mutate(
    # Combine address components, handling missing values and trimming spaces
    addr = trimws(paste(
      coalesce(house_num, ""),
      coalesce(half_code, ""),
      coalesce(street_dir, ""),
      coalesce(street_name, ""),
      coalesce(street_type_cd, ""),
      coalesce(street_sufx_cd, ""),
      sep = " "
    )),
    # Replace multiple spaces with single space and remove trailing/leading spaces
    addr = gsub("\\s+", " ", addr)
  ) %>%
  select(
    ncid,
    addr,
    res_city_desc, state_cd, zip_code,
    county_id, county_desc, pct_label, pct_description
  ) 


write_csv(v2, "REVISE/validation-data/voter-addresses-corrected.csv", na="")

