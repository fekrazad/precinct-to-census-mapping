library(tidyverse)
library(nnls)


run_nnls_for_cluster <- function(data) {
  Y <- data$bg_population
  
  # Define the required columns
  required_columns <- c("lc_21", "lc_22", "lc_23", "lc_24", "lc_52", 
                        "lc_81", "lc_82", "lc_41", "lc_42", "lc_43", 
                        "lc_31", "lc_71")
  
  # Construct the matrix
  X <- as.matrix(data[, required_columns])
  
  # Run NNLS
  model <- nnls(X, Y)
  
  # Return coefficients
  return(coef(model))
}

# Directories
bg_folder <- '/Users/amir/Downloads/Data/clustered-tracts/'
acs_bg <- read_csv('/Users/amir/Downloads/Data/ACSBlockGroup/ACSDT5Y2016.B01003-Data.csv') %>%
  slice(-1) %>%
  mutate(GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    GEOID,
    bg_population = B01003_001E
  ) %>%
  mutate(
    bg_population = as.numeric(bg_population)
  )

#temp <- read_csv(str_c(bg_folder, "dc-cluster-2016.csv"), col_types = cols(GEOID = col_character()))
#temp$lc_82 <- 0
#write_csv(temp, str_c(bg_folder, "dc-cluster-2016.csv"))


# States
states <- c(
  "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "ia", "id", "il", "in", "ks", "ky", "la", 
  "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", 
  "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy", "dc"
)

#states <- c('dc')


bg_lc_estimates <- data.frame()


# Process each state
for (state in states) {
  
  print(state)

    # Load data for the state
    bg <- read_csv(str_c(bg_folder, state, "-cluster-2016.csv"), col_types = cols(GEOID = col_character(), cluster_id = col_character()))

    # Merge with ACS data
    bg_pop <- bg %>% inner_join(acs_bg, by = "GEOID")
    
    
    results <- bg_pop %>%
      group_by(cluster_id) %>%
      do({
        coef_df <- as.data.frame(t(run_nnls_for_cluster(.)))
        coef_df$cluster_id <- unique(.$cluster_id)
        coef_df
      })
    results$cluster_id <- as.character(results$cluster_id)
    
    colnames(results) <- c("coef_lc_21", "coef_lc_22", "coef_lc_23", "coef_lc_24", "coef_lc_52", 
                           "coef_lc_81", "coef_lc_82", "coef_lc_41", "coef_lc_42", "coef_lc_43", 
                           "coef_lc_31", "coef_lc_71", "cluster_id")
    
  
    bg_lc_estimates <- bind_rows(bg_lc_estimates, bg %>% select(GEOID, cluster_id) %>% inner_join(results, by="cluster_id") )
    print(nrow(bg))
    print(nrow(bg_lc_estimates))
    
}




#bg_lc_estimates <- bg_lc_estimates %>% rename(bg_GEOID = bg_geoid)

write_csv(bg_lc_estimates, "block-group-landcover-estimates-2016.csv")



