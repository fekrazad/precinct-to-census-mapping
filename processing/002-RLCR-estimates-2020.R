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
block_folder <- '/Users/amir/Downloads/Data/blocks/processed/'
acs_bg <- read_csv('/Users/amir/Downloads/Data/ACSBlockGroup/ACSDT5Y2020.B01003-Data.csv') %>%
  slice(-1) %>%
  mutate(GEOID = str_sub(GEO_ID, 10)) %>%
  select(
    GEOID,
    bg_population = B01003_001E
  ) %>%
  mutate(
    bg_population = as.numeric(bg_population)
  )

#temp <- read_csv(str_c(bg_folder, "dc-cluster-2020.csv"), col_types = cols(GEOID = col_character()))
#temp$lc_82 <- 0
#write_csv(temp, str_c(bg_folder, "dc-cluster-2020.csv"))
#temp <- read_csv(str_c(block_folder, "dc-blocks.csv"), col_types = cols(GEOID20 = col_character()))
#temp$lc_82 <- 0
#write_csv(temp, str_c(block_folder, "dc-blocks.csv"))

# States
states <- c(
  "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "ia", "id", "il", "in", "ks", "ky", "la", 
  "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", 
  "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy", "dc"
)

#states <- c('dc')

# Dataframe to store results
correlations <- data.frame(
  state = character(),
  r_area = numeric(),
  r_habitability = numeric(),
  r_landcover = numeric(),
  stringsAsFactors = FALSE
)

bg_lc_estimates <- data.frame()

total_block_and_group2 <- data.frame()


# Process each state
for (state in states) {
  
  print(state)

    # Load data for the state
    bg <- read_csv(str_c(bg_folder, state, "-cluster-2020.csv"), col_types = cols(GEOID = col_character(), cluster_id = col_character()))
    block <- read_csv(str_c(block_folder, state, "-blocks.csv"), col_types = cols(GEOID20 = col_character())) %>%
      mutate(GEOID = str_sub(GEOID20, 1, -4), POP20) %>% select(GEOID20, GEOID, POP20, ALAND20, AWATER20, habitability_sum, starts_with('lc_'))
    
    # Merge with ACS data
    bg_pop <- bg %>% inner_join(acs_bg, by = "GEOID")
    
    # Calculate correlations
    r_area <- cor(block$POP20, block$ALAND20 + block$AWATER20, use = "complete.obs")
    r_habitability <- cor(block$POP20, block$habitability_sum, use = "complete.obs")
    
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
    
    block_and_group <- block %>% inner_join(bg_pop %>% select(GEOID, cluster_id), by = "GEOID")
    block_and_group2 <- block_and_group %>% inner_join(results, by = "cluster_id")
    
    block_and_group2 <- block_and_group2 %>%
      mutate(predicted_population = 
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
               coef_lc_71 * lc_71) 
    
    r_landcover <- cor(block_and_group2$POP20, block_and_group2$predicted_population, use = "complete.obs")
    
    # Append results to dataframe
    correlations <- rbind(correlations, data.frame(
      state = state,
      r_area = r_area,
      r_habitability = r_habitability,
      r_landcover = r_landcover
    ))
    
    total_block_and_group2 <- bind_rows(total_block_and_group2, block_and_group2)
    
    bg_lc_estimates <- bind_rows(bg_lc_estimates, bg %>% select(GEOID, cluster_id) %>% inner_join(results, by="cluster_id") )
    print(nrow(bg))
    print(nrow(bg_lc_estimates))
    
}


total_r_area <- cor(total_block_and_group2$POP20, total_block_and_group2$ALAND20 + total_block_and_group2$AWATER20, use = "complete.obs")
total_r_habitability <- cor(total_block_and_group2$POP20, total_block_and_group2$habitability_sum, use = "complete.obs")
total_r_landcover <- cor(total_block_and_group2$POP20, total_block_and_group2$predicted_population, use = "complete.obs")

# Append total correlations to the results dataframe
correlations <- rbind(correlations, data.frame(
  state = "total",
  r_area = total_r_area,
  r_habitability = total_r_habitability,
  r_landcover = total_r_landcover
))


fixed_eps <- 0.0

total_block_and_group3 <- total_block_and_group2 %>% mutate(
  w1 = ALAND20 + AWATER20 ,
  w2 = habitability_sum,
  w3 = predicted_population,
  pop = POP20
)


block_sample <- total_block_and_group3 %>% 
  group_by(GEOID) %>% mutate(
    w1total = sum(w1),
    w2total = sum(w2),
    w3total = sum(w3),
    poptotal = sum(pop),
    
    w1r = w1 / w1total,
    w2r = ifelse(w2total > 0, w2 / w2total, w1r),
    w3r = ifelse(w3total > 0, w3 / w3total, w2r),
    
    w1pop = w1r * poptotal,
    w2pop = w2r * poptotal,
    w3pop = w3r * poptotal
  ) %>% ungroup()

block_sample_err <- block_sample %>% mutate(
  err1 = w1pop - pop,
  err2 = w2pop - pop,
  err3 = w3pop - pop
  ) %>%
  summarise(
    mae1 = mean(abs(err1)),
    mae2 = mean(abs(err2)),
    mae3 = mean(abs(err3)),
    
    mse1 = mean((err1)^2),
    mse2 = mean((err2)^2),
    mse3 = mean((err3)^2),
    
    mape1 = mean(abs(err1/(pop+1))),
    mape2 = mean(abs(err2/(pop+1))),
    mape3 = mean(abs(err3/(pop+1))),
  )

reshaped_df <- data.frame(
  Metric = c("MAE", "MSE", "MAPE"),
  Areal = round(c(block_sample_err$mae1, block_sample_err$mse1, block_sample_err$mape1), 2),
  Imperviousness = round(c(block_sample_err$mae2, block_sample_err$mse2, block_sample_err$mape2), 2),
  RLCR = round(c(block_sample_err$mae3, block_sample_err$mse3, block_sample_err$mape3), 2)
)

reshaped_df %>%
  kable("latex", booktabs = TRUE, linesep = "", caption = "Compare") %>%
  kable_styling(latex_options = "hold_position") 




# Save results
write_csv(correlations, "state_correlations_2020.csv")

#bg_lc_estimates <- bg_lc_estimates %>% rename(bg_GEOID = bg_geoid)
write_csv(bg_lc_estimates, "block-group-landcover-estimates-2020.csv")



#comparison graph

correlations <- read_csv("state_correlations.csv")


correlations_long <- correlations %>%
  pivot_longer(
    cols = c(r_area, r_habitability, r_landcover),
    names_to = "correlation_type",
    values_to = "value"
  )


correlations_long <- correlations_long %>%
  mutate(
    state = ifelse(state == "total", "Total", toupper(state))
  ) %>%
  group_by(state) %>%
  mutate(r_landcover = ifelse(state == "Total", NA, value[correlation_type == "r_landcover"])) %>%  # Extract landcover correlation
  ungroup() %>%
  mutate(state = factor(state, levels = c(setdiff(state[order(r_landcover)], "Total"), "Total") ))

# Plot with transformed and sorted state labels
ggplot(correlations_long, aes(y = state, x = value, fill = correlation_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  # Adjust bar width
  scale_fill_manual(
    values = c("r_area" = "#d00000", "r_habitability" = "#ffba08", "r_landcover" = "#3f88c5"),
    labels = c("Areal", "Impreviousness", "RLCR")
  ) +
  labs(
    #title = "Correlations for Each State (Total First, Sorted by Landcover)",
    y = "State",
    x = "Correlation Value",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),           # Adjust state text size
    axis.text.x = element_text(size = 10),           # Adjust value text size
    legend.position = "top",
    panel.grid.major.y = element_blank(),            # Reduce horizontal gridlines
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(1.5, "lines")             # Add spacing between states
  )


