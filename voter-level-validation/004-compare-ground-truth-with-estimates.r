library(tidyverse)

our_data_areal <- read_csv("REVISE/produced-data/election-results-census-tracts-2020-areal-method.csv") %>%
  filter(tract_state_fp == 37) %>%
  mutate(total_votes_areal = rowSums(across(starts_with('G20PRE')), na.rm = T)) %>%
  select(tract_GEOID, total_votes_areal, tract_population)


our_data_imperv <- read_csv("REVISE/produced-data/election-results-census-tracts-2020-imperviousness-method.csv") %>%
  filter(tract_state_fp == 37) %>%
  mutate(total_votes_imperv = rowSums(across(starts_with('G20PRE')), na.rm = T)) %>%
  select(tract_GEOID, total_votes_imperv)

our_data_landcover <- read_csv("REVISE/produced-data/election-results-census-tracts-2020-landcover-method.csv") %>%
  filter(tract_state_fp == 37) %>%
  mutate(total_votes_landcover = rowSums(across(starts_with('G20PRE')), na.rm = T)) %>%
  select(tract_GEOID, total_votes_landcover)


our_data <- our_data_areal %>% inner_join(our_data_imperv) %>% inner_join(our_data_landcover)


gt <- read_rds("REVISE/validation-data/nc-ground-truth-all.rds")


df <- our_data %>% left_join(gt, by=c("tract_GEOID" = "GEOID"))


s1 <- sum(df$total_votes_areal, na.rm = T)
s2 <- sum(df$total_votes_gt, na.rm = T)
df <- df %>% mutate(
  total_votes_areal = total_votes_areal * s2 / s1,
  total_votes_imperv = total_votes_imperv * s2 / s1,
  total_votes_landcover = total_votes_landcover * s2 / s1
)


metrics <- df %>%
  summarise(
    across(
      c(total_votes_areal, total_votes_imperv, total_votes_landcover),
      list(
        MAE  = ~ mean(abs(. - total_votes_gt),               na.rm = TRUE),
        MSE  = ~ sqrt( mean((. - total_votes_gt)^2,                na.rm = TRUE) ),
        MAPE = ~ mean(2*abs(. - total_votes_gt)/((.+total_votes_gt)), na.rm = TRUE) * 100
      ),
      .names = "{.col}_{.fn}"
    )
  )


reshaped_metrics <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE"),
  Areal = round(c(metrics$total_votes_areal_MAE, metrics$total_votes_areal_MSE, metrics$total_votes_areal_MAPE), 2),
  Imperviousness = round(c(metrics$total_votes_imperv_MAE, metrics$total_votes_imperv_MSE, metrics$total_votes_imperv_MAPE), 2),
  RLCR = round(c(metrics$total_votes_landcover_MAE, metrics$total_votes_landcover_MSE, metrics$total_votes_landcover_MAPE), 2)
)

reshaped_metrics %>%
  kable("latex", booktabs = TRUE, linesep = "", caption = "Compare") %>%
  kable_styling(latex_options = "hold_position") 


