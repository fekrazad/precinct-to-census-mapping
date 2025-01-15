library(knitr)
library(kableExtra)
library("stargazer")
library(xtable)
library(tidyverse)

# -------------------- preparing ACS data Zip

acs_tract_path = '/Users/amir/Downloads/Data/ACSCensusTract/'

acs_dp02 = data_frame()

df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2016.DP02-Data.csv') ) %>%
  slice(-c(1)) %>%
  mutate(year = 2016, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    l_total_hh = DP02_0001E,
    l_avg_hh_size = DP02_0015E,
    
    p_married = DP02_0004PE,
    p_households_with_children = DP02_0013PE,
    p_households_with_elderly = DP02_0014PE,
    
    p_in_college = DP02_0057PE, # in college out of 3+ year in school
    
    p_civilian_veteran = DP02_0069PE, #  veteran out of 18+ civilians
    p_with_disability = DP02_0071PE, # out of Total Civilian Noninstitutionalized Population
    
    p_diff_house = DP02_0080PE, # out of pop_1yearplus
    p_diff_county = DP02_0082PE, # out of diff_house
    p_diff_state = DP02_0084PE, #out of diff_county
    p_diff_abroad = DP02_0085PE, # out of pop_1yearplus
    
    p_non_english_language = DP02_0112PE, # out of 5 year old+
    
    #p_less_than_9th_grade = DP02_0059PE, #base
    p_ninth_to_12th_no_diploma = DP02_0060PE,
    p_high_school_graduate = DP02_0061PE,
    p_some_college_no_degree = DP02_0062PE,
    p_associates_degree = DP02_0063PE,
    p_bachelors_degree = DP02_0064PE,
    p_graduate_or_professional_degree = DP02_0065PE,
    
    p_foreign_born_all = DP02_0092PE,
    p_non_citizen_fraction_of_foreign_born = DP02_0095PE,
    m_foreign_born_margin = DP02_0092PM,
    m_non_citizen_margin = DP02_0095PM,
  ) %>% 
  mutate(
    across(3:last_col(), as.double),
    p_non_citizen = round((p_non_citizen_fraction_of_foreign_born * p_foreign_born_all) / 100, 2),
    p_foreign_born_citizen = p_foreign_born_all - p_non_citizen
  )

acs_dp02 <- acs_dp02 %>% bind_rows(df)


df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2020.DP02-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2020, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    l_total_hh = DP02_0001E,
    l_avg_hh_size = DP02_0016E,
    
    p_married = DP02_0002PE,
    p_households_with_children = DP02_0014PE,
    p_households_with_elderly = DP02_0015PE,
    
    p_in_college = DP02_0058PE, # in college out of 3+ year in school
    
    p_civilian_veteran = DP02_0070PE, #  veteran out of 18+ civilians
    p_with_disability = DP02_0072PE, # out of Total Civilian Noninstitutionalized Population
    
    p_diff_house = DP02_0082PE, # out of diff_house_us_or_abroad
    p_diff_county = DP02_0084PE, # out of diff_house
    p_diff_state = DP02_0086PE, #out of diff_county
    p_diff_abroad = DP02_0087PE, # out of diff_house_us_or_abroad
    p_non_english_language = DP02_0114PE, # out of 5 year old+
    
    #p_less_than_9th_grade = DP02_0060PE, # base
    p_ninth_to_12th_no_diploma = DP02_0061PE,
    p_high_school_graduate = DP02_0062PE,
    p_some_college_no_degree = DP02_0063PE,
    p_associates_degree = DP02_0064PE,
    p_bachelors_degree = DP02_0065PE,
    p_graduate_or_professional_degree = DP02_0066PE,
    
    p_foreign_born_all = DP02_0094PE,
    p_non_citizen_fraction_of_foreign_born = DP02_0097PE,
    m_foreign_born_margin = DP02_0094PM,
    m_non_citizen_margin = DP02_0097PM
  ) %>% 
  mutate(
    across(3:last_col(), as.double),
    p_non_citizen = round((p_non_citizen_fraction_of_foreign_born * p_foreign_born_all) / 100, 2),
    p_foreign_born_citizen = p_foreign_born_all - p_non_citizen
  )

acs_dp02 <- acs_dp02 %>% bind_rows(df)



#-------------- ACS DP 03 ----------------------

acs_dp03 = data_frame()


df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2016.DP03-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2016, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    p_labor_force_total = DP03_0002PE,
    p_unemployed = DP03_0005PE,
    p_armed_forces = DP03_0006PE,
    
    # Class of Worker
    # p_private_wage_and_salary_workers = DP03_0047PE, # base
    p_government_workers = DP03_0048PE,
    p_self_employed_not_incorporated = DP03_0049PE,
    
    p_worked_from_home = DP03_0024PE,
    
    # Industry Breakdown
    # p_agriculture_forestry_fishing_hunting_mining = DP03_0033PE, #base
    p_construction = DP03_0034PE, #2
    p_manufacturing = DP03_0035PE, #2
    p_wholesale_trade = DP03_0036PE, #3
    p_retail_trade = DP03_0037PE, #3
    p_transportation_warehousing_utilities = DP03_0038PE, #2
    p_information = DP03_0039PE, #4
    p_finance_insurance_real_estate_rental_leasing = DP03_0040PE, #4
    p_professional_scientific_management_admin_waste_mgmt_services = DP03_0041PE, #4
    p_educational_services_healthcare_social_assistance = DP03_0042PE, #5
    p_arts_entertainment_recreation_accommodation_food_services = DP03_0043PE, #6
    p_other_services_except_public_administration = DP03_0044PE, #6
    p_public_administration = DP03_0045PE, #5
    
    # p_income_less_than_10k = DP03_0052PE, # base
    p_income_10k_to_15k = DP03_0053PE,
    p_income_15k_to_25k = DP03_0054PE,
    p_income_25k_to_35k = DP03_0055PE,
    p_income_35k_to_50k = DP03_0056PE,
    p_income_50k_to_75k = DP03_0057PE,
    p_income_75k_to_100k = DP03_0058PE,
    p_income_100k_to_150k = DP03_0059PE,
    p_income_150k_to_200k = DP03_0060PE,
    p_income_200k_or_more = DP03_0061PE,
    
    l_median_household_income = DP03_0062E,
    p_on_snap = DP03_0074PE,
    p_with_health_insurance = DP03_0096PE
  ) %>% mutate(across(3:last_col(), as.numeric)) 

acs_dp03 <- acs_dp03 %>% bind_rows(df)



df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2020.DP03-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2020, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    p_labor_force_total = DP03_0002PE,
    p_unemployed = DP03_0005PE,
    p_armed_forces = DP03_0006PE,
    
    # Class of Worker
    # p_private_wage_and_salary_workers = DP03_0047PE, # base
    p_government_workers = DP03_0048PE,
    p_self_employed_not_incorporated = DP03_0049PE,
    
    p_worked_from_home = DP03_0024PE,
    
    # Industry Breakdown
    # p_agriculture_forestry_fishing_hunting_mining = DP03_0033PE, #base
    p_construction = DP03_0034PE, #2
    p_manufacturing = DP03_0035PE, #2
    p_wholesale_trade = DP03_0036PE, #3
    p_retail_trade = DP03_0037PE, #3
    p_transportation_warehousing_utilities = DP03_0038PE, #2
    p_information = DP03_0039PE, #4
    p_finance_insurance_real_estate_rental_leasing = DP03_0040PE, #4
    p_professional_scientific_management_admin_waste_mgmt_services = DP03_0041PE, #4
    p_educational_services_healthcare_social_assistance = DP03_0042PE, #5
    p_arts_entertainment_recreation_accommodation_food_services = DP03_0043PE, #6
    p_other_services_except_public_administration = DP03_0044PE, #6
    p_public_administration = DP03_0045PE, #5
    
    # p_income_less_than_10k = DP03_0052PE, # base
    p_income_10k_to_15k = DP03_0053PE,
    p_income_15k_to_25k = DP03_0054PE,
    p_income_25k_to_35k = DP03_0055PE,
    p_income_35k_to_50k = DP03_0056PE,
    p_income_50k_to_75k = DP03_0057PE,
    p_income_75k_to_100k = DP03_0058PE,
    p_income_100k_to_150k = DP03_0059PE,
    p_income_150k_to_200k = DP03_0060PE,
    p_income_200k_or_more = DP03_0061PE,
    
    l_median_household_income = DP03_0062E,
    p_on_snap = DP03_0074PE,
    p_with_health_insurance = DP03_0096PE
  ) %>% mutate(across(3:last_col(), as.numeric)) 

acs_dp03 <- acs_dp03 %>% bind_rows(df)


#-------------- ACS DP 04 ----------------------


acs_dp04 = data_frame()


df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2016.DP04-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2016, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    p_owner_occupied_housing_units = DP04_0046PE,
    p_renter_occupied_housing_units = DP04_0047PE,
    l_median_home_value = DP04_0089E,
  ) %>% mutate_at(vars(3:last_col()), as.numeric)

acs_dp04 <- acs_dp04 %>% bind_rows(df)


df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2020.DP04-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2020, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    p_owner_occupied_housing_units = DP04_0046PE,
    p_renter_occupied_housing_units = DP04_0047PE,
    l_median_home_value = DP04_0089E,
  ) %>% mutate_at(vars(3:last_col()), as.numeric) 

acs_dp04 <- acs_dp04 %>% bind_rows(df)


#-------------- ACS DP 05 ----------------------


acs_dp05 = data_frame()


df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2016.DP05-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2016, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    l_total_population = DP05_0001E,
    l_voting_population = DP05_0082E,
    #p_total_18plus = DP05_0018PE,
    #p_total_65plus = DP05_0021PE,
    p_male_18plus = DP05_0023PE,
    
    # p_total_under_5 =  DP05_0004PE, # base
    l_median_age   = DP05_0017E,
    p_total_5_to_9 =   DP05_0005PE,
    p_total_10_to_14 = DP05_0006PE,
    p_total_15_to_19 = DP05_0007PE,
    p_total_20_to_24 = DP05_0008PE,
    p_total_25_to_34 = DP05_0009PE,
    p_total_35_to_44 = DP05_0010PE,
    p_total_45_to_54 = DP05_0011PE,
    p_total_55_to_59 = DP05_0012PE,
    p_total_60_to_64 = DP05_0013PE,
    p_total_65_to_74 = DP05_0014PE,
    p_total_75_to_84 = DP05_0015PE,
    p_total_85_and_over = DP05_0016PE,
    
    p_mixed_race = DP05_0030PE,
    p_white = DP05_0032PE,
    p_black = DP05_0033PE,
    p_asian = DP05_0039PE,
    p_hispanic = DP05_0066PE
  ) %>% mutate_at(vars(3:last_col()), as.numeric)

acs_dp05 <- acs_dp05 %>% bind_rows(df)



df <- read_csv( paste0(acs_tract_path, 'ACSDP5Y2020.DP05-Data.csv') ) %>%
  slice(-1) %>%
  mutate(year = 2020, geoid = str_sub(GEO_ID, 10)) %>%
  select(
    geoid,
    year,
    
    l_total_population = DP05_0001E,
    l_voting_population = DP05_0087E,
    #p_total_18plus = DP05_0021PE,
    #p_total_65plus = DP05_0024PE,
    p_male_18plus = DP05_0026PE,
    
    #p_total_under_5 =  DP05_0005PE, # base
    l_median_age   = DP05_0018E,
    p_total_5_to_9 =   DP05_0006PE,
    p_total_10_to_14 = DP05_0007PE,
    p_total_15_to_19 = DP05_0008PE,
    p_total_20_to_24 = DP05_0009PE,
    p_total_25_to_34 = DP05_0010PE,
    p_total_35_to_44 = DP05_0011PE,
    p_total_45_to_54 = DP05_0012PE,
    p_total_55_to_59 = DP05_0013PE,
    p_total_60_to_64 = DP05_0014PE,
    p_total_65_to_74 = DP05_0015PE,
    p_total_75_to_84 = DP05_0016PE,
    p_total_85_and_over = DP05_0017PE,
    
    p_mixed_race = DP05_0035PE,
    p_white = DP05_0037PE,
    p_black = DP05_0038PE,
    p_asian = DP05_0044PE,
    p_hispanic = DP05_0071PE
  ) %>% mutate_at(vars(3:last_col()), as.numeric) 

acs_dp05 <- acs_dp05 %>% bind_rows(df)



acs <- acs_dp02 %>% 
  full_join(acs_dp03, by = c("geoid", "year")) %>% 
  full_join(acs_dp04, by = c("geoid", "year")) %>%
  full_join(acs_dp05, by = c("geoid", "year"))

#acs <- acs %>% filter(l_total_population > 0)

write_rds(acs, 'acs-data-profiles.rds')

acs <- read_rds('acs-data-profiles.rds')



########## vote data census tract ----------------------------

votes_1 <- read_csv("Produced Datasets/election-results-census-tracts-2020-areal-method.csv") %>%
  mutate(
    votes_total1 = select(., starts_with("G20PRE")) %>% rowSums(na.rm = TRUE)
  ) %>% select(tract_GEOID, votes_total1)

votes_2 <- read_csv("Produced Datasets/election-results-census-tracts-2020-imperviousness-method.csv") %>%
  mutate(
    votes_total2 = select(., starts_with("G20PRE")) %>% rowSums(na.rm = TRUE)
  ) %>% select(tract_GEOID, votes_total2)

votes_3 <- read_csv("Produced Datasets/election-results-census-tracts-2020-landcover-method.csv") %>%
  mutate(
    votes_total3 = select(., starts_with("G20PRE")) %>% rowSums(na.rm = TRUE)
  ) %>% select(tract_GEOID, tract_state_fp, tract_county_fp, votes_total3, tract_population, tract_land_area)


votes <- votes_1 %>% inner_join(votes_2, by="tract_GEOID") %>% inner_join(votes_3, by="tract_GEOID")


###- --------------- merging



df_analysis <- votes %>% inner_join(acs %>% filter(year == 2020), by = c("tract_GEOID" = "geoid")) 

df_analysis_complete <- df_analysis %>% mutate(
  turnout1 = votes_total1 / l_voting_population * 100,
  turnout2 = votes_total2 / l_voting_population * 100,
  turnout3 = votes_total3 / l_voting_population * 100,
  income = l_median_household_income / 1000,
  county = str_c(tract_state_fp, ' ', tract_county_fp),
  minority = 100 - p_white,
  college = p_bachelors_degree + p_graduate_or_professional_degree,
  age = l_median_age,
  pop_density = tract_population / tract_land_area,
  ) %>% filter(l_voting_population >= 50) %>% select(
    county,
  turnout1, turnout2, turnout3, 
  age, minority, income, college, pop_density) %>% 
  drop_na() %>% mutate(
    density_pentile = ntile(pop_density, 5)
  )


reg_method1_all <- lm(turnout1 ~ age + income + college + minority , data = df_analysis_complete)
reg_method2_all <- lm(turnout2 ~ age + income + college + minority, data = df_analysis_complete)
reg_method3_all <- lm(turnout3 ~ age + income + college + minority, data = df_analysis_complete)

reg_method1_rural <- lm(turnout1 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 1))
reg_method2_rural <- lm(turnout2 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 1))
reg_method3_rural <- lm(turnout3 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 1))

reg_method1_metro <- lm(turnout1 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 5))
reg_method2_metro <- lm(turnout2 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 5))
reg_method3_metro <- lm(turnout3 ~ age + income + college + minority, data = df_analysis_complete %>% filter(density_pentile == 5))

# Create a function to extract estimates and standard errors, ignoring the intercept
extract_coefficients <- function(model, subset_name, method_name) {
  coefs <- summary(model)$coefficients
  # Exclude the intercept
  coefs <- coefs[rownames(coefs) != "(Intercept)", ]
  estimates <- coefs[, "Estimate"]
  sds <- coefs[, "Std. Error"]
  variables <- rownames(coefs)
  
  p_values <- coefs[, "Pr(>|t|)"]
  
  # Add stars based on p-values
  stars <- ifelse(p_values < 0.01, "***", 
                  ifelse(p_values < 0.05, "**", 
                         ifelse(p_values < 0.1, "*", "")))
  
  estimates <- paste0(format(round(estimates, 3), scientific = FALSE), stars)
  
  # Format standard errors with parentheses, avoiding scientific notation
  sds <- paste0("(", format(round(sds, 3), scientific = FALSE), ")")
  
  # Create a data frame for the model
  data.frame(
    Subset = subset_name,
    X = rep(variables, each = 2),
    Turnout = c(rep(method_name, length(variables)), rep(method_name, length(variables))),
    ValueType = rep(c("Estimate", "Std. Error"), times = length(variables)),
    Value = as.vector(rbind(estimates, sds))
  )
}

# Extract coefficients and standard errors for all models
results_all <- rbind(
  extract_coefficients(reg_method1_all, "All", "turnout1"),
  extract_coefficients(reg_method2_all, "All", "turnout2"),
  extract_coefficients(reg_method3_all, "All", "turnout3")
)

results_rural <- rbind(
  extract_coefficients(reg_method1_rural, "Rural", "turnout1"),
  extract_coefficients(reg_method2_rural, "Rural", "turnout2"),
  extract_coefficients(reg_method3_rural, "Rural", "turnout3")
)

results_metro <- rbind(
  extract_coefficients(reg_method1_metro, "Metro", "turnout1"),
  extract_coefficients(reg_method2_metro, "Metro", "turnout2"),
  extract_coefficients(reg_method3_metro, "Metro", "turnout3")
)

# Combine all results
final_results <- rbind(results_all, results_rural, results_metro)

# Rearrange the columns as per your desired format
final_results <- final_results %>%
  pivot_wider(names_from = Turnout, values_from = Value) 

# Display the results
View(final_results)


final_results %>%
  kable("latex", booktabs = TRUE, linesep = "", caption = "Regression Results") %>%
  kable_styling(latex_options = "hold_position") %>%
  collapse_rows(columns = c(1, 2), valign = "middle", latex_hline = "major") 



  
  
  # Function to extract t-statistics and compute differences
  extract_tstats <- function(model1, model2, model3, subset_name) {
    # Extract t-statistics
    t1 <- summary(model1)$coefficients[, "t value"]
    t2 <- summary(model2)$coefficients[, "t value"]
    t3 <- summary(model3)$coefficients[, "t value"]
    
    # Exclude intercept
    variable_names <- rownames(summary(model1)$coefficients)[rownames(summary(model1)$coefficients) != "(Intercept)"]
    t1 <- t1[variable_names]
    t2 <- t2[variable_names]
    t3 <- t3[variable_names]
    
    # Calculate percentage differences
    diff_1_3 <- abs(t1 - t3) / abs(t3) * 100
    diff_2_3 <- abs(t2 - t3) / abs(t3) * 100
    
    # Create the data frame
    data.frame(
      Subset = subset_name,
      X = variable_names,
      VarType = 't-stat',
      turnout1 = round(t1, 2),
      turnout2 = round(t2, 2),
      turnout3 = round(t3, 2),
      `Diff 1 and 3` = paste0( round(diff_1_3, 2), '%') ,
      `Diff 2 and 3` = paste0( round(diff_2_3, 2), '%')
    )
  }
  
  # Generate data frames for all, rural, and metro subsets
  results_all <- extract_tstats(reg_method1_all, reg_method2_all, reg_method3_all, "All")
  results_metro <- extract_tstats(reg_method1_metro, reg_method2_metro, reg_method3_metro, "Metro")
  results_rural <- extract_tstats(reg_method1_rural, reg_method2_rural, reg_method3_rural, "Rural")
  
  # Combine all results into a single data frame
  final_results_tstat <- rbind(results_all, results_rural,results_metro)
  
  rownames(final_results_tstat) <- NULL
  
  final_results_tstat %>%
    kable("latex", booktabs = TRUE, linesep = "", caption = "Regression Results") %>%
    kable_styling(latex_options = "hold_position") %>%
    collapse_rows(columns = c(1, 2), valign = "middle", latex_hline = "major") 
  
  
  




  
  
  
  









# Extract coefficients
coef_se <- function(model) {
  coefs <- summary(model)$coefficients
  paste0(formatC(coefs[, 1], format = "f", digits = 4), " (", 
         formatC(coefs[, 2], format = "f", digits = 4), ")")
}

# Extract t-values
t_values <- function(model) {
  summary(model)$coefficients[, 3]
}

# Prepare rural and metro results
results_rural <- data.frame(
  Variable = rownames(summary(reg_method1_rural)$coefficients),
  Method1 = coef_se(reg_method1_rural),
  Method2 = coef_se(reg_method2_rural),
  Method3 = coef_se(reg_method3_rural),
  Percent_Difference13 = formatC(abs(100 * 
                                       (t_values(reg_method3_rural) - t_values(reg_method1_rural)) / 
                                       t_values(reg_method1_rural)), format = "f", digits = 2),
  Percent_Difference23 = formatC(abs(100 * 
                                       (t_values(reg_method3_rural) - t_values(reg_method2_rural)) / 
                                       t_values(reg_method2_rural)), format = "f", digits = 2),
  Area = "Rural"
)

results_metro <- data.frame(
  Variable = rownames(summary(reg_method1_metro)$coefficients),
  Method1 = coef_se(reg_method1_metro),
  Method2 = coef_se(reg_method2_metro),
  Method3 = coef_se(reg_method3_metro),
  Percent_Difference13 = formatC(abs(100 * 
                                       (t_values(reg_method3_metro) - t_values(reg_method1_metro)) / 
                                       t_values(reg_method1_metro)), format = "f", digits = 2),
  Percent_Difference23 = formatC(abs(100 * 
                                       (t_values(reg_method3_metro) - t_values(reg_method2_metro)) / 
                                       t_values(reg_method2_metro)), format = "f", digits = 2),
  Area = "Metro"
)

# Combine the results into a single dataframe
combined_results <- rbind(results_rural, results_metro)

# Ensure column names are clear and formatted
colnames(combined_results) <- c(
  "Variable", 
  "Method 1 (Estimate, S.E.)", 
  "Method 2 (Estimate, S.E.)", 
  "Method 3 (Estimate, S.E.)", 
  "% Difference (t-value 1 vs. 3)", 
  "% Difference (t-value 2 vs. 3)", 
  "Area"
)
