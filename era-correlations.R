# Purpose -------------------------------------------------------------------------------------
# This is the analysis code for the correlation plots shown in HIP's announcement blogpost 
# of the county-level ERA assistance dataset.

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(scales)
library(skimr)
library(tidycensus)
library(sf)

# Project data path
project_path = this.path::this.dir()

theme_set(theme_light())

# Get data -----------------------------------------------------------------------------------

# ERA assistance county-total dataset
era <- 
  read_csv(str_c(project_path, 
                 "aggregated_data/county_total_aggregated_2025-04-14.csv", 
                 sep = "/")) %>% 
  # Filter suppressed counties
  # filter: removed 269 rows (11%), 2,095 rows remaining
  filter(unique_assisted_addresses != -99999) %>% 
  rename(geoid = county_geoid_coalesced)

# ACS tenure by race/ethnicity
renter_race_ethnicity <- 
  get_acs(geography = "county",
          variables = c("B25003_003", "B25003H_003"),
          year = 2023,
          survey = "acs5",
          geometry = TRUE) %>% 
  clean_names() %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(renter_households_of_color = B25003_003 - B25003H_003) %>% 
  mutate(percent_renter_households_of_color = renter_households_of_color / B25003_003)

# ACS severe cost burden
renter_severe_cost_burden <- 
  get_acs(geography = "county",
          variables = c("B25140_010", "B25140_012"),
          year = 2023,
          survey = "acs5",
          geometry = TRUE) %>% 
  clean_names() %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(percent_renter_severe_cost_burden = B25140_012 / B25140_010)

# Join data -----------------------------------------------------------------------------------

joined <- era %>% 
  inner_join(renter_race_ethnicity %>% 
              select(geoid, name, percent_renter_households_of_color),
            by = "geoid") %>% 
  inner_join(renter_severe_cost_burden %>% 
               st_drop_geometry() %>% 
               select(geoid, percent_renter_severe_cost_burden),
             by = "geoid")

# Plots: race and ethnicity --------------------------------------------------------------------

plot_race_dollar <- joined %>% 
  ggplot(aes(x = log(sum_assistance_amount), y = percent_renter_households_of_color)) +
  geom_point(alpha = 0.3, color = "#1f2859ff") +
  geom_smooth(color = "#0b9444ff")

plot_race_dollar

plot_race_addresses <- joined %>% 
  ggplot(aes(x = log(unique_assisted_addresses), y = percent_renter_households_of_color)) +
  geom_point(alpha = 0.3, color = "#1f2859ff") +
  geom_smooth(color = "#0b9444ff")

plot_race_addresses

# Plots: severe cost burden --------------------------------------------------------------------

plot_burden_dollar <- joined %>% 
  ggplot(aes(x = log(sum_assistance_amount), y = percent_renter_severe_cost_burden)) +
  geom_point(alpha = 0.3, color = "#1f2859ff") +
  geom_smooth(color = "#0b9444ff")

plot_burden_dollar

plot_burden_addresses <- joined %>% 
  ggplot(aes(x = log(unique_assisted_addresses), y = percent_renter_severe_cost_burden)) +
  geom_point(alpha = 0.3, color = "#1f2859ff") +
  geom_smooth(color = "#0b9444ff")

plot_burden_addresses


# ---------------------------------------------------------------------------------------------

joined %>% 
  ggplot(aes(fill = percent_renter_households_of_color, geometry = geometry)) +
  geom_sf()

mapview::mapview(joined %>% st_as_sf(.) %>% mutate(sum_assistance_amount_logged = log(sum_assistance_amount)), zcol = "sum_assistance_amount_logged")








