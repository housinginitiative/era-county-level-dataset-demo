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

# ACS population density
population_density <- 
  get_acs(geography = "county",
          variables = c("B01003_001"),
          year = 2023,
          survey = "acs5",
          geometry = TRUE) %>% 
  clean_names() %>% 
  select(-moe) %>% 
  mutate(area = as.numeric(st_area(geometry)) * 0.0000003861) %>% 
  mutate(population_density = estimate / area)

# Join data -----------------------------------------------------------------------------------

joined <- era %>% 
  mutate(sum_assistance_amount_logged = log10(sum_assistance_amount)) %>% 
  mutate(unique_assisted_addresses_logged = log10(unique_assisted_addresses)) %>% 
  # Race/ethnicity
  inner_join(renter_race_ethnicity %>% 
              select(geoid, name, percent_renter_households_of_color),
            by = "geoid") %>% 
  mutate(percent_renter_households_of_color_decile = 
           ntile(percent_renter_households_of_color, 10)) %>% 
  group_by(percent_renter_households_of_color_decile) %>% 
  mutate(percent_renter_households_of_color_decile_min = 
           min(percent_renter_households_of_color)) %>% 
  mutate(percent_renter_households_of_color_decile_max = 
           max(percent_renter_households_of_color)) %>% 
  ungroup() %>% 
  mutate(percent_renter_households_of_color_decile_label = 
           str_c(label_comma(1)(percent_renter_households_of_color_decile_min * 100), 
                 "- ", 
                 label_percent(1)(percent_renter_households_of_color_decile_max))) %>% 
  arrange(percent_renter_households_of_color_decile) %>% 
  mutate(percent_renter_households_of_color_decile_label =
           as_factor(percent_renter_households_of_color_decile_label)) %>% 
  # Severe cost burden
  inner_join(renter_severe_cost_burden %>% 
               st_drop_geometry() %>% 
               select(geoid, percent_renter_severe_cost_burden),
             by = "geoid") %>% 
  mutate(percent_renter_severe_cost_burden_decile = 
           ntile(percent_renter_severe_cost_burden, 10)) %>% 
  group_by(percent_renter_severe_cost_burden_decile) %>% 
  mutate(percent_renter_severe_cost_burden_decile_min = 
           min(percent_renter_severe_cost_burden)) %>% 
  mutate(percent_renter_severe_cost_burden_decile_max = 
           max(percent_renter_severe_cost_burden)) %>% 
  ungroup() %>% 
  mutate(percent_renter_severe_cost_burden_decile_label = 
           str_c(label_comma(1)(percent_renter_severe_cost_burden_decile_min * 100), 
                 "- ", 
                 label_percent(1)(percent_renter_severe_cost_burden_decile_max))) %>% 
  arrange(percent_renter_severe_cost_burden_decile) %>% 
  mutate(percent_renter_severe_cost_burden_decile_label =
           as_factor(percent_renter_severe_cost_burden_decile_label)) %>% 
  # Population density
  inner_join(population_density %>% 
               st_drop_geometry() %>% 
               select(geoid, population_density),
             by = "geoid") %>% 
  mutate(population_density_decile = 
           ntile(population_density, 10)) %>% 
  group_by(population_density_decile) %>% 
  mutate(population_density_decile_min = 
           min(population_density)) %>% 
  mutate(population_density_decile_max = 
           max(population_density)) %>% 
  ungroup() %>% 
  mutate(population_density_decile_label = 
           str_c(label_comma(1)(population_density_decile_min), 
                 "- ", 
                 label_comma(1)(population_density_decile_max))) %>% 
  arrange(population_density_decile) %>% 
  mutate(population_density_decile_label =
           as_factor(population_density_decile_label))

# Plots: race and ethnicity --------------------------------------------------------------------

plot_race_dollar <- joined %>% 
  ggplot(aes(y = sum_assistance_amount_logged, 
             x = percent_renter_households_of_color_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_dollar()(10^.)) +
  labs(x = NULL,
       y = "Amount paid in assistance (logged)")

plot_race_dollar

plot_race_addresses <- joined %>% 
  ggplot(aes(y = unique_assisted_addresses_logged, 
             x = percent_renter_households_of_color_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_comma()(10^.)) +
  labs(x = NULL,
       y = "Unique addresses assisted (logged)")

plot_race_addresses

ggpubr::ggarrange(plot_race_dollar, NULL, plot_race_addresses,
                  nrow = 1, widths = c(1, 0.05, 1)) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Counties with higher share of renters of color (generally) had more ERA assistance", face = "bold"),
                          bottom = "Percent renter households of color (in deciles)") %>% 
  ggsave(str_c(project_path, "/images/", "plot_race.png"), ., width = 10, height = 4)

# Plots: severe cost burden --------------------------------------------------------------------

plot_burden_dollar <- joined %>% 
  ggplot(aes(y = sum_assistance_amount_logged, 
             x = percent_renter_severe_cost_burden_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_dollar()(10^.)) +
  labs(x = NULL,
       y = "Amount paid in assistance (logged)")

plot_burden_dollar

plot_burden_addresses <- joined %>% 
  ggplot(aes(y = unique_assisted_addresses_logged, 
             x = percent_renter_severe_cost_burden_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_comma()(10^.)) +
  labs(x = NULL,
       y = "Unique addresses assisted (logged)")

plot_burden_addresses

ggpubr::ggarrange(plot_burden_dollar, NULL, plot_burden_addresses,
                  nrow = 1, widths = c(1, 0.05, 1)) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Counties with higher rent burden had more ERA assistance", face = "bold"),
                          bottom = "Percent severely rent burdened (in deciles)") %>% 
  ggsave(str_c(project_path, "/images/", "plot_burden.png"), ., width = 10, height = 4)

# Plots: population density -------------------------------------------------------------------

plot_density_dollar <- joined %>% 
  ggplot(aes(y = sum_assistance_amount_logged, 
             x = population_density_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_dollar()(10^.)) +
  labs(x = NULL,
       y = "Amount paid in assistance (logged)")

plot_density_dollar

plot_density_addresses <- joined %>% 
  ggplot(aes(y = unique_assisted_addresses_logged, 
             x = population_density_decile_label)) +
  geom_violin(draw_quantiles = 0.5, fill = "#1f2859ff", color = "white") +
  scale_x_discrete(labels = ~ str_wrap(., 1)) +
  scale_y_continuous(labels = ~ label_comma()(10^.)) +
  labs(x = NULL,
       y = "Unique addresses assisted (logged)")

plot_density_addresses

ggpubr::ggarrange(plot_density_dollar, NULL, plot_density_addresses,
                  nrow = 1, widths = c(1, 0.05, 1)) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Counties with higher population density had more ERA assistance", face = "bold"),
                          bottom = "Population per square mile (in deciles)") %>% 
  ggsave(str_c(project_path, "/images/", "plot_density.png"), ., width = 10, height = 4)






