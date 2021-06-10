# Load Packages  -----------------------------------------------------------

library(tidyverse)
library(patchwork)


# DATA CHECK --------------------------------------------------------------
# LOAD DATA----
ted <- read_csv("data/unprocessed/ted_data.csv") %>% 
  # standardized naming convention
  janitor::clean_names() %>% 
  # mutate character variables to factor
  mutate_if(is.character, as.factor)


# CHECK DATA-----
# check problems in read-in
problems(ted)
# need to get rid of the x in front of years, then
# i need to pivot_longer on the years and pivot_wider on indicator
# measure is a descriptor, which I don't need in the dataset but coudl use in the app


ted <- ted %>% 
  # get rid of X's in column names
  rename_with(~str_remove(., 'x')) %>% 
  # make year its own column
  pivot_longer(
    cols = !c(region, iso, country, indicator, measure),
    names_to = "year",
    values_to = "value"
  ) %>% 
  # make year column numeric
  mutate(year = as.numeric(year)) %>% 
  select(-c(iso, measure)) %>% 
  # make the measure values into their own columns
  pivot_wider(
    names_from = "indicator",
    values_from = "value"
  ) %>% 
  janitor::clean_names()

# check that types match
ted %>% glimpse()


# check missingness issues
ted %>%  naniar::miss_var_summary() %>% 
  arrange(-pct_miss) %>% 
  print(n = Inf)

# take a quick skim over the data
skimr::skim_without_charts(ted)

# Data for graph ----------------------------------------------------------
ted_region_dat <- ted %>% 
  # get rid of growth variables because they're not that important (esp if we are seeing a line graph)
  # easier to just see real_gdp instead of nominal_gdp 
  select(-c(contains("growth"), nominal_gdp, country)) %>% 
  group_by(region, year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup()

write_csv(ted_region_dat, "data/processed/ted_region_dat.csv")

# Testing visualizations --------------------------------------------------

ted_region_dat %>% 
  ggplot(mapping = aes(x = year, y = employment, color = region)) +
  geom_line()

