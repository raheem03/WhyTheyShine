########################################################################################
# File: CreatingStateMaps
# Date: 8/16/2018
# Author: Raheem Chaudhry
# Description: Loads Census data and produces state and county maps.
########################################################################################

########################################################################################
# set environment (recommended before starting/switching analyses)
########################################################################################

# For my use only
# rm(list = ls())
# setwd(paste(Sys.getenv("gitpath"), "Visualizations", sep = "/"))

# Load
require(censusapi)
require(urbnmapr)
require(dplyr)
require(ggplot2)

########################################################################################
# STATE MAPS
########################################################################################

# Get data by state from Census
df <- getCensus(name = "acs/acs5", vintage = 2016,
          vars = c("NAME", "B19013_001E"),
          region = "state:*")

## Minimum code to create a map
df %>%
  rename(state_fips = state) %>%
  left_join(states, by = "state_fips") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = B19013_001E)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Customize
df %>% 
  rename(state_fips = state) %>%
  left_join(states, by = "state_fips") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = B19013_001E)) +
  scale_fill_gradient(low = "#ffffff", high = "#e8544e",
                      labels = scales:: dollar, 
                      guide = guide_colorbar(title = element_blank())) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(.75, "in")) + 
  labs(title = "Median Household Income, by State",
       subtitle = "2012 - 2016", 
       caption = "Author's calculations of Census data from 5-year American Community Survey")

########################################################################################
# COUNTY MAP (WITHIN STATE)
########################################################################################

# Get data by county for Virginia in VA
df <- getCensus(name = "acs/acs5", vintage = 2016,
                vars = c("NAME", "B19013_001E"),
                region = "county:*", regionin = "state:51")

df %>% 
  mutate(county_fips = paste0(state, county)) %>%
  left_join(counties, by = "county_fips") %>% 
  filter(state == 51) %>%
  ggplot(mapping = aes(long, lat, group = group, fill = B19013_001E)) +
  scale_fill_gradient(low = "#ffffff", high = "#e8554e",
                      labels = scales:: dollar, 
                      guide = guide_colorbar(title = element_blank())) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(.75, "in")) + 
  labs(title = "Median Household Income by County in Virginia",
       subtitle = "2012 - 2016", 
       caption = "Author's calculations of Census data from 5-year American Community Survey")

########################################################################################
# END OF FILE
########################################################################################
