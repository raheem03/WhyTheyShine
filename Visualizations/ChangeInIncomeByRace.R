########################################################################################
# File: AnimationsWithCensusData.r
# Date: 7/19/2018
# Author: Raheem Chaudhry
# Description: Accessing and Visualizing Census data.
########################################################################################

########################################################################################
# 1. set environment (recommended before starting/switching analyses)
########################################################################################


# Set working directory
# setwd("YOUR_PATH")
#Sys.setenv(CENSUS_KEY="YOUR_KEY_HERE")

# Load packages
if (!require("devtools")) install.packages("devtools", dep=TRUE)

## There is a bug that R3.5 is incompatible with build tools so this helps for now - RC 7/28/2018 (necessary for gganimate while it's in build mode)
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")

if (!require("dplyr")) install.packages("dplyr", dep=TRUE)
if (!require("tidyr")) install.packages("tidyr", dep=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dep=TRUE)
if (!require("censusapi")) install.packages("censusapi", dep=TRUE)
if (!require("urbnmapr")) devtools::install_github("UrbanInstitute/urbnmapr")
if (!require("gganimate")) devtools::install_github("thomasp85/gganimate")


########################################################################################
# GET CENSUS DATA
########################################################################################

get1yr <- function(datayear) {
  df <- getCensus(name = "acs/acs1", vintage = datayear,
                  vars = c("NAME", "B19013_001E", "B19013B_001E", "B19013H_001E", "B19013I_001E"),
                  region = "us:*") %>%
    mutate(year = datayear)
  return(df)
}

dflist = list()
for (i in 2010:2016) {
  df <- get1yr(i)
  assign(paste0("acs", i), df)
  dflist[[i-2009]] <- df
}

acs <- bind_rows(dflist) %>%
  gather(race, median_income, -us, -NAME, -year) %>%
  mutate(race = case_when(race == "B19013_001E" ~ "All", 
                          race == "B19013H_001E" ~ "Non-Hispanic White", 
                          race == "B19013B_001E" ~ "Black", 
                          race == "B19013I_001E" ~ "Hispanic")) %>%
  mutate(real_median_income = case_when(year == 2010 ~ median_income*(352.6/320.2), 
                                        year == 2011 ~ median_income*(352.6/330.5), 
                                        year == 2012 ~ median_income*(352.6/337.3), 
                                        year == 2013 ~ median_income*(352.6/342.2), 
                                        year == 2014 ~ median_income*(352.6/347.8), 
                                        year == 2015 ~ median_income*(352.6/348.2),
                                        year == 2016 ~ median_income)) %>%
  arrange(year, race) %>%
  group_by(race) %>%
  mutate(changerealincome = real_median_income/dplyr::lag(real_median_income, n = 1) - 1) %>%
  filter(year > 2010)

########################################################################################
# LINE GRAPH ANIMATION
########################################################################################


df <- data.frame(year = acs$year, 
                 real_median_income = acs$real_median_income,
                 changerealincome = acs$changerealincome, 
                 race2 = acs$race)

ani <- acs %>%
  ggplot(aes(year, changerealincome)) + 
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y=element_line(color="#e2e2e2", linetype="dotted"), 
        panel.grid.minor.y=element_line(color="#e2e2e2", linetype="dotted"),
        axis.ticks=element_blank(), axis.title.x = element_blank()) + 
  geom_hline(yintercept=0, color="#bababa") + 
  geom_line(data = df, mapping=aes(year, changerealincome, group = race2), color = "gray", size = 1, alpha = 1) + 
  geom_line(color="#e8554e", alpha = 1, size = 1.5) + 
  transition_states(race, 1, 1.5) + 
  labs(title = "One-year Change in Median Income in the U.S. for \n {closest_state} Americans", 
       subtitle = "2011 - 2016", 
       caption = "Author's calculations of Census data from 1-year American Community Survey", 
       y = "Percent Change in Real Median Income")

animate(ani, nframes = 75)
anim_save("ChangeInIncomeByRace_LineAnimation_v1.gif", path = getwd())

########################################################################################
# BAR CHART ANIMATION
########################################################################################

ani <- acs %>%
  ggplot(aes(factor(race), changerealincome)) + 
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y=element_line(color="#e2e2e2", linetype="dotted"), 
        panel.grid.minor.y=element_line(color="#e2e2e2", linetype="dotted"),
        axis.ticks=element_blank(), axis.title.x = element_blank()) + 
  geom_hline(yintercept=0, color="#bababa") + 
  geom_bar(stat = "identity", position = "identity") + 
  labs(title = "One-year Change in Median Income by Race", 
       subtitle = "{closest_state}", 
       caption = "Author's calculations of Census data from 1-year American Community Survey", 
       y = "Percent Change in Real Median Income") + 
    transition_states(year, 1, 1.5)

animate(ani, nframes = 75)
anim_save("ChangeInIncomeByRace_BarAnimation_v1.gif", path = getwd())

########################################################################################
# END OF FILE
########################################################################################
