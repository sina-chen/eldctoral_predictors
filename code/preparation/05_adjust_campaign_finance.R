#-------------------------------------------------------------------------------
#
# Electoral predictors - Adjust campaign finance data
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(dplyr)


# Data --------------------------------------------------------------------

polls <- readRDS("data/polls/us_senate_polls1990_2022_context.RDS")
vep <- readRDS("data/election_features/vep1990_2022.RDS")

# inflation adjustment factor
inflation_adj <- data.frame(year = seq(1990, 2022, 2),
                            adj_value = c(2.24, 2.09, 1.97, 1.87, 1.8, 
                                          1.7, 1.63, 1.55, 1.45, 1.36,
                                          1.34, 1.27, 1.24, 1.22, 1.17,
                                          1.13, 1))

#-------------------------------------------------------------------------------

# Preparation -------------------------------------------------------------

# election level data
election_level_data <- polls %>% 
  group_by(cycle, state, state_abb, exp_rep, raising_rep, exp_dem, 
           raising_dem) %>% 
  summarise() %>% 
  ungroup()


# Adjust expenditures and raising -----------------------------------------

# merge inflation adjustment
election_level_data <- merge(election_level_data, inflation_adj %>% 
                               select(year, adj_value), 
                             by.x = "cycle", by.y = "year", all.x = T)

rm(inflation_adj)

# calculate inflation adjusted expenditures and disbursements
#   adj_exp[year] = exp[year] * adj_value[2022]/adj_value[year] https://www.lawyerdb.de/Inflationsrechner.aspx
election_level_data <- election_level_data %>% 
  mutate(exp_adj_rep = (1/adj_value)*exp_rep,
         raising_adj_rep = (1/adj_value)*raising_rep,
         exp_adj_dem = (1/adj_value)*exp_dem,
         raising_adj_dem = (1/adj_value)*raising_dem)

# merge VEP
election_level_data <- merge(election_level_data, vep, 
                             by.x = c("cycle", "state_abb"), 
                             by.y = c("cycle", "state"),
                             all.x = T)
rm(vep)

# calculate per VEP capita expenditures and raising
election_level_data <- election_level_data %>% 
  mutate(exp_pc_rep = exp_adj_rep/vep,
         raising_pc_rep = raising_adj_rep/vep,
         exp_pc_dem = exp_adj_dem/vep,
         raising_pc_dem = raising_adj_dem/vep)

# merge to polling data
polls <- merge(polls, election_level_data, 
               by = c("cycle", "state", "state_abb", "exp_rep", "raising_rep", 
                      "exp_dem", "raising_dem"))

saveRDS(polls, "data/polls/us_senate_polls1990_2022_context_finance.RDS")
