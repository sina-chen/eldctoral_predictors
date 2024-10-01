#-------------------------------------------------------------------------------
#
# Electoral predictors - fit CF Score Dem. Model
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  library(shinystan)
}


# Data --------------------------------------------------------------------

polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 101 & !is.na(cf_score_dem)) %>%        # exclude polls conducted more than 100 days before election day
  group_by(state, cycle) %>%                          # compute election groups
  mutate(n_poll = n()) %>% 
  ungroup() %>% 
  filter(n_poll >= 5) %>%                             # exclude elections with less than 5 polls
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte))) # scale days to election 0-1
  
# election-level data 
vote_sy <- polls %>%
  group_by(state_year, cycle,  state, vote2_rep, cf_score_dem) %>%
  summarise() %>%
  ungroup()

# Stan data 
stan_dat <- list(
  N = nrow(polls),                             # number of polls
  SY = length(unique(polls$state_year)),       # number of elections

  poll = polls$pct2_rep,                       # two-party poll share
  vote = vote_sy$vote2_rep,                    # two-party vote share
  
  feature = vote_sy$cf_score_dem,              # election-level feature
  t = polls$t_sc,                              # scaled days to election
  
  sample_size = polls$sample_size * 
    (polls$vote_rep + polls$vote_dem),         # sample size adjusted for Rep. & Dem. poll share
  
  sy_id = polls$state_year_int                 # election id
  
)

# check Stan data
sapply(stan_dat, length)
sapply(stan_dat, range)


# Fit Stan model ----------------------------------------------------------

resStan <- stan(file = "~/fit_stan/stan_ml/ml_senate_bias_context_cont.stan", 
                data = stan_dat,
                chains = 4, iter = 5000,
                control = list(adapt_delta = 0.95, max_treedepth = 12)
) 

# save simulation results
saveRDS(resStan, '~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_cf_dem.RDS') 
