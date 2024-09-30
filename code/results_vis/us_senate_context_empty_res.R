#-------------------------------------------------------------------------------
#
#  Electoral predictors - empty model election day bias and 
#     (election day) excess variance
#   - full time period
#   - 30 day time window
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(rv)
  library(dplyr)
  library(gtools)
  library(rstan)
}


setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")

# simulation results
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_empty.RDS')
resStan30 <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_empty30.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x)


# Preparation -------------------------------------------------------------

# compute election groups and ids for election year and state
polls <- polls %>%
  filter(dte <= 100) %>% 
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle)) %>%
  group_by(state_year) %>%
  mutate(n_poll = n()) %>%
  ungroup() %>%
  filter(n_poll >= 5) %>% # exclude polls conducted more than 4 years (max time to orevious election)
  mutate(state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

polls30 <- polls %>%
  filter(dte <= 30) %>% 
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle)) %>%
  group_by(state_year) %>%
  mutate(n_poll = n()) %>%
  ungroup() %>%
  filter(n_poll >= 5) %>% # exclude polls conducted more than 4 years (max time to orevious election)
  mutate(state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

# Election-level data
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle, state, state_abb, vote2_rep) %>%
  summarise(n_avg = mean(sample_size)) %>%
  ungroup()

election_data30 <- polls30 %>%
  group_by(state_year, state_year_int, cycle, state, state_abb, vote2_rep) %>%
  summarise(n_avg = mean(sample_size)) %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)
postrv30 <- as.rv(resStan30)
rm(polls30, resStan, resStan30)


# Election-level election day bias ----------------------------------------

# election day estimate
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$alpha)
p0_r30 <- ilogit.rv(logit.rv(election_data30$vote2_rep) +
                    postrv30$alpha)

# election day bias
b0 <- p0_r - election_data$vote2_rep
b0_30 <- p0_r30 - election_data30$vote2_rep

# election day bias summary
b0_summary <- summary(b0)
b0_summary$state_year_int <- seq(1:length(unique(election_data$state_year)))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")
b0_summary <- b0_summary %>%
  mutate(cycle_factor = as.factor(cycle))
rm(b0)

b0_30_summary <- summary(b0_30)
b0_30_summary$state_year_int <- seq(1:length(unique(election_data30$state_year)))
b0_30_summary <- merge(b0_30_summary, election_data30, by = "state_year_int")
b0_30_summary <- b0_30_summary %>%
  mutate(cycle_factor = as.factor(cycle))
rm(b0_30, p0_r30)

saveRDS(b0_summary, "~/results_vis/us_senate_predictable/bias_senate1990_2022_empty.RDS")
saveRDS(b0_30_summary, "~/results_vis/us_senate_predictable/bias_senate1990_2022_empty30.RDS")


# Election-level excess SD ------------------------------------------------

ex_sd_summary <- sqrt(postrv$phi2) %>%
  summary()
ex_sd_summary$state_year_int <- seq(1:length(unique(election_data$state_year)))
ex_sd_summary <- merge(ex_sd_summary, election_data, by = "state_year_int")

ex_sd_30_summary <- sqrt(postrv30$phi2) %>%
  summary()
ex_sd_30_summary$state_year_int <- seq(1:length(unique(election_data30$state_year)))
ex_sd_30_summary <- merge(ex_sd_30_summary, election_data30, by = "state_year_int")
rm(postrv30)

saveRDS(ex_sd_summary, "~/results_vis/us_senate_predictable/excess_sd_senate1990_2022_empty.RDS")
saveRDS(ex_sd_30_summary, "~/results_vis/us_senate_predictable/excess_sd_senate1990_2022_empty30.RDS")


# Election level election day variance ------------------------------------     

# election day variance
v0_i <- (p0_r*(1-p0_r))[polls$state_year_int]/polls$sample_size + postrv$phi2[polls$state_year_int]
rm(postrv)

# avg. election-level election day variance
v0_r <- rv(length(unique(polls$state_year)))

for(i in 1:length(unique(polls$state_year))){
  v0_r[i] <- mean(v0_i[i == polls$state_year_int])
}

v0_r_summary <- summary(v0_r)
v0_r_summary$state_year_int <- seq(1:length(unique(polls$state_year)))
v0_r_summary <- merge(v0_r_summary, election_data, by = "state_year_int")
rm(v0_i, v0_r, p0_r, polls)

saveRDS(v0_r_summary, "~/results_vis/us_senate_predictable/v0_senate1990_2022_empty.RDS")
