#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   margin last poll
#
# Data: US Senate pre-election polls 1990 - 2022
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(rv)
  library(gtools)
  library(rstan)
  library(dplyr)
  library(scales)
}

setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")

# simulation results
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_last.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 1460) %>% # exclude polls conducted more than 4 years (max time to previous election)
  group_by(state, cycle) %>% 
  mutate(n_poll = n(),
         last_poll = max(end_date)) %>% 
  ungroup() %>% 
  filter(n_poll >= 5) %>% 
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)),
         last_poll_margin = if_else(end_date == last_poll, pct2_rep-pct2_dem, 
                                    NA_real_))

# Election-level data 
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle,  state, vote2_rep) %>%
  summarise(last_poll_margin = mean(last_poll_margin, na.rm = T)) %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


#### Election level election day bias ####

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$last_poll_margin + 
                    postrv$alpha_sc*postrv$sig_alpha)

# expected election day estimates
exp_p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                        postrv$mu_alpha + 
                        postrv$beta2*election_data$last_poll_margin)

# election day bias
b0_r <- p0_r - election_data$vote2_rep

b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

# expected election day bias
exp_b0_r <- exp_p0_r - election_data$vote2_rep

exp_b0_summary <- summary(exp_b0_r)
exp_b0_summary$state_year_int <- seq(1:nrow(exp_b0_summary))
exp_b0_summary <- merge(exp_b0_summary, election_data, by = "state_year_int")

saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_last.RDS")
saveRDS(exp_b0_summary, "~/results_vis/us_senate_predictable/us_senate_exp_b0_last.RDS")

