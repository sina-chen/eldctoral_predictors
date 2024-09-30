#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   turnout
#
# Data: US Senate pre-election polls 1990 - 2022
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(rv)
  library(gtools)
  library(ggplot2)
  library(rstan)
  library(dplyr)
  library(scales)
  library(shinystan)
  library(RColorBrewer)
  library(ggridges)
  library(cowplot)
}

setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")

# simulation results
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_turnout.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 101 & !is.na(turnout_vep)) %>% # exclude polls conducted more than 4 years (max time to previous election)
  group_by(state, cycle) %>%
  mutate(n_poll = n()) %>%
  ungroup() %>%
  filter(n_poll >= 5) %>%
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

# Election-level data 
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle, state, vote2_rep, turnout_vep) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)



# Compute estimates -------------------------------------------------------

#### Election level excess variance ####

#  excess sd
ex_sd_r <- sqrt(postrv$mu_phi2 + postrv$phi2_sc*postrv$sig_phi2 + 
                  postrv$gamma*election_data$turnout_vep)

summary_ex_sd_r <- summary(ex_sd_r)
summary_ex_sd_r$state_year_int <- seq(1, nrow(election_data))
summary_ex_sd_r <- merge(summary_ex_sd_r, election_data, by = "state_year_int")


# expected excess sd
exp_ex_sd_r <- sqrt(postrv$mu_phi2 + postrv$gamma*election_data$turnout_vep)

summary_exp_ex_sd_r <- summary(exp_ex_sd_r)
summary_exp_ex_sd_r$state_year_int <- seq(1, nrow(election_data))
summary_exp_ex_sd_r <- merge(summary_exp_ex_sd_r, election_data, by = "state_year_int")


# save
saveRDS(summary_ex_sd_r, "~/results_vis/us_senate_predictable/us_senate_ex_sd_turnout.RDS")
saveRDS(summary_exp_ex_sd_r, "~/results_vis/us_senate_predictable/us_senate_exp_ex_sd_turnout.RDS")
