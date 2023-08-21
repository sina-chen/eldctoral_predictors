#-------------------------------------------------------------------------------
#
# Putting polling errors into context
#   average expected edb state democracy score
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
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_democracy.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 1460 & !is.na(democracy_mcmc)) %>% # exclude polls conducted more than 4 years (max time to previous election)
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
  group_by(state_year, state_year_int, cycle,  state, vote2_rep, democracy_mcmc) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


# Average expected EDB ----------------------------------------------------


avg_b0_last <- rv(length = length(election_data$democracy_mcmc))

for(i in 1:nrow(election_data)){
  b0_sim_r_i <- sapply(election_data$vote2_rep, 
                       function(x) 
                         ilogit.rv(logit.rv(x) +
                                     postrv$mu_alpha + 
                                     postrv$beta2*election_data$democracy_mcmc[i]) - 
                         x)
  avg_b0_last[[i]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  
}

# summary
avg_b0_last_summary <- summary(avg_b0_last)
avg_b0_last_summary$state_year_int <- 1:nrow(election_data)
avg_b0_last_summary <- merge(avg_b0_last_summary, election_data, by = "state_year_int")

# save
saveRDS(avg_b0_last_summary, "~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_democracy.RDS")


