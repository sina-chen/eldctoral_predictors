#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   average expected edb name white prob rep
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
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_name_prob_white_rep.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 101) %>% # exclude polls conducted more than 4 years (max time to previous election)
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
  group_by(state_year, state_year_int, cycle,  state, vote2_rep, 
           name_prob_white_rep) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


# Estimates ---------------------------------------------------------------

#### Election level election day bias ####

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$name_prob_white_rep +
                    postrv$alpha_sc*postrv$sig_alpha)

# election level election day bias
b0_r <- p0_r - election_data$vote2_rep

# election level bias summaries
b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_minority_name_prob_rep.RDS")


#### Average expected EDB ####

avg_b0 <- rv(length = length(election_data$name_prob_white_rep))

for(i in 1:nrow(election_data)){
  b0_sim_r_i <- sapply(
    election_data$vote2_rep, 
    function(x) 
      ilogit.rv(logit.rv(x) +
                  postrv$mu_alpha + 
                  postrv$beta2*election_data$name_prob_white_rep[i]) - x)
  avg_b0[[i]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  
}

# summary
avg_b0_summary <- summary(avg_b0)
avg_b0_summary$state_year_int <- 1:nrow(election_data)
avg_b0_summary <- merge(avg_b0_summary, election_data, by = "state_year_int")

# save
saveRDS(avg_b0_summary, "~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_name_prob_white_rep.RDS")


