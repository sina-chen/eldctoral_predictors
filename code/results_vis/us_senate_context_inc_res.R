#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   incumbency
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
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_inc.RDS')


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
         t_sc = as.numeric(dte)/max(as.numeric(dte)),
         inc_dem = if_else(inc == "Dem. incumbent", 1, 0),
         inc_rep = if_else(inc == "Rep. incumbent", 1, 0))

# Election-level data 
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle,  state, vote2_rep, 
           inc_dem, inc_rep) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


#### Election level election day bias ####

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$inc_dem + 
                    postrv$beta3*election_data$inc_rep + 
                    postrv$alpha_sc*postrv$sig_alpha)

# expected election day estimates
exp_p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$inc_dem + 
                    postrv$beta3*election_data$inc_rep)


# # expected empty election day estimates
# p0empty_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
#                         postrv$mu_alpha)

# election level election day bias estimate
b0_r <- p0_r - election_data$vote2_rep
saveRDS(b0_r, "~/results_vis/us_senate_predictable/us_senate_b0_sim_inc.RDS")

# expected election level election day bias estimate
exp_b0_r <- exp_p0_r - election_data$vote2_rep

# expected election level feature bias estimate
# b0feature_r <- p0_r - p0empty_r
rm(p0empty_r, p0_r)

# election level bias summaries
b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

# # compute mean bias for each simulation & summarise
# b0_dem_summary <- mean(b0feature_r[which(election_data$inc_dem == 1)]) %>%  
#   summary() %>% 
#   mutate(feature = "inc_dem")
# 
# b0_rep_summary <- mean(b0feature_r[which(election_data$inc_rep == 1)]) %>%  
#   summary()  %>% 
#   mutate(feature = "inc_rep")
# 
# rm(b0feature_r)
# 
# bias_inc <- rbind(b0_dem_summary, b0_rep_summary)
# rm(b0_dem_summary, b0_rep_summary)

# avg expected edb by incumbency
avg_b0_dem_summary <- mean(exp_b0_r[which(election_data$inc_dem == 1)]) %>%  
  summary() %>% 
  mutate(feature = "inc_dem")

avg_b0_rep_summary <- mean(exp_b0_r[which(election_data$inc_rep == 1)]) %>%  
  summary() %>% 
  mutate(feature = "inc_rep")

avg_b0_neither_summary <- mean(exp_b0_r[which(election_data$inc_rep == 0 &
                                            election_data$inc_dem == 0)]) %>%  
  summary() %>% 
  mutate(feature = "inc_neither")

avg_b0_summary <- rbind(avg_b0_dem_summary, avg_b0_rep_summary, 
                        avg_b0_neither_summary)
rm(avg_b0_dem_summary, avg_b0_rep_summary, avg_b0_neither_summary)

# save
# saveRDS(bias_inc, "~/results_vis/us_senate_predictable/us_senate_bias_inc.RDS")
saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_inc.RDS")
saveRDS(avg_b0_summary, "~/results_vis/us_senate_predictable/us_senate_avg_b0_inc.RDS")

