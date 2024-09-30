#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   minority based on name
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
  library(ggplot2)
  library(cowplot)
}

setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")

# simulation results
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_minority_name.RDS')


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
         minority_name_dem = if_else(name_white_dem == 0, 1, 0),
         minority_name_rep = if_else(name_white_rep == 0, 1, 0))

# Election-level data 
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle,  state, vote2_rep, 
           minority_name_dem, minority_name_rep, minority, minority4) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


#### Election level election day bias ####

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$minority_name_dem + 
                    postrv$beta3*election_data$minority_name_rep + 
                    postrv$beta4*election_data$minority_name_dem*election_data$minority_name_rep +
                    postrv$alpha_sc*postrv$sig_alpha)

# expected election day estimates
exp_p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                        postrv$mu_alpha  + 
                        postrv$beta2*election_data$minority_name_dem + 
                        postrv$beta3*election_data$minority_name_rep + 
                        postrv$beta4*election_data$minority_name_dem*election_data$minority_name_rep)

# election level election day bias
b0_r <- p0_r - election_data$vote2_rep

# expected election level election day bias
exp_b0_r <- exp_p0_r - election_data$vote2_rep

# election level bias summaries
b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

rm(b0_r, p0_r)

# avg edb expectation by gender
avg_b0_dem_summary <- mean(exp_b0_r[which(election_data$minority_name_dem == 1 &
                                        election_data$minority_name_rep == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_dem")

avg_b0_rep_summary <- mean(exp_b0_r[which(election_data$minority_name_rep == 1 &
                                        election_data$minority_name_dem == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_rep")

avg_b0_both_summary <- mean(exp_b0_r[which(election_data$minority_name_rep == 1 &
                                         election_data$minority_name_dem == 1)]) %>%  
  summary() %>% 
  mutate(feature = "minority_both")

avg_b0_neither_summary <- mean(exp_b0_r[which(election_data$minority_name_rep == 0 &
                                            election_data$minority_name_dem == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_neither")

avg_b0_summary <- rbind(avg_b0_dem_summary, avg_b0_rep_summary, 
                        avg_b0_both_summary, avg_b0_neither_summary)
rm(avg_b0_dem_summary, avg_b0_rep_summary, avg_b0_both_summary, 
   avg_b0_neither_summary, exp_b0_r, exp_p0_r)

# save
saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_minority_name.RDS")
saveRDS(avg_b0_summary, "~/results_vis/us_senate_predictable/us_senate_avg_b0_minority_name.RDS")
