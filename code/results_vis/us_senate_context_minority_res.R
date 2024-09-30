#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   minority results
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
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_minority.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 101) %>% 
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
           minority_dem, minority_rep, minority, minority4) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)


#### Election level election day bias ####

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$minority_dem + 
                    postrv$beta3*election_data$minority_rep + 
                    postrv$beta4*election_data$minority_dem*election_data$minority_rep +
                    postrv$alpha_sc*postrv$sig_alpha)

# expected election day estimates
exp_p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                        postrv$mu_alpha  + 
                        postrv$beta2*election_data$minority_dem + 
                        postrv$beta3*election_data$minority_rep + 
                        postrv$beta4*election_data$minority_dem*election_data$minority_rep)

# election level election day bias
b0_r <- p0_r - election_data$vote2_rep

# expected election level election day bias
exp_b0_r <- exp_p0_r - election_data$vote2_rep

# election level bias summaries
b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

# # compute mean bias for each simulation & summarise
# b0_dem_summary <- mean(b0feature_r[which(election_data$race_dem == 1 &
#                                      election_data$race_rep == 0)]) %>%  
#   summary() %>% 
#   mutate(context = "minority_dem")
# 
# b0_rep_summary <- mean(b0feature_r[which(election_data$race_rep == 1 &
#                                      election_data$race_dem == 0)]) %>%  
#   summary()  %>% 
#   mutate(context = "minority_rep")
# 
# b0_both_summary <- mean(b0feature_r[which(election_data$race_dem == 1 & 
#                                       election_data$race_rep == 1)]) %>%  
#   summary()  %>% 
#   mutate(context = "minority_both")
# rm(b0feature_r)
# 
# bias_race <- rbind(b0_dem_summary, b0_rep_summary, b0_both_summary)
# rm(b0_dem_summary, b0_rep_summary, b0_both_summary)

# avg edb expectation by gender
avg_b0_dem_summary <- mean(exp_b0_r[which(election_data$minority_dem == 1 &
                                        election_data$minority_rep == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_dem")

avg_b0_rep_summary <- mean(exp_b0_r[which(election_data$minority_rep == 1 &
                                        election_data$minority_dem == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_rep")

avg_b0_both_summary <- mean(exp_b0_r[which(election_data$minority_rep == 1 &
                                         election_data$minority_dem == 1)]) %>%  
  summary() %>% 
  mutate(feature = "minority_both")

avg_b0_neither_summary <- mean(exp_b0_r[which(election_data$minority_rep == 0 &
                                            election_data$minority_dem == 0)]) %>%  
  summary() %>% 
  mutate(feature = "minority_neither")

avg_b0_summary <- rbind(avg_b0_dem_summary, avg_b0_rep_summary, 
                        avg_b0_both_summary, avg_b0_neither_summary)
rm(avg_b0_dem_summary, avg_b0_rep_summary, avg_b0_both_summary, 
   avg_b0_neither_summary)

# save
saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_minority.RDS")
saveRDS(avg_b0_summary, "~/results_vis/us_senate_predictable/us_senate_avg_b0_minority.RDS")

