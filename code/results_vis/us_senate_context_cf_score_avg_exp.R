#-------------------------------------------------------------------------------
#
# Putting polling errors into context
#   average expected edb cf score
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
resStan_dem <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_cf_dem.RDS')
resStan_rep <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_cf_rep.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls_dem <- polls %>%
  filter(dte < 1460 & !is.na(cf_score_dem)) %>% # exclude polls conducted more than 4 years (max time to previous election)
  group_by(state, cycle) %>% 
  mutate(n_poll = n()) %>% 
  ungroup() %>% 
  filter(n_poll >= 5) %>% 
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

polls_rep <- polls %>%
  filter(dte < 1460 & !is.na(cf_score_rep)) %>% # exclude polls conducted more than 4 years (max time to previous election)
  group_by(state, cycle) %>% 
  mutate(n_poll = n()) %>% 
  ungroup() %>% 
  filter(n_poll >= 5) %>% 
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

# election-level data 
election_data_dem <- polls_dem %>%
  group_by(state_year, state_year_int, cycle, state, vote2_rep, cf_score_dem) %>%
  summarise() %>%
  ungroup()

election_data_rep <- polls_rep %>%
  group_by(state_year, state_year_int, cycle, state, vote2_rep, cf_score_rep) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv_dem <- as.rv(resStan_dem)
postrv_rep <- as.rv(resStan_rep)


# Election day bias -------------------------------------------------------

# dem
p0_r_dem <- ilogit.rv(logit.rv(election_data_dem$vote2_rep) +
                    postrv_dem$mu_alpha + 
                      postrv_dem$beta2*election_data_dem$cf_score_dem +
                      postrv_dem$alpha_sc*postrv_dem$sig_alpha)

b0_r_dem <- p0_r_dem - election_data_dem$vote2_rep

b0_dem_summary <- summary(b0_r_dem)
b0_dem_summary$state_year_int <- seq(1:nrow(b0_dem_summary))
b0_dem_summary <- merge(b0_dem_summary, election_data_dem, by = "state_year_int")

saveRDS(b0_dem_summary, "~/results_vis/us_senate_predictable/us_senate_b0_cf_dem.RDS")
rm(p0_r_dem, b0_r_dem, b0_dem_summary)

# rep
p0_r_rep <- ilogit.rv(logit.rv(election_data_rep$vote2_rep) +
                        postrv_rep$mu_alpha + 
                        postrv_rep$beta2*election_data_rep$cf_score_rep +
                        postrv_rep$alpha_sc*postrv_rep$sig_alpha)

b0_r_rep <- p0_r_rep - election_data_rep$vote2_rep

b0_rep_summary <- summary(b0_r_rep)
b0_rep_summary$state_year_int <- seq(1:nrow(b0_rep_summary))
b0_rep_summary <- merge(b0_rep_summary, election_data_rep, by = "state_year_int")

saveRDS(b0_rep_summary, "~/results_vis/us_senate_predictable/us_senate_b0_cf_rep.RDS")


# Average expected EDB ----------------------------------------------------

avg_b0_cf_dem <- rv(length = nrow(election_data_dem))

for(i in 1:nrow(election_data_dem)){
  b0_sim_r_i <- sapply(election_data_dem$vote2_rep, 
                       function(x) 
                         ilogit.rv(logit.rv(x) +
                                     postrv_dem$mu_alpha  + 
                                     postrv_dem$beta2*election_data_dem$cf_score_dem[i]) - 
                         x)
  avg_b0_cf_dem[[i]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
}

avg_b0_cf_rep <- rv(length = nrow(election_data_rep))

for(i in 1:nrow(election_data_rep)){
  b0_sim_r_i <- sapply(election_data_rep$vote2_rep, 
                       function(x) 
                         ilogit.rv(logit.rv(x) +
                                     postrv_rep$mu_alpha  + 
                                     postrv_rep$beta2*election_data_rep$cf_score_rep[i]) - 
                         x)
  avg_b0_cf_rep[[i]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
}

# summary
avg_b0_cf_dem_summary <- summary(avg_b0_cf_dem)
avg_b0_cf_dem_summary$state_year_int <- 1:nrow(election_data_dem)
avg_b0_cf_dem_summary <- merge(avg_b0_cf_dem_summary, election_data_dem, 
                               by = "state_year_int")

avg_b0_cf_rep_summary <- summary(avg_b0_cf_rep)
avg_b0_cf_rep_summary$state_year_int <- 1:nrow(election_data_rep)
avg_b0_cf_rep_summary <- merge(avg_b0_cf_rep_summary, election_data_rep, 
                               by = "state_year_int")

# save
saveRDS(avg_b0_cf_dem_summary, "~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_cf_dem.RDS")
saveRDS(avg_b0_cf_rep_summary, "~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_cf_rep.RDS")


