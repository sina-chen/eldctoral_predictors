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
resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_minority_population.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(dte < 1460 & !is.na(minority4) & !is.na(white_share)) %>% # exclude polls conducted more than 4 years (max time to previous election)
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
           white_share, minority_dem, minority_rep, minority4) %>%
  summarise() %>%
  ungroup()

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)

warnings()

# Election level election day bias ----------------------------------------

# election day estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                    postrv$mu_alpha + 
                    postrv$beta2*election_data$minority_dem +
                    postrv$beta3*election_data$minority_rep +
                    postrv$beta4*election_data$white_share +
                    postrv$beta5*election_data$minority_dem*election_data$minority_rep +
                    postrv$beta6*election_data$minority_dem*election_data$white_share +
                    postrv$beta7*election_data$minority_rep*election_data$white_share +
                    postrv$beta8*election_data$minority_rep*election_data$minority_dem*election_data$white_share +
                    postrv$alpha_sc*postrv$sig_alpha)

# election level election day bias
b0_r <- p0_r - election_data$vote2_rep

# election level bias summaries
b0_summary <- summary(b0_r)
b0_summary$state_year_int <- seq(1:nrow(b0_summary))
b0_summary <- merge(b0_summary, election_data, by = "state_year_int")

saveRDS(b0_summary, "~/results_vis/us_senate_predictable/us_senate_b0_minority_population.RDS")


# Average expected EDB ----------------------------------------------------

#### Dem. minority ####
dem_minority <- which(election_data$minority_dem == 1 & 
                        election_data$minority_rep == 0)
avg_b0_dem <- rv(length = length(dem_minority))
counter <- 1

for(i in dem_minority) {
  b0_sim_r_i <- sapply(dem_minority, 
                       function(x) 
                         ilogit.rv(logit.rv(election_data$vote2_rep[x]) +
                                     postrv$mu_alpha + 
                                     postrv$beta2*election_data$minority_dem[x] +
                                     postrv$beta4*election_data$white_share[i] +
                                     postrv$beta6*election_data$minority_dem[x]*election_data$white_share[i]) - 
                         election_data$vote2_rep[x])
  avg_b0_dem[[counter]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  counter <- counter + 1
  
}



# summary
avg_b0_dem_summary <- summary(avg_b0_dem)
avg_b0_dem_summary$state_year_int <- dem_minority



#### Rep. minority ####

rep_minority <- which(election_data$minority_dem == 0 & 
                        election_data$minority_rep == 1)
avg_b0_rep <- rv(length = length(rep_minority))
counter <- 1

for(i in rep_minority) {
  b0_sim_r_i <- sapply(rep_minority, 
                       function(x) 
                         ilogit.rv(logit.rv(election_data$vote2_rep[x]) +
                                     postrv$mu_alpha + 
                                     postrv$beta3*election_data$minority_rep[x] +
                                     postrv$beta4*election_data$white_share[i] +
                                     postrv$beta7*election_data$minority_rep[x]*election_data$white_share[i]) - 
                         election_data$vote2_rep[x])
  avg_b0_rep[[counter]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  counter <- counter + 1
  
}



# summary
avg_b0_rep_summary <- summary(avg_b0_rep)
avg_b0_rep_summary$state_year_int <- rep_minority



#### Both minority ####

both_minority <- which(election_data$minority_dem == 1 & election_data$minority_rep == 1)
avg_b0_both <- rv(length = length(both_minority))
counter <- 1

for(i in both_minority) {
  b0_sim_r_i <- sapply(both_minority, 
                       function(x) 
                         ilogit.rv(logit.rv(election_data$vote2_rep[x]) +
                                     postrv$mu_alpha + 
                                     postrv$beta2*election_data$minority_dem[x] +
                                     postrv$beta3*election_data$minority_rep[x] +
                                     postrv$beta4*election_data$white_share[i] +
                                     postrv$beta5*election_data$minority_dem[x]*election_data$minority_rep[x] +
                                     postrv$beta6*election_data$minority_dem[x]*election_data$white_share[i] +
                                     postrv$beta7*election_data$minority_rep[x]*election_data$white_share[i] +
                                     postrv$beta8*election_data$minority_rep[x]*election_data$minority_dem[x]*election_data$white_share[i]) - 
                         election_data$vote2_rep[x])
  avg_b0_both[[counter]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  counter <- counter + 1
  
}

# summary
avg_b0_both_summary <- summary(avg_b0_both)
avg_b0_both_summary$state_year_int <- both_minority


#### Neither minority ####

neither_minority <- which(election_data$minority_dem == 0 & election_data$minority_rep == 0)
avg_b0_neither <- rv(length = length(neither_minority))
counter <- 1

for(i in neither_minority) {
  b0_sim_r_i <- sapply(neither_minority, 
                       function(x) 
                         ilogit.rv(logit.rv(election_data$vote2_rep[x]) +
                                     postrv$mu_alpha + 
                                     postrv$beta4*election_data$white_share[i]) - 
                         election_data$vote2_rep[x])
  avg_b0_neither[[counter]] <- colMeans(do.call(rbind,b0_sim_r_i))
  print(i)
  counter <- counter + 1
  
}

# summary
avg_b0_neither_summary <- summary(avg_b0_neither)
avg_b0_neither_summary$state_year_int <- neither_minority



#### Combine ####
avg_b0_minority_population_summary <- rbind(avg_b0_dem_summary, 
                                            avg_b0_rep_summary, 
                                            avg_b0_neither_summary,
                                            avg_b0_both_summary)

avg_b0_minority_population_summary <- merge(avg_b0_minority_population_summary, 
                                            election_data, 
                                            by = "state_year_int")

# save
saveRDS(avg_b0_minority_population_summary, "~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_minority_population.RDS")


