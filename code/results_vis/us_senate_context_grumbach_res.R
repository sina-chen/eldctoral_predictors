#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   State Democracy Score
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
  # library(tidyr)
}

setnsims(10000)


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")

# simulation results
# resStan <- readRDS('~/fit_stan/us_senate_predictable/resStan_us_senate_context1990_2022_grumbach.RDS')


# Functions ---------------------------------------------------------------

# extra rv functins
ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x) # taken from Bon et al. (2019)
logit.rv <- function(x) rvmapply(FUN = logit, x) # taken from Bon et al. (2019)

# log.rv <- function(x) rvmapply(FUN = log, x) 
# exp.rv <- function(x) rvmapply(FUN = exp, x) 


# Preparation -------------------------------------------------------------

# compute election groups
polls <- polls %>%
  filter(t < 1460 & !is.na(democarcy_mcmc)) %>% # exclude polls conducted more than 4 years (max time to previous election)
  group_by(state, cycle) %>%
  mutate(n_poll = n()) %>%
  ungroup() %>%
  filter(n_poll >= 5) %>%
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle),
         state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(t)/max(as.numeric(t)))

# Election-level data 
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle, state, vote2_rep, democarcy_mcmc) %>%
  summarise() %>%
  ungroup()

# election identifier
r_id <- polls$state_year_int

# convert simulations to random variable (rv) obj.
postrv <- as.rv(resStan)

# turnout
eval_points <- quantile(election_data$democarcy_mcmc,probs = seq(0,1,0.1))


# Compute estimates -------------------------------------------------------


#### Election level excess variance ####

# ed mean estimates
p0_r <- ilogit.rv(logit.rv(election_data$vote2_rep) +
                     postrv$alpha)

# edb
b0_r <- p0_r - election_data$vote2_rep

# edb simulations
b0_sim_r <- rv(length = length(eval_points))

for(i in 1:length(eval_points)){
  b0_sim_r_i <- rv(nrow(election_data))
  for(j in 1:nrow(election_data)){
    b0_sim_r_i[j] <-  ilogit.rv(logit.rv(election_data$vote2_rep) + 
                                  postrv$mu_alpha + 
                                  postrv$beta2*eval_points[i] + 
                                  postrv$alpha_sc[j]*postrv$sigma_alpha) -
      election_data$vote2_rep[j]
  }
  b0_sim_r[i] <- mean(b0_sim_r_i)
}







#### Marginal effect ####

# poll level election day marginal effect on variance
me_i <- exp.rv(log.rv(srs2_0i) + postrv$phi[r_id] + 
                 postrv$gamma*election_data$turnout_vep[r_id]) * postrv$gamma

# poll level election day marginal effect on sd
me_sd_i <- sqrt(me_i)

# election level election day marginal effect on sd
me_r <- rv(length = nrow(election_data))
for(i in 1:nrow(election_data)){
  me_r[i] <- mean(me_sd_i[r_id==i])
}

# average marginal effect on sd
ame <- mean(me_r)
ame_summary <- summary(ame)

# save 
saveRDS(ame_summary, "~/results_vis/us_senate_predictable/ame_sd_senate_turnout.RDS")

ggplot(ex_r_summary
       , aes(x = turnout_vep, y = mean)) +
  geom_point() +
  geom_segment(aes(x = turnout_vep, 
                   xend = turnout_vep, 
                   y = `2.5%`, yend = `97.5%`)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  geom_smooth(method = "lm")





