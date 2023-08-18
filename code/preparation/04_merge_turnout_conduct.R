#-------------------------------------------------------------------------------
#
# Electoral predictors - Merge turnout, state control & state demoicracy index
#   prepare context data
#
# Authore: Sina Chen
# Source: ballopedia, Jake Grumbach, election project
#
#-------------------------------------------------------------------------------

# Libraries ---------------------------------------------------------------

{
  library(dplyr)
  library(usdata)
  library(readr)
  library(readxl)
  library(stringr)
}



# Data --------------------------------------------------------------------

# polls
polls <- readRDS("data/polls/polls1990_2022_fte_scores.RDS") # polls

# turnout
turnout <- readRDS("data/election_features/senate_turnout1990_2022.RDS") # turnout

# state control
state_control <- readRDS("data/election_features/us_senate_state_control.RDS") # state control

# state demorcacy index
grumbach <- read_csv("data/election_features/grumbach_state_democracy_index.csv") # state democracy index


#-------------------------------------------------------------------------------

# Preparation -------------------------------------------------------------

# compute covariates 
polls <- polls  %>% 
  rename("minority_dem" = minority_final_dem,
         "minority_rep" = minority_final_rep,
         "gender_dem" = gender_final_dem,
         "gender_rep" = gender_final_rep) %>% 
  group_by(state, cycle) %>% 
  mutate(front_rep = sum(if_else(end_date == min(end_date) & 
                                   pct2_rep >= 0.525,1,0)),
         front_dem = sum(if_else(end_date == min(end_date) & 
                                   pct2_rep < 0.475,1,0)),
         early_date3 = list(sort(end_date, decreasing = F)[1:3]),
         early_date5 = list(sort(end_date, decreasing = F)[1:5])) %>% 
  ungroup() %>% 
  mutate(sample_size = as.double(sample_size),
         pct_dem = as.numeric(pct_dem)/100,
         pct_rep = as.numeric(pct_rep)/100,
         inc = case_when(candidate_name_rep == senator ~ "Rep. incumbent",
                         candidate_name_dem == senator ~ "Dem. incumbent",
                         TRUE ~ "Open seat") %>% 
           factor(levels = c("Dem. incumbent","Open seat",  "Rep. incumbent")),
         minority = case_when(minority_rep == 1 & minority_dem == 0 ~ 
                                "Rep. minority",
                              minority_rep == 0 & minority_dem == 1 ~ 
                                "Dem. minority",
                              minority_rep == 0 & minority_dem == 0 | 
                                minority_rep == 1 & minority_dem == 1 ~ 
                                "Neither/both minority") %>% 
           factor(levels = c("Dem. minority",  "Rep. minority", 
                             "Neither/both minority")),
         minority4 = case_when(minority_rep == 1 & minority_dem == 0 ~ 
                                 "Rep. minority",
                               minority_rep == 0 & minority_dem == 1 ~ 
                                 "Dem. minority",
                               minority_rep == 0 & minority_dem == 0 ~ 
                                 "Neither minority", 
                               minority_rep == 1 & minority_dem == 1 ~  
                                 "Both minority") %>% 
           factor(levels = c("Dem. minority", "Rep. minority", "Both minority",
                             "Neither minority")),
         gender = case_when(gender_rep == 1 & gender_dem == 0 ~ "Rep. female",
                            gender_rep == 0 & gender_dem == 1 ~ "Dem. female",
                            gender_rep == 0 & gender_dem == 0 | 
                              gender_rep == 1 & gender_dem == 1 ~ 
                              "Neither/both female") %>% 
           factor(levels = c("Dem. female", "Neither/both female", "Rep. female")),
         gender4 = case_when(gender_rep == 1 & gender_dem == 0 ~ "Rep. female",
                             gender_rep == 0 & gender_dem == 1 ~ "Dem. female",
                             gender_rep == 0 &  gender_dem == 0 ~ 
                               "Neither female", 
                             gender_rep == 1 & gender_dem == 1 ~ "Both female") %>% 
           factor(levels = c("Dem. female", "Rep. female", "Neither female",
                             "Both female")),
         front = case_when(front_rep == 0 & front_dem >= 1 ~ "Dem. front runner",
                           front_rep == 0 & front_dem == 0 |
                             front_rep == 1 & front_dem == 1 ~ "No front runner",
                           front_rep >= 1 & front_dem == 0 ~ "Rep. front runner"),
         state_abb = state2abbr(state),
         early_poll3 = if_else(end_date %in% unlist(early_date3), 1, 0),
         early_poll5 = if_else(end_date %in% unlist(early_date5), 1, 0)) %>% 
  select(-c(early_date3, early_date5)) %>% 
  group_by(state, cycle) %>% 
  mutate(front3 = case_when(all(pct2_rep[which(early_poll3 == 1)] > 0.525) ~ "Rep. front runner",
                            all(pct2_dem[which(early_poll3 == 1)] > 0.525) ~ "Dem. front runner",
                            TRUE ~ "No front runner"),
         front5 = case_when(all(pct2_rep[which(early_poll5 == 1)] > 0.525) ~ "Rep. front runner",
                            all(pct2_dem[which(early_poll5 == 1)] > 0.525) ~ "Dem. front runner",
                            TRUE ~ "No front runner")) %>% 
  ungroup() %>% 
  select(-c(question_id, early_poll3, early_poll5))  %>% 
  group_by(state, cycle) %>% 
  mutate(sample_size = if_else(is.na(sample_size) == T, 
                               mean(sample_size, na.rm = T), 
                               sample_size)) %>% 
  ungroup()

# turnout
turnout <- turnout %>% 
  mutate(state = str_to_title(state))

polls <- merge(polls, turnout, all.x = T, by.x = c("state", "cycle"), 
               by.y = c("state", "year"))
rm(turnout)

# state control
state_control <- state_control %>% 
  mutate(state_abb = state2abbr(state))

polls <- merge(polls, state_control, by.x = c("state","state_abb", "cycle"), 
               by.y = c("state", "state_abb", "year"), all.x = T)

rm(state_control)

# state democracy index
polls <- merge(polls, grumbach[,c("state", "year", "democracy_mcmc")], 
               by.x = c("state", "cycle"), by.y = c("state", "year"), all.x = T)

rm(grumbach)


# Save data ---------------------------------------------------------------

saveRDS(polls, "data/polls/us_senate_polls1990_2022_context.RDS")
