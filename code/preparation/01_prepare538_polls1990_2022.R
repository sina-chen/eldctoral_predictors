#-------------------------------------------------------------------------------
#
# Electoral predictors - Preprocess historical 538 US Senate pre-election poll data 1990 -2022
#   - merge 1990-2020 with 2022
#   - merge covariate info on election results, third party, general election, gender & race 
#
# Author: Sina Chen
# Source: 538
# 
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(data.table)
library(fuzzyjoin)
library(usdata)


# Data --------------------------------------------------------------------

# polls long 1990-2020
polls1990_2020_long <- read_csv("data/polls/senate_polls_all.csv", col_names = T)[-1,]

# polls 2022 long
polls2022_long <- read_csv("data/polls/senate_polls538_2022.csv")

# elections in which third party received at least 5%
third_party1990_2020 <- read_excel("data/election_features/senate_third_party.xlsx")

# indicator whether 
#   candidate was one of the 2 major candidates in the general election and
#   whether there are more than one candidate from the same party
general <- read.csv("data/election_features/candidates_general_fte.csv", sep = ";")

# race_gender (preliminary)
minority_gender <- readRDS("data/election_features/us_senate_minority_gender1990_2022_final.RDS")

# election results 1990-2020
load("data/election_features/mit_senate_results_1976_2020.RData")
res1990_2020 <- x
rm(x)

# election result 2022
res2022 <- readRDS("data/election_features/us_senate_result2022.RDS")

# incumbents 
incumbents <-read_delim("data/election_features//incumbents_senate1990_2022.csv", delim = ";")


#-------------------------------------------------------------------------------


# Preparation -------------------------------------------------------------

# filter relevant polls form 1990-2020
polls1990_2020_long <- polls1990_2020_long %>%  
  filter(cycle %in% seq(1990, 2020, 2)  & 
           candidate_party %in% c("REP", "DEM") & 
           stage == "general" &
           candidate_name != "Generic Candidate") %>% 
  rename("party" = candidate_party)

# filter relevant polls form 2022
polls2022_long <- polls2022_long %>%  
  filter(party %in% c("REP", "DEM") & 
           stage == "general" &
           cycle == "2022" &
           !(state %in% c("Alaska", "Georgia", "Utah")) &   # Alaska 2 Rep. candidates, Georgia special election dec 2022, utah independent cand 2.
           !candidate_name %in% c("Andy Biggs",
                                  "Bryant Messner",
                                  "Chris Sununu",
                                  "Corey Lewandowski",
                                  "Doug Ducey",
                                  "John Sidney McCain",
                                  "Kari Lake",
                                  "Kelli Ward",
                                  "Kelly Ayotte",
                                  "Kimberly Yee",
                                  "Larry Hogan",
                                  "Lauren Boebert",
                                  "Roy Blunt",
                                  "Josh Mandel",
                                  "Jane Timken",
                                  "Sam Brown",
                                  "Vicky Hartzler",
                                  "Eric Greitens",
                                  "Richard Sean Parnell",
                                  "Jeffrey A. Bartos",
                                  "Mark Walker",
                                  "Pat McCrory",
                                  "Marjorie Knott Eastman",
                                  "Jim Lamon",
                                  "Michael T. McGuire",
                                  "Ron Hanks",
                                  "Eli Bremer",
                                  "Gino Campana",
                                  "Peter Lumaj",
                                  "Themis Klarides",
                                  "Charles W. Morse",
                                  "Bruce Fenton",
                                  "Kevin H. Smith",
                                  "Mark Brnovich",
                                  "Christina Nolan",
                                  "Alan Grayson",
                                  "Amy Leigh Acton",
                                  "Jay Nixon",
                                  "Patrick J. Leahy",
                                  "Sarah Godlewski",
                                  "Kendra Horn",
                                  "Scott Sifton",
                                  "Lucas Kunce",
                                  "Tom Nelson",
                                  "Alex Lasry",
                                  "Abby Finkenauer")) 
  

# merge 1990-2020 with 2022 polls
polls1990_2022_long <- rbind(polls1990_2020_long, polls2022_long %>% 
                               select(colnames(polls2022_long)[colnames(polls2022_long) %in% 
                                        colnames(polls1990_2020_long)]))

rm(polls1990_2020_long, polls2022_long)

# special election dummy with 1 indicating a special election
polls1990_2022_special <- polls1990_2022_long %>% 
  mutate(special = if_else(state == "Hawaii" & cycle == 1990 |
                             state == "Indiana" & cycle == 1990 |
                             state == "North Dakota" & cycle == 1992 & candidate_name %in% c("Kent Conrad", "Jack Dalrymple") |
                             state == "California" & cycle == 1992 & candidate_name %in% c("Dianne Feinstein", "John Seymour") |
                             cycle %in% c(1993, 2013, 2017) |
                             state == "Tennessee" & cycle == 1994 & candidate_name %in% c("Fred Thompson", "Jim Cooper") |
                             state == "Oklahoma" & cycle == 1994 |
                             state == "Kansas" & cycle == 1996 & candidate_name %in% c("Sam Brownback", "Jill Docking") |
                             state == "Georgia" & cycle == 2000 |
                             state == "Missouri" & cycle == 2002 |
                             state == "Wyoming" & cycle == 2008 & candidate_name %in% c("John Barrasso", "Nicholas H. Carter") |
                             state == "Mississippi" & cycle == 2008 & candidate_name %in% c("Roger F. Wicker", "Ronnie Musgrove") |
                             state == "West Virginia" & cycle == 2010 |
                             state == "Massachusetts" & cycle == 2010 |
                             state == "Delaware" & cycle == 2010 |
                             state == "New York" & cycle == 2010 & candidate_name %in% c("Kirsten E. Gillibrand", "Joseph J. DioGuardi",
                                                                                         "Harold E. Ford Jr.", "George Pataki", 
                                                                                         "Bruce Blakeman", "David Robert Malpass",
                                                                                         "William Colridge Thompson") |
                             state == "South Carolina" & cycle == 2014 & candidate_name %in% c("Tim Scott", "Joyce Dickerson",
                                                                                               "Jill Bossi") |
                             state == "Hawaii" & cycle == 2014 |
                             state == "Oklahoma" & cycle == 2014 & candidate_name %in% c("James Lankford", "Constance N. Johnson", 
                                                                                         "Jim Rogers", "Mark Beard") |
                             state == "Minnesota" & cycle == 2018 & candidate_name %in% c("Tina Smith", "Karin Housley", "Sarah Wellington",
                                                                                          "Jerry Trooien") |
                             state == "Mississippi" & cycle == 2018 & candidate_name %in% c("Cindy Hyde-Smith", "Mike Espy", 
                                                                                            "Chris McDaniel", "David Baria",
                                                                                            "Jason Shelton", "Tobey Bartee") |
                             state == "Arizona" & cycle == 2020 | 
                             state == "Georgia" & cycle == 2020 & candidate_name %in% c("Raphael Warnock", "Kelly Loeffler", "Doug Collins",
                                                                                        "Matt Lieberman", "Ed Tarver",  
                                                                                        "Brian Richard Slowinski", "A. Wayne Johnson", 
                                                                                        "Richard Dien Winfield", "Jon Ossoff", 
                                                                                        "David A. Perdue", "Joy Felicia Slade", 
                                                                                        "Valencia Stovall", "Al Bartell", "John Fortuin", 
                                                                                        "Tamara Y. Johnson-Shealey", "Deborah A. Jackson", 
                                                                                        "Annette L. Davis Jackson", "Michael Todd Greene",
                                                                                        "Derrick E. Grayson", "Allen Buckley", "Jamesia James", 
                                                                                        "Kandiss Taylor", "Teresa Tomlinson"), 1, 0  
                             
  ))
rm(polls1990_2022_long)

# third party candidate dummy with 1 indicating a third party candidate received >= 5% 
third_party1990_2020 <- third_party1990_2020 %>% 
  group_by(Year, State) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(third_party = 1)

polls1990_2022_third <- merge(polls1990_2022_special, third_party1990_2020[, c("State", "Year", "third_party")], by.x = c("state", "cycle"), 
              by.y = c("State", "Year"), all.x = T) %>% 
  mutate(third_party = if_else(!is.na(third_party), 1, 0))
rm(polls1990_2022_special)

# general candidate dummy with 1 indicting running in the general election
# multi candidate dummy with 1 indicating that more than one candidate from the same party

polls1990_2022_multi <- merge(polls1990_2022_third, general, 
                             by.x = c("candidate_name", "state", "cycle", 
                                      "party"), 
                             by.y = c("candidate_name", "state", "cycle", 
                                      "candidate_party"), all.x = T)
rm(polls1990_2022_third)

# filter relevant polls 
polls1990_2022_clean <- polls1990_2022_multi %>% 
  filter((multi_cand == 0 & 
           general == 1 & 
           special == 0 &
           third_party == 0)|cycle == "2022") %>% 
  group_by(question_id) %>% 
  mutate(n_question = n()) %>% 
  ungroup() %>% 
  filter(n_question > 1) %>% 
  select(-n_question)
rm(polls1990_2022_multi)

# add number of polls per candidate and election
polls1990_2022_n <- polls1990_2022_clean %>% 
  group_by(cycle, state, candidate_name) %>% 
  mutate(n_poll = n()) %>% 
  ungroup() %>% 
  filter(n_poll >= 5)

rm(polls1990_2022_clean, third_party1990_2020, general)

#-------------------------------------------------------------------------------

# Gender & race -----------------------------------------------------------

# remove middle name from race gender
minority_gender_clean <- minority_gender %>% 
  select(-c(party, gender_old, minority_old, last, first, jpgs, face_male)) %>% 
  unique() %>% 
  rename("candidate_name" = name)

# adjust candiadte names
polls1990_2022_n <- polls1990_2022_n %>% 
  mutate(candidate_name = candidate_name %>%            
           str_remove_all("[A-Z][.] | Jr[.]| MD") %>% 
           str_replace_all("J.Vance", "J.D. Vance")%>% 
           str_replace_all("Art Small", "Arthur Small")%>% 
           str_replace_all("Alfonse D'Amato", "Al D'Amato") %>% 
           str_replace_all("Benjamin Cardin", "Ben Cardin") %>% 
           str_replace_all("Moseley-", "Moseley ") %>% 
           str_replace_all("Chele Farley", "Chele Chiavacci Farley")  %>% 
           str_replace_all("á", "a") %>% 
           str_replace_all("Barbara Goolsbee Bollier", "Barbara Bollier")  %>% 
           str_replace_all("Bill Redmond", "William Redmond")  %>% 
           str_replace_all("Bob Flanders", "Robert Flanders") %>% 
           str_replace_all("Charles Bradley Hutto", "Brad Hutto") %>% 
           str_replace_all("Chris Van Hollen", "Chris VanHollen")  %>% 
           str_replace_all("Christopher Bond", "Kit Bond") %>% 
           str_replace_all("Christopher Coons", "Chris Coons") %>% 
           str_replace_all("Christopher Dodd", "Chris Dodd") %>% 
           str_replace_all("Christopher Murphy", "Chris Murphy") %>% 
           str_replace_all("Christopher Rothfuss", "Chris Rothfuss") %>% 
           str_replace_all("Daniel Coats", "Dan Coats") %>% 
           str_replace_all("Douglas Forrester", "Doug Forrester") %>% 
           str_replace_all("Pipkin", "E Pipkin") %>% 
           str_replace_all("Ed Bernstein", "Edward Bernstein") %>% 
           str_replace_all("Edward Kennedy", "Ted Kennedy") %>% 
           str_replace_all("Edward Markey", "Ed Markey") %>% 
           str_replace_all("Gordon Douglas Jones", "Doug Jones") %>% 
           str_replace_all("Hillary Rodham Clinton", "Hillary Clinton") %>% 
           str_replace_all("Jacob Hoogendyk", "Jack Hoogendyk") %>% 
           str_replace_all("James Inhofe", "Jim Inhofe") %>% 
           str_replace_all("James Jeffords", "Jim Jeffords") %>% 
           str_replace_all("James Oberweis", "Jim Oberweis") %>% 
           str_replace_all("James Risch", "Jim Risch") %>% 
           str_replace_all("Jeffrey Beatty", "Jeff Beatty") %>% 
           str_replace_all("John Neely Kennedy", "John Kennedy") %>% 
           str_replace_all("John Wright Hickenlooper", "John Hickenlooper") %>% 
           str_replace_all("Jon Stevens Corzine", "Jon Corzine") %>% 
           str_replace_all("Joseph Biden", "Joe Biden") %>% 
           str_replace_all("Joseph Heck", "Joe Heck") %>% 
           str_replace_all("Joseph Lieberman", "Joe Lieberman") %>% 
           str_replace_all("Kurt Patrick Bills", "Kurt Bills") %>% 
           str_replace_all("Margaret Wood Hassan", "Maggie Hassan") %>% 
           str_replace_all("Marvin Bailey Scott", "Marvin Scott") %>% 
           str_replace_all("Mary Jennings Hegar", "MJ Hegar") %>% 
           str_replace_all("Michael DeWine", "Mike DeWine") %>% 
           str_replace_all("Michael Enzi", "Mike Enzi") %>% 
           str_replace_all("Misty Katherine Snow", "Misty Snow") %>% 
           str_replace_all("Paul Van Dam", "Paul VanDam") %>% 
           str_replace_all("Phil Giordano", "Philip Giordano") %>% 
           str_replace_all("Richard Durbin", "Dick Durbin") %>% 
           str_replace_all("Rikin Mehta", "Rik Mehta") %>% 
           str_replace_all("Robert Bennett", "Bob Bennett") %>% 
           str_replace_all("Robert Casey", "Bob Casey") %>% 
           str_replace_all("Robert Gerald Lorge", "Robert Lorge") %>% 
           str_replace_all("Robert Menendez", "Bob Menendez") %>% 
           str_replace_all("Robert Tuke", "Bob Tuke") %>% 
           str_replace_all("Rodney Britz Glassman", "Rodney Glassman") %>% 
           str_replace_all("Stevan Pearce", "Steve Pearce") %>% 
           str_replace_all("Thomas Allen", "Tom Allen") %>% 
           str_replace_all("Thomas Carper", "Tom Carper") %>% 
           str_replace_all("Thomas Roland Tillis", "Thom Tillis") %>% 
           tolower(),
         party = tolower(party)) 



# merge race and gender
polls1990_2022_minority_gender <- fuzzy_left_join(polls1990_2022_n, 
                                              minority_gender_clean, 
                         by = "candidate_name", match_fun = str_detect) %>% 
  rename(candidate_name = candidate_name.x) %>% 
  select(-c(candidate_name.y))

# check for missing information on race or gender
polls1990_2022_minority_gender[which(
  is.na(polls1990_2022_minority_gender$minority_final)),] %>% 
  select(cycle, state, candidate_name, minority_final, gender_final) %>% 
  unique()

polls1990_2022_minority_gender[which(
  is.na(polls1990_2022_minority_gender$gender_final)),] %>% 
  select(cycle, state, candidate_name, minority_final, gender_final) %>% 
  unique()

rm(minority_gender, minority_gender_clean, polls1990_2022_n)



# Incumbency --------------------------------------------------------------

polls1990_2022_inc <- merge(polls1990_2022_minority_gender, incumbents, 
                            by = c("cycle", "state"), all.x = T) %>% 
  rename(senator = incumbent) %>% 
  mutate(senator = str_remove_all(senator, "[A-Z][.] | III| Jr[.]") %>% 
           str_replace_all("Mark Hatfield", "Mark Odom Hatfield") %>% 
           str_replace_all("Chuck Robb", "Charles Robb") %>% 
           str_replace_all("Chuck Schumer", "Charles Schumer") %>% 
           str_replace_all("Joe Manchin", "Joe Manchin, III") %>% 
           str_replace_all("Pat Toomey", "Patrick Toomey") %>% 
           str_replace_all("Fritz Hollings", "Ernest Hollings"),
         inc = if_else(senator == candidate_name, 1, 0))

rm(incumbents, polls1990_2022_minority_gender)


# Election results --------------------------------------------------------

# pre process results
res1990_2020 <- res1990_2020 %>% 
  mutate(special = if_else(state == "ILLINOIS" & year == 2010, FALSE, as.logical(special))) %>% 
  subset(party_simplified %in% c("REPUBLICAN", "DEMOCRAT") &
           special == F & writein == F & stage == "gen" & 
           year %in% seq(1990, 2020, 2)) %>% 
  select(state, year, party_simplified, candidatevotes, totalvotes) %>% 
  rename(cycle = year,
         party = party_simplified) %>% 
  mutate(state = str_to_title(state),
         party = if_else(party == "REPUBLICAN", "rep", 
                                   "dem"),
         vote = candidatevotes/totalvotes) %>% 
  group_by(cycle, state, party) %>% 
  summarise(vote = max(vote))

# merge 1990-2020
polls1990_2022_result <- merge(polls1990_2022_inc, res1990_2020, 
                               by = c("cycle", "state", "party"), 
                     all.x =T)


rm(polls1990_2022_inc, res1990_2020)


# US regions --------------------------------------------------------------


polls1990_2022_geo <- polls1990_2022_result %>% 
  mutate(state_short = state2abbr(state),
         division = case_when(state_short %in% c("WA", "OR", "CA", "HI", "AK") ~ "Pacific", 
                            state_short %in% c("MT", "ID", "NV", "WY", "UT", 
                                               "AZ", "CO", "NM") ~ "Mountain",
                            state_short %in% c("ND", "SD", "NE", "KS", "MN", 
                                               "IA", "MO") ~ "West North Central", 
                            state_short %in% c("WI", "IL", "MI", "IN", "OH") ~ "East North Central",
                            state_short %in% c("NY", "PA", "NJ") ~ "Middle Atlantic", 
                            state_short %in% c("VT", "NH", "MA", "CT", "RI", 
                                               "ME") ~ "New England",
                            state_short %in% c("OK", "TX", "AR", "LA") ~ "West South Central", 
                            state_short %in% c("KY", "TN", "MS", "AL") ~ "East South Central", 
                            state_short %in% c("WV", "VA", "MD", "DE", "NC", "SC", "GA", 
                                               "FL") ~ "South Atlantic"),
         region = case_when(state_short %in% c("WA", "OR", "CA", "HI", "AK", 
                                                 "MT", "ID", "NV", "WY", "UT", 
                                                 "AZ", "CO", "NM") ~ "West",
                              state_short %in% c("ND", "SD", "NE", "KS", "MN", 
                                                 "IA", "MO", "WI", "IL", "MI", 
                                                 "IN", "OH") ~ "Midwest",
                              state_short %in% c("NY", "PA", "NJ", "VT", "NH", 
                                                 "MA", "CT", "RI", "ME") ~ "Northeast",
                              state_short %in% c("OK", "TX", "AR", "LA", "KY", 
                                                 "TN", "MS", "AL", "WV", "VA", 
                                                 "MD", "DE", "NC", "SC", "GA", 
                                                 "FL") ~ "South"))

rm(polls1990_2022_result)



# Wide format -------------------------------------------------------------

polls1990_2022_wide <- polls1990_2022_geo %>% 
  pivot_wider(id_cols = c(state, cycle, sample_size, end_date,
                          election_date, question_id, poll_id, fte_grade, 
                          population, methodology, seat_name, internal, 
                          n_poll, senator, division, region, pollster, 
                          seat_name, race_id, partisan),
              names_from = party,
              values_from = c(pct, vote, candidate_name, inc, gender_final,
                              minority_final, italian, cuban, name_white,
                              name_prob_white, face_white, face_prob_white))

rm(polls1990_2022_geo)


# Add 2022 election result ------------------------------------------------

# prepare 2-party vote share 2022
res2022 <- res2022 %>% 
  rename("vote_dem" = dem_vote,
         "vote_rep" = rep_vote) %>% 
  select(c(state, cycle, vote_dem, vote_rep))

polls_2022_res_wide <- polls1990_2022_wide %>% 
  filter(cycle == 2022) %>% 
  select(-c(vote_rep, vote_dem)) %>% 
  merge(res2022, by = c("state", "cycle"), all.x = T)

# add 2-party vote share 2022
polls1990_2022_final <- rbind(polls1990_2022_wide %>% filter(cycle != 2022), 
                              polls_2022_res_wide) %>% 
  mutate(pct2_dem = as.numeric(pct_dem)/(as.numeric(pct_dem)+as.numeric(pct_rep)),
         pct2_rep = as.numeric(pct_rep)/(as.numeric(pct_dem)+as.numeric(pct_rep)),
         vote2_dem = vote_dem/(vote_dem+vote_rep),
         vote2_rep = vote_rep/(vote_dem+vote_rep),
         dte = difftime(as.Date(election_date, "%m/%d/%y"),
                        as.Date(end_date, "%m/%d/%y")))
rm(res2022, polls1990_2022_wide, polls_2022_res_wide)


saveRDS(polls1990_2022_final, "data/polls//us_senate_polls_fte1990_2022.RDS")


