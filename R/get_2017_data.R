#### Getting 2017 Season Data ####==============================================

# This code will be sourced by the knitr file for the 2017 season recap. It 
#  does the data pull and combines things into a roughly usable format for 
#  further analysis. 

# Load libraries
library(jsonlite)       # Pulling down JSON data
library(tidyverse)      # Tools for data analysis
library(glue)           # Sticking strings together
library(RColorBrewer)   # Color Pals
library(plotly)         # Interactive graphics
library(DT)             # JS datatables package

#### ~Gathering Data~ ####======================================================
# This section is actually going to be gather and combine. It crazy how easy 
#  this is based on Sleeper smart design of the API. Awesome. 

#### ~~Reference Data~~ ####====================================================
# Some things will not change week to week get that first. 

# user will be basis of other URLs
user_res <- fromJSON("https://api.sleeper.app/v1/user/ncriswell")
user_id <- user_res$user_id

# get 2018 season info
s17_lg_res <- fromJSON(glue("https://api.sleeper.app/v1/user/", 
                            user_id, 
                            "/leagues/nfl/2017"))
s17_lg_id <- s17_lg_res$league_id

# get the scoring rules for this season
s17_lg_rules <- data.frame(t(s17_lg_res$scoring_settings), 
                           stringsAsFactors = FALSE) %>% 
  rownames_to_column(var = "stat_code") %>% 
  rename(stat_value = X1) %>% 
  as_tibble()

# get the user
s17_users_res <- fromJSON(glue("https://api.sleeper.app/v1/league/", 
                               s17_lg_id, 
                               "/users"))

# get the rosters
s17_rosters_res <- fromJSON(glue("https://api.sleeper.app/v1/league/",
                                 s17_lg_id, 
                                 "/rosters"))

# combine the roster and the user information to get a table of user info
s17_user_vw0 <- s17_users_res %>% 
  select(display_name, user_id) %>% 
  left_join(select(s17_rosters_res, owner_id, roster_id), 
            by = c("user_id" = "owner_id"))

# Get the player data
player_res <- fromJSON("https://api.sleeper.app/v1/players/nfl")

# There are list elements in this data. That is a problem. Get only the 
#  character and numeric values. Sort of a double lapply here. Not awesome. 
player_res_no_lists <- lapply(player_res,
                              function(m) {
                                m[unlist(lapply(m,
                                                function(n)
                                                  is.character(n) | is.numeric(n)))]
                              })

# Combine this into a simple data frame
# (Note, we are calling from the data.table name space here. We should probably 
#  figure out how to do the rest of the this work in data.table eventually as
#  performance might be an issue as time goes on...)
player_view0 <- data.table::rbindlist(player_res_no_lists, fill = TRUE) %>% 
  select(player_id, last_name, first_name, team, number, position, years_exp, 
         height, weight, age, status, injury_status, injury_body_part, 
         college, hashtag) %>% 
  distinct() # not sure what happens here, but we get some dupes...

#### ~~Stats Data~~ ####========================================================

# Let's pull all 2018 week data

# Abstract season and week
working_season <- 2017

# Get the player stats data
stats_res <- lapply(1:16,
                    function(m)
                      fromJSON(
                        glue("https://api.sleeper.app/v1/stats/nfl/regular/2017/",
                             m)
                      )) 

# The stats are a list in a list in a list. Combine each week into a large 
#  data frame. 
stats_res_comb <- lapply(stats_res, function(m) bind_rows(m, .id = "player_id"))

# now combine THESE into a really large data frame
stats_view0 <- bind_rows(stats_res_comb, .id = "week")

# In 2017, the stats frame contains a lot of competely blank lines. This ends 
#  creating some issues with duplicated column names. Fix that here. 
stats_view1 <- stats_view0[, !duplicated(colnames(stats_view0))]


# Find the columns that have relavent stats in our league. We'll pull those
#  out along with the player_id
stat_cols <- c("player_id", "week",
               names(stats_view1)[names(stats_view1) %in% s17_lg_rules$stat_code])

# Now select those column and melt the data so we can stick our metric values 
#  onto it. 
stats_melt0 <- stats_view1 %>% 
  select(stat_cols) %>% 
  gather(stat_code, stat_actual, -player_id, -week) %>% 
  mutate(stat_actual = case_when(is.na(stat_actual) ~ 0, 
                                 TRUE ~ stat_actual))

# Now add the metric values
stats_melt1 <- stats_melt0 %>% 
  left_join(s17_lg_rules,
            by = "stat_code") %>% 
  mutate(stat_extended = stat_actual * stat_value)

# Get player scores by grouping all of this together at the week and player 
#  level.
player_stats_sum0 <- stats_melt1 %>% 
  group_by(week, player_id) %>% 
  summarise(player_score = sum(stat_extended))

# Combine this with the player names so we can check some shit
player_stats_sum1 <- player_stats_sum0 %>% 
  left_join(player_view0, by = "player_id")

colMeans(is.na(player_stats_sum1))

#### ~~Matchup Data~~ ####======================================================
# Get the matchup data
mu_res <- lapply(1:16, 
                 function(m) fromJSON(glue("https://api.sleeper.app/v1/league/", 
                                           s17_lg_id, 
                                           "/matchups/", m)))

mu_comb0 <- bind_rows(mu_res, .id = "week") %>% as_tibble() %>% 
  filter(!is.na(matchup_id))

# get rid of some useless columns
mu_comb1 <- mu_comb0 %>% 
  select(-players, -custom_points)

# unnest the starters
mu_unnest_view0 <- mu_comb1 %>% 
  unnest() %>% 
  rename(player_id = starters)

# now join the player stats onto the matchup info
mu_unnest_view1 <- mu_unnest_view0 %>% 
  left_join(player_stats_sum1, 
            by = c("week", "player_id"))

# check for missing values. my god, we might be there
colMeans(is.na(mu_unnest_view1))
# holy shit, I think this is working. 

# add in the team information
mu_unnest_view2 <- mu_unnest_view1 %>% 
  left_join(s17_user_vw0, by = "roster_id")

