#### Sleeper Fantasy FOotball Global ####=======================================
# Code will do things which need to be done every time we use Sleeper data

#### Setting Things Up ####=====================================================

# Load libraries
library(jsonlite)       # Pulling down JSON data
library(tidyverse)      # Tools for data analysis
library(glue)           # Sticking strings together
library(RColorBrewer)   # Color Pals
library(plotly)         # Interactive graphics
library(DT)             # JS datatables package
library(ggthemes)       # For 538 theme
library(scales)         # Labels for ggplot scales

#### Getting Data ####==========================================================

# User information is required to pull league data and will not change ever

user_res <- fromJSON("https://api.sleeper.app/v1/user/ncriswell")
user_id <- user_res$user_id

# player information will only need to be pulled once a day / once each time 
#  this is run regardless of the season or week

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
  mutate(player_name = paste0(last_name, ", ", str_sub(first_name, 1, 1))) %>% 
  distinct() %>% # not sure what happens here, but we get some dupes...
  as_tibble()

#### Utility Functions ####=====================================================

# Function to return league season information. Will return 
#   league id
#   league rules
#   league owners
F_get_lg_info <- function(user_id = user_id, 
                        season = 2019){
  
  # get league information
  lg_res <- fromJSON(glue("https://api.sleeper.app/v1/user/", 
                              user_id, 
                              "/leagues/nfl/", season))
  lg_id <- lg_res$league_id
  
  # get the scoring rules for this season
  lg_rules <- data.frame(t(lg_res$scoring_settings), 
                             stringsAsFactors = FALSE) %>% 
    rownames_to_column(var = "stat_code") %>% 
    rename(stat_value = X1) %>% 
    as_tibble()
  
  # get the users
  users_res <- fromJSON(glue("https://api.sleeper.app/v1/league/", 
                             lg_id, 
                                 "/users"))
  
  # get the rosters
  rosters_res <- fromJSON(glue("https://api.sleeper.app/v1/league/",
                               lg_id, 
                                   "/rosters"))
  
  # combine the roster and the user information to get a table of user info
  user_vw0 <- users_res %>% 
    select(display_name, user_id) %>% 
    left_join(select(rosters_res, owner_id, roster_id), 
              by = c("user_id" = "owner_id"))
  
  # things to return
  list(lg_res = lg_res, 
       lg_id = lg_id, 
       lg_rules = lg_rules, 
       user_res = user_res, 
       rosters_res = rosters_res, 
       user_vw0 = user_vw0)
  
}

F_get_weekly_player_stats <- function(season = 2019, 
                                      week = 1,
                                      .lg_rules = lg_rules) {
  
  # Get the player stats data
  stats_res <- lapply(week,
                      function(m)
                        fromJSON(
                          glue("https://api.sleeper.app/v1/stats/nfl/regular/", 
                               season, "/",
                               m)
                        )) 
  
  # The stats are a list in a list in a list. Combine each week into a large 
  #  data frame. 
  stats_res_comb <- lapply(stats_res, function(m) bind_rows(m, .id = "player_id"))
  
  # now combine THESE into a really large data frame
  stats_view0 <- bind_rows(stats_res_comb, .id = "week")
  
  # In 2017, the stats frame contains a lot of competely blank lines. This ends 
  #  creating some issues with duplicated column names. Fix that here. 
  if (season == 2017){
    stats_view0 <- stats_view0[, !duplicated(colnames(stats_view0))]
  }

  # Find the columns that have relavent stats in our league. We'll pull those
  #  out along with the player_id
  stat_cols <- c("player_id", "week",
                 names(stats_view0)[names(stats_view0) %in% .lg_rules$stat_code])
  
  # Now select those column and melt the data so we can stick our metric values 
  #  onto it. 
  stats_melt0 <- stats_view0 %>% 
    select(stat_cols) %>% 
    gather(stat_code, stat_actual, -player_id, -week) %>% 
    mutate(stat_actual = case_when(is.na(stat_actual) ~ 0, 
                                   TRUE ~ stat_actual))
  
  # Now add the metric values
  stats_melt1 <- stats_melt0 %>% 
    left_join(lg_rules,
              by = "stat_code") %>% 
    mutate(stat_extended = stat_actual * stat_value)
  
  # Get player scores by grouping all of this together at the week and player 
  #  level.
  player_stats_sum0 <- stats_melt1 %>% 
    group_by(week, player_id) %>% 
    summarise(player_score = sum(stat_extended))
  
  list(player_stats_sum0 = player_stats_sum0,
       stats_melt1 = stats_melt1)
  
}

# function to pull and unnest the matchup information
F_get_weekly_matchup <- function(season = 2019, 
                                 week = 1,
                                 .lg_id = lg_id){
  
  # pull matchup data
  mu_res <- lapply(week, 
                        function(m) fromJSON(glue("https://api.sleeper.app/v1/league/", 
                                                  .lg_id, "/matchups/", m)))
  
  # combine by week
  mu_week <- bind_rows(mu_res, .id = "week")
  
  # This is nested DF where starters and players are both nested. Unnest this
  # and identify the starters
  mu_view0 <- mu_week %>% 
    select(-custom_points) %>% 
    unnest(players, .drop = FALSE) %>% 
    rename(player_id = players) %>% 
    mutate(starter = player_id %in% unlist(starters)) %>% 
    select(-starters) %>% 
    as_tibble()
  
  list(mu_view0 = mu_view0)
  
}

