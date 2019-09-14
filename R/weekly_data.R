#### Weekly Data Pull ####======================================================

# Code to pull information on any week of any season. It will support a range
#  of weeks but will only support one season for now. 

#### Setup ####=================================================================

setwd("C:/sleeper_fantasy")

# Source global code that loads packages, pulls standardized refernces data, 
#  and creates some utility functions. 
source("R/global.R")

# Get league info for 2019 season
s19_info <- F_get_lg_info(user_id = user_id, season = 2019)

# unload some information from this
lg_id <- s19_info$lg_id
lg_owners <- s19_info$user_vw0
lg_rules <- s19_info$lg_rules

#### Combining Data ####========================================================

# pull matchup and stat information

mu_out <- F_get_weekly_matchup(season = 2019, week = 1)
mu_view0 <- mu_out$mu_view0

stat_out <- F_get_weekly_player_stats(season = 2019, week = 1)
stat_sum0 <- stat_out$player_stats_sum0
stat_melt0 <- stat_out$stats_melt1

# combine the matchup and stat data
mu_view1 <- mu_view0 %>% 
  left_join(stat_sum0, 
            by = c("player_id" = "player_id", 
                   "week" = "week"))

# add in the owner name
mu_view2 <- mu_view1 %>% 
  left_join(lg_owners, 
            by = c("roster_id" = "roster_id"))

# add in player information
mu_view3 <- mu_view2 %>% 
  left_join(player_view0, 
            by = c("player_id" = "player_id"))

# make sure the joins haven't dupped
nrow(mu_view3) == nrow(mu_view0)

#### Analysis ####==============================================================

#### ~Owner Stats~ ####=========================================================
# Get the owner by week stats
weekly_owner_scores0 <- mu_view3 %>% 
  group_by(week, display_name, starter, matchup_id) %>% 
  summarise(tot_points = sum(player_score, na.rm = TRUE)) %>% 
  ungroup()

# join this back onto 
weekly_owner_scores1 <- weekly_owner_scores0 %>% 
  left_join(weekly_owner_scores0, by = c("week", "matchup_id", "starter")) %>% 
  filter(display_name.x != display_name.y) %>% 
  mutate(Week = as.factor(week),
         display_name.x = as.factor(display_name.x)) %>% 
  select(Week, display_name.x, tot_points.x, tot_points.y, starter, matchup_id) %>% 
  rename(`Owner` = display_name.x, 
         `Points For` = tot_points.x, 
         `Points Against` = tot_points.y) %>% 
  arrange(Week) %>% 
  mutate(Outcome = case_when(`Points For` > `Points Against` ~ "Win", 
                             TRUE ~ "Loss"))

ggplot(data = weekly_owner_scores1 %>% filter(starter == TRUE),
       aes(x = matchup_id, y = `Points For`, fill = Outcome, label = Owner)) + 
  geom_bar( position = "dodge", stat = "identity") + 
  geom_text(position = position_dodge(width = 1),
             angle = 0, size = 3) + 
  geom_hline(yintercept = median(weekly_owner_scores1 %>% 
                                   filter(starter == TRUE) %>% 
                                   pull(`Points For`))) + 
  geom_hline(yintercept = mean(weekly_owner_scores1 %>% 
                                   filter(starter == TRUE) %>% 
                                   pull(`Points For`)),
             linetype = "dashed") + 
  scale_fill_brewer(palette = "Set1") + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle("TLST: TS - 2019 Season Week 1 Results",
          subtitle = "Teams are grouped by matchup. Solid line is week median. Dotted line is week average")


# Get the player scores by position, by player by matchup
weekly_owner_position0 <- mu_view3 %>% 
  group_by(week, display_name, starter, matchup_id, position) %>% 
  summarise(position_points = sum(player_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Week = as.factor(week), 
         display_name = as.factor(display_name)) %>% 
  rename(Owner = display_name,
         `Points For` = position_points) %>% 
  ungroup() 
  
ggplot(data = weekly_owner_position0 %>% filter(starter == TRUE),
       aes(x = Owner,  y = `Points For`, fill = position, label = round(`Points For`, 1))) + 
  geom_bar( stat = "identity") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  geom_hline(yintercept = median(weekly_owner_scores1 %>% 
                                   filter(starter == TRUE) %>% 
                                   pull(`Points For`))) + 
  geom_hline(yintercept = mean(weekly_owner_scores1 %>% 
                                 filter(starter == TRUE) %>% 
                                 pull(`Points For`)),
             linetype = "dashed") + 
  scale_fill_brewer(palette = "Set1") + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(),
        axis.title.y = element_blank()) + 
  ggtitle("TLST: TS - 2019 Season Week 1 Owner Position Scores",
          subtitle = "Best_Bod: Worst_Kicker AMIRITE!?!?!?")


# Ok make the stack as a percentage
weekly_owner_position_p0 <- weekly_owner_position0 %>% 
  filter(starter == TRUE) %>% 
  group_by(Owner, week) %>% 
  mutate(p_score = `Points For` / sum(`Points For`))


ggplot(data = weekly_owner_position_p0, 
       aes(x = Owner,  y = `p_score`, 
           fill = position, 
           label = percent(round(`p_score`, 2)))) + 
  geom_bar( stat = "identity", colour = "black") + 
  geom_text(position = position_stack(vjust = 0.5),
            size = 3) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_y_continuous(name = "% of Total Score", 
                     labels = percent) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(),
        axis.title.y = element_blank()) + 
  ggtitle("TLST: TS - 2019 Season Week 1 Owner Position Scores",
          subtitle = "Best_Bod: Worst_Kicker AMIRITE!?!?!?")
