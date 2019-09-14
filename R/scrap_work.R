# make a heat map of the round robbin performance by week, by player
head(mu_unnest_view2)

# get the times that each week a player would win against all other playes
rr_view0 <- weekly_owner_scores1 %>% 
  mutate(Result = case_when(`Points For` > `Points Against` ~ "W", 
                            TRUE ~ "L")) %>% 
  group_by(Week) %>% 
  mutate(num_over = rank(`Points For`)) %>% 
  ungroup() %>% 
  mutate(`RR Wins` = num_over - 1)

ggplot(data = rr_view0, aes(x = Owner, y = Week, fill = `RR Wins`, label = Result)) + 
  geom_tile() + 
  geom_text() + 
  scale_fill_gradient2(high = "red", low = "blue", mid = "white", midpoint = 7)
