clipr::read_clip_tbl() %>% 
  mutate(点位 = Route_restitute(路线A)) %>% 
  separate_rows(点位,sep = "—") %>% 
  left_join(PointData) %>%
  summarise(hasmora = sum(有摩拉, na.rm = T),
            nomora = sum(无摩拉, na.rm = T))
  
map_dbl(1:1e6, ~{
  sum(
    sample(1:2, size = 38, prob = c(.5,.5), replace = T),
    sample(1:2, size = 89, prob = c(.65,.35), replace = T)
  ) * 420
}) %>% (\(x) c(mean = mean(x), quantile(x, c(.025,.975))))

