# Plotting Figure 6: Tair vs gsw correlation (Li600)
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 




Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  ggplot(aes(x=Tref, y=gsw, group=Site))+
  geom_point(aes(color=Tdepr))+
  ylab(expression(g[sw]))+ # axes labels
  xlab(expression(T[air]~plain("(°C)")))+
  theme_light(base_family = "Times", base_size=13)+
  scale_color_viridis_c(option='turbo')+
  labs(color = expression(T[depr]~(T[leaf]-T[air]))) #legend title
