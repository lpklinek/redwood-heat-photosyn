# Plotting Figure 7: Tdepr and PhiPS2, faceted by month, with Fv/Fm overlaid in red
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 



Li600_all_entire %>%
  group_by(month(Date)) %>%
  mutate(Tdepr = Tleaf - Tref) %>%
  dplyr::filter(Site == 'Garcia') %>%
  dplyr::filter(LightDark == 'L' | LightDark == 'D') %>%  # both light and dark-adapted points
  ggplot(aes(x=Tdepr, y=PhiPS2)) + 
  geom_point(data = . %>% filter(LightDark == 'L'),  # light-adapted points, colored by Qamb
             aes(color = Qamb, group = Site), size = 1) + 
  geom_point(data = . %>% filter(LightDark == 'D'),   # dark-adapted points, colored in red
             color = "red", size = 1, aes(group = Site)) + 
  # faceting by month, labeling
  facet_grid(cols = vars(month(Date)), 
             labeller = labeller(`month(Date)` = function(x) month.abb[as.numeric(x)])) + 
  # Qamb continuous color scale
  scale_color_viridis(name = expression(Q[amb]), discrete = FALSE) + 
  scale_y_continuous(limits = c(0, 0.85)) + #y axis limits
  scale_x_continuous(limits = c(-8, 8)) +  #x axis limits
  # axes labels
  xlab(expression(T[depr] ~ (T[leaf] - T[air]))) + 
  ylab(expression(phi[PSII])) + 
  theme_light(base_family = "Times", base_size = 13) + 
  theme(axis.title.y = element_text(size = 17)) +
  guides(color = guide_legend(override.aes = list(color = "red", size = 2), # manual legend for dark-adapted
                              title = "Condition", 
                              labels = c(expression(Q[amb]), "Dark-adapted")))


