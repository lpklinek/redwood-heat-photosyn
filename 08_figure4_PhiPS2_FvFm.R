# Plotting Figure 4: DeltaT and PhiPS2, faceted by month, with Fv/Fm overlaid in red
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

ymin <- 0
ymax <- 0.83

# generating regression lines
fit_lines <- Li600_all_entire %>%
  filter(Site == "Garcia", LightDark %in% c("L", "D")) %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(Date)) %>%
  group_by(month) %>%
  do({
    model <- lm(PhiPS2 ~ Tdepr, data = .)
    rng <- range(.$Tdepr, na.rm = TRUE)
    new_x <- seq(rng[1], rng[2], length.out = 200)
    new_y <- predict(model, newdata = data.frame(Tdepr = new_x))
    keep <- which(new_y >= ymin & new_y <= ymax)
    data.frame(month = unique(.$month), Tdepr = new_x[keep], PhiPS2 = new_y[keep])
  }) %>%
  ungroup()

# note: the code below generates the figure without a legend, which we then added separately for formatting
# reasons. to generate WITH a legend, remove the theme(legend.position="none")

fig4 <- Li600_all_entire %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(Date)) %>%
  group_by(month) %>%
  dplyr::filter(Site == 'Garcia') %>%
  dplyr::filter(LightDark == 'L' | LightDark == 'D') %>% 
  dplyr::filter(PhiPS2 <= 0.83 & PhiPS2 > 0) %>% # both light and dark-adapted points
  ungroup() %>%
  ggplot(aes(x=Tdepr, y=PhiPS2)) + 
  geom_point(data = . %>% filter(LightDark == 'L'),  # light-adapted points, colored by Qamb
             aes(color = Qamb, group = Site), size = 1) + 
  geom_point(data = . %>% filter(LightDark == 'D'),   # dark-adapted points, colored in red
             color = "red", size = 1, aes(group = Site)) + 
  # Qamb continuous color scale
  scale_color_viridis(name=expression(Q[amb]), discrete=FALSE)+
  scale_y_continuous(limits = c(0, 0.85), # y axis limits
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8),
                     expand = expansion(mult = 0, add = 0)) + 
  scale_x_continuous(limits = c(-8, 8)) +  # x axis limits
  # axes labels
  xlab(expression(paste(Delta, "T", ~ (T[leaf] - T[air]))~plain("(°C)"))) + 
  ylab(expression(Phi*PSII)) + 
  theme_light(base_family = "Times", base_size = 17) + 
  theme(legend.position="none")+ # remove this line to include a legend
  geom_line(data = fit_lines, aes(x = Tdepr, y = PhiPS2), 
              color = "black",alpha=0.6, size=0.8, inherit.aes = FALSE)+
  facet_wrap(~month, nrow=2, 
             labeller = labeller(`month` = function(x) month.abb[as.numeric(x)]))

fig4




  
