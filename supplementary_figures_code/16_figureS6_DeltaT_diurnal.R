# Figure S6

supp_fig_s6 <- Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  filter(!is.na(Hour)) %>%
  filter(Date!='2023-10-25') %>%
  mutate(Hour = as.integer(Hour),
         deltaT = Tleaf-Tref,
         month = month(Date)) %>%
  group_by(Hour) %>%
  mutate(mean_deltaT = mean(deltaT, na.rm=TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x=Hour, y=deltaT, group=Hour))+
  geom_jitter(aes(y=deltaT, color=Tleaf), alpha=0.6)+
  geom_boxplot(aes(), alpha=0.3, outlier.shape = NA, varwidth=TRUE, fill=NA)+
  scale_color_viridis_c(option='magma')+
  scale_x_continuous(breaks=c(9, 10, 11, 12, 13, 14, 15, 16, 17))+
  scale_y_continuous(breaks=c(-5, -2.5, 0, 2.5, 5, 7.5, 10))+
  geom_abline(intercept = 0, slope = 0, linetype='dotted', alpha=0.5) + 
  ylab(expression(Delta~"T"~plain("(°C)")))+
  labs(color=expression(T[leaf]~plain("(°C)")))+
  theme_bw(base_family = "Times", base_size=14)+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=11))

supp_fig_s6