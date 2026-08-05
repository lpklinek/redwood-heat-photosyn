# Figure S7

Li600_growing_season <- Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  filter(month(Date)>=4 & month(Date)<=9) %>%
  mutate(deltaT = Tleaf-Tref)


sup_plot <- Li600_growing_season %>%
  ggplot(aes(x=deltaT, y=PhiPS2, group=Date))+
  geom_point(aes(color=Tref))+
  stat_correlation(size=3.5, label.y = 0.87, label.x=.93, family="Times")+
  geom_line(stat="smooth", method="lm", 
            color = "gray3", alpha=0.7, size=0.8, se=FALSE) +
  stat_poly_eq(
    aes(label = after_stat(paste("slope == ", round(b_1, 2)))),
    output.type = "numeric",
    parse = TRUE, 
    size=3.5,
    family="Times",
    label.y=0.98,
    label.x=.93
  )+
  facet_wrap(~Date) +
  scale_color_viridis(name = expression(paste(T[air])~plain("(°C)")), discrete = FALSE) +
  geom_vline(aes(xintercept=0), linetype="dotted")+
  labs(y=expression(phi[PSII]),
       x=expression(paste(Delta, "T", ~ (T[leaf] - T[air]))~plain("(°C)")))+
  theme_bw(base_family='Times', base_size=14)+
  theme(legend.title = element_text(size = 13),
        axis.title.y=element_text(size=16))

sup_plot
