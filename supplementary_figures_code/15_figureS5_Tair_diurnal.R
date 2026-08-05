# Figure S5

supp_fig_5 <- all_meteo %>%
  mutate(datetime = as_datetime(Timestamp)) %>%
  mutate(date = date(datetime),
         hour = hour(datetime),
         month = month(date)) %>%
  filter(month>5 & month <11) %>%
  group_by(month, hour) %>%
  summarize(mean_Tair = mean(Tair, na.rm = TRUE),
            sd_Tair = sd(Tair, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x=hour, y=mean_Tair, group=month))+
  geom_point(aes())+
  geom_ribbon(aes(ymin = mean_Tair - sd_Tair,
                  ymax = mean_Tair + sd_Tair),
              alpha=0.2)+
  geom_line(aes())+
  facet_wrap(~month, 
             labeller = labeller(`month` = function(x) month.abb[as.numeric(x)]))+
  labs(y=expression(plain("Mean")~T[air]~plain("(°C)")),
       x="Hour")+
  scale_x_continuous(breaks=c(seq(0, 24, 2)))+
  theme_bw(base_family = "Times", base_size=17)

supp_fig_5