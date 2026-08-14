# Plotting Figure 2: Tair vs Tleaf correlation (Li600)
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

fig2_data <- Li600_all_entire %>%
  filter(LightDark == 'L') %>%
  filter(!is.na(Site)) %>%
  filter(Site == 'Garcia') %>%
  filter(!is.na(Hour)) %>%
  mutate(Hour = as.integer(Hour))

# plot
fig2_data <- Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  filter(!is.na(Hour)) %>%
  mutate(Hour = as.integer(Hour)) %>% # hour to integer
  ggplot(aes(x=Tref, y=Tleaf, group=Site))+
  scale_color_viridis_c(option='magma')+
  stat_ma_line(color = "black", fullrange=T) + # specifying 'fullrange' to expand regression line
  geom_point(aes(color=Hour), alpha=0.85)+
  expand_limits(x = c(-5, 45), y = c(-5, 45))+ # changing plot limits
  scale_x_continuous(limits=c(7,40), expand = c(0, 0))+
  scale_y_continuous(limits=c(7,40), expand = c(0, 0))+
  geom_abline(intercept = 0, slope = 1, linetype='dotted', alpha=0.5) + # y=x line with slope 1
  stat_ma_eq(mapping=use_label(labels=c("eq", "R2")), label.y=.96, family='Times') + # adding R2 labels
  xlab(expression(T[air]~plain("(°C)")))+ #formatting axes titles
  ylab(expression(T[leaf]~plain("(°C)")))+
  theme_bw(base_family = "Times", base_size=14)+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=11))

fig2



## calculating paired difference
fig2_data <- fig2_data %>%
  mutate(diff_leaf_air = Tleaf - Tref)

mean_diff <- mean(fig2_data$diff_leaf_air, na.rm = TRUE)
mean_diff

## performing paired t-test
t_test_result <- t.test(fig2_data$Tleaf, fig2_data$Tref, paired = TRUE)
t_test_result


