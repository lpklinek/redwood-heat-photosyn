# Plotting Figure 8: temperature response curves from Li6800
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 



# uses data output from 04_read_wrangle_li6800_data.R



plot_data <- combined_data %>% 
  filter(!is.na(ETR),
         Site == 'Garcia') %>%
  ungroup()

# removing error points
plot_data <- plot_data[-c(35, 24),]


# binning each step on the temperature response curves 
## this is to account for slight differences in temperature between curves, since simple
## rounding sometimes grouped data points into the wrong bins

# creating blank matrix for one curve
mat <- matrix(1:8, nrow=8, ncol=1)
# 12 curves total
mat3 <- rbind(mat, mat, mat, mat, mat, mat, mat, mat, mat, mat, mat, mat)
mat3 <- as.data.frame(mat3)
# adding as column to plot data
plot_data['step'] <- (mat3)

# creating 'air temp' manually rounded column
plot_data['air_temp'] <- NA

plot_data['air_temp'][plot_data['step'] == 1] = 22
plot_data['air_temp'][plot_data['step'] == 2] = 25
plot_data['air_temp'][plot_data['step'] == 3] = 28
plot_data['air_temp'][plot_data['step'] == 4] = 31
plot_data['air_temp'][plot_data['step'] == 5] = 34
plot_data['air_temp'][plot_data['step'] == 6] = 37
plot_data['air_temp'][plot_data['step'] == 7] = 40
plot_data['air_temp'][plot_data['step'] == 8] = 43


## PhiPS2 (Li6800's 'Fv/Fm' variable for light-adapted points)
plot_data %>%
  group_by(Date, air_temp) %>%
  # calculating summary stats
  mutate(FvFm_mean = mean(`Fv'/Fm'`, na.rm=T),
         FvFm_sd = sd(`Fv'/Fm'`, na.rm=T),
         N_FvFm = n(), 
         se = FvFm_sd/sqrt(N_FvFm),
         upper_limit=FvFm_mean+se,
         lower_limit=FvFm_mean-se) %>%
  ggplot(aes(x=air_temp, y=FvFm_mean, group=Date)) + # coloring by measurement date
  geom_point(aes(color=Date), alpha=0.7) +
  geom_smooth(aes(color=Date, group=Date), alpha=0.6, se=FALSE, size=1)+
  # axes labels
  ylab(expression(phi[PSII]))+
  xlab(expression(T[air]~plain("(°C)")))+
  # error bars
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, color=Date), width=.2, alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  theme_bw(base_family = "Times", base_size=13)+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10.5))


# A (gas exchange rate)
plot_data %>%
  group_by(Date, air_temp) %>%
  # summary stats
  mutate(A_mean = mean(A, na.rm=T),
         A_sd = sd(A, na.rm=T),
         N_A = n(), 
         se = A_sd/sqrt(N_A),
         upper_limit=A_mean+se,
         lower_limit=A_mean-se) %>%
  ggplot(aes(x=air_temp, y=A_mean, group=Date)) + # coloring by measurement date
  geom_point(aes(color=Date), alpha=0.7) +
  geom_smooth(aes(color=Date, group=Date), alpha=0.6, se=FALSE, size=1)+
  # axes labels
  ylab("A (µmol m⁻² s⁻¹)")+
  xlab(expression(T[air]~plain("(°C)")))+
  # error bars
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, color=Date), width=.2, alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  theme_bw(base_family = "Times", base_size=13)+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10.5))

# stomatal conductance
plot_data %>%
  group_by(Date, air_temp) %>%
  mutate(gsw_mean = mean(gsw, na.rm=T),
         gsw_sd = sd(gsw, na.rm=T),
         N_g = n(), 
         se = gsw_sd/sqrt(N_g),
         upper_limit=gsw_mean+se,
         lower_limit=gsw_mean-se) %>%
  ggplot(aes(x=air_temp, y=gsw_mean, group=Date)) + # coloring by measurement date
  geom_point(aes(color=Date), alpha=0.7) +
  geom_smooth(aes(color=Date, group=Date), alpha=0.6, se=FALSE, size=1)+
  # axes labels
  ylab(expression(g[sw]))+
  xlab(expression(T[air]~plain("(°C)")))+
  # error bars
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, color=Date), width=.2, alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  theme_bw(base_family = "Times", base_size=13)+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10.5))

# NPQ
plot_data %>%
  group_by(Date, air_temp) %>%
  # summary stats
  mutate(NPQ_mean = mean(NPQ, na.rm=T),
         NPQ_sd = sd(NPQ, na.rm=T),
         N_g = n(), 
         se = NPQ_sd/sqrt(N_g),
         upper_limit=NPQ_mean+se,
         lower_limit=NPQ_mean-se) %>%
  ggplot(aes(x=air_temp, y=NPQ_mean, group=Date)) + # coloring by measurement date
  geom_point(aes(color=Date), alpha=0.7) +
  geom_smooth(aes(color=Date, group=Date), alpha=0.6, se=FALSE, size=1)+
  # axes labels 
  ylab('NPQ')+
  xlab(expression(T[air]~plain("(°C)")))+
  # error bars
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit, color=Date), width=.2, alpha=0.5)+
  scale_color_brewer(palette='Dark2')+
  theme_bw(base_family = "Times", base_size=13)+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10.5))
