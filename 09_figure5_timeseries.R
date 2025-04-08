# Plotting Figure 5: timeseries of Tdepr, gsw, and NPQ from Li600 and junior-PAM data
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# uses data output from 01_read_wrangle_li600_data.R and 03_read_wrangle_jpam_data.R


# Temperature depression
Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  ggplot(aes(x=Date, y=Tdepr, group=Site))+
  geom_point(aes(color=PhiPS2), size=1.3)+
  ylab(expression(T[depr]~(T[leaf]-T[air])))+  #axes labels
  xlab("")+
  labs(color = expression(phi[PSII]))+  #legend title
  theme(axis.title.y = element_text(size = 17))+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b%Y")+ # date labels on x axis
  theme_light(base_family = "Times", base_size=13)+
  geom_line(aes(y=0), linetype='dotted', alpha=0.5)+ # y=0 horizontal dotted line
  scale_color_viridis_c(option='turbo', direction=-1)



# Stomatal conductance
Li600_all_entire %>%
  filter(LightDark=='L') %>%
  filter(!is.na(Site)) %>%
  filter(Site=='Garcia') %>%
  group_by(Date) %>%
  # calculating daily summary statistics for gsw
  mutate(gsw_mean = mean(gsw, na.rm=T),
         gsw_sd = sd(gsw, na.rm=T),
         N_g = n(), 
         se = gsw_sd/sqrt(N_g),
         upper_limit=gsw_mean+se,
         lower_limit=gsw_mean-se) %>%
  ggplot(aes(x=Date, y=gsw_mean, group=Site))+
  geom_point(aes(), color='#001889')+
  geom_smooth(aes(), color='#001889', method='loess', alpha=0.6, se=FALSE, size=1, span=0.4)+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), color='#001889', width=5, alpha=0.2)+
  ylab(expression(g[sw]))+
  theme_light(base_family = "Times", base_size=13)+
  theme(axis.title.y = element_text(size = 17))+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b%Y")


# NPQ plot (junior PAM data)
final_clean_jpam %>%
  filter(Site=='Garcia') %>%
  rowwise() %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  filter(PAR==820) %>% # filtering for only points at the end of each light-response curve (at max light)
  group_by(Site, Date) %>%
  mutate(NPQ_mean = mean(NPQ, na.rm=T),  # summary stats for NPQ for each measurement date
         NPQ_sd = sd(NPQ, na.rm=T),
         N_g = n(), 
         se = NPQ_sd/sqrt(N_g),
         upper_limit=NPQ_mean+se,
         lower_limit=NPQ_mean-se) %>%
  ungroup() %>%
  ggplot(aes(x=Date, y=NPQ_mean))+
  ylab("Max. NPQ")+
  geom_point(aes(), color='tomato2')+
  geom_smooth(aes(), color='tomato2', method='loess', alpha=0.6, se=FALSE, size=1, span=0.5)+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), color='tomato2', width=5, alpha=0.2)+
  theme_light(base_family = "Times", base_size=13)+
  theme(axis.title.y = element_text(size = 17))+
  scale_x_date(date_breaks = "2 months" , date_labels = "%b
%Y")