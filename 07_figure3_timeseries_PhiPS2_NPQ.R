# Plotting Figure 3: timeseries of DeltaT, PhiPS2, NPQ from Li600 and junior-PAM data
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# uses data output from 01_read_wrangle_li600_data.R and 03_read_wrangle_jpam_data.R


## date axes specs
year_breaks <- seq(ymd("2022-01-01"), ymd("2025-01-01"), by = "1 year")
year_labels <- year(year_breaks)
year_breaks[-length(year_breaks)] + 180


panel1 <- Li600_all_entire %>%
  filter(LightDark == 'L') %>%
  filter(!is.na(Site)) %>%
  filter(Site == 'Garcia') %>%
  ggplot(aes(x = Date, y = Tdepr, group = Site)) +
  geom_point(aes(color = PhiPS2), size = 1.3) +
  ylab(expression(Delta*T~(T[leaf] - T[air])~plain("(°C)"))) +
  xlab("") +
  labs(color = expression(Phi*PSII)) +
  theme(axis.title.y = element_text(size = 17)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b", 
               limits=c(as.Date('2022-06-10'), as.Date('2024-10-20')),
               expand=c(0,0))+
  theme_light(base_family = "Times", base_size = 13) +
  # dotted y=0 line
  geom_line(aes(y = 0), linetype = 'dotted', alpha = 0.5) +
  scale_color_viridis_c(option = 'turbo', direction = -1) +
  # vertical lines at each January
  geom_vline(xintercept = as.numeric(year_breaks), linetype = "dashed", color = "gray40") +
  # year labels 
  annotate("text",
           x = as.Date(c("2022-07-25", "2023-06-30", "2024-06-29")), 
           y = max(Li600_all_entire$Tdepr, na.rm = TRUE) + 0.5,  
           label = year_labels[-length(year_labels)],
           size = 5,
           family = "Times")


# NPQ plot (junior PAM data)
panel2 <- final_clean_jpam %>%
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
  xlab("") +
  geom_point(aes(), color='tomato2')+
  geom_smooth(aes(), color='tomato2', method='loess', alpha=0.6, se=FALSE, size=1, span=0.5)+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), color='tomato2', width=5, alpha=0.2)+
  theme_light(base_family = "Times", base_size=13)+
  theme(axis.title.y = element_text(size = 17))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b", 
               limits=c(as.Date('2022-06-10'), as.Date('2024-10-20')),
               expand=c(0,0))+
  geom_vline(xintercept = as.numeric(year_breaks), linetype = "dashed", color = "gray40") +
  # year labels
  annotate("text",
           x = as.Date(c("2022-07-25", "2023-06-30", "2024-06-29")),
           y = c(3.05, 3.05, 3.05),
           label = year_labels[-length(year_labels)],
           size = 5,
           family = "Times")

fig3 <- panel1 /
  panel2 +
  plot_layout(
    ncol = 1
  )+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

fig3

  