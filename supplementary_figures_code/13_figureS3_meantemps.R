# Figure S3

# plotting mean temps before each of the temperature response curves

tempresp_dates <- as.Date(unique(combined_data$Date)[2:5])
tempresp_date_starts <- tempresp_dates - 7

patched_data %>%
  mutate(Date = as.Date(date)) %>%
  dplyr::filter(Date >= tempresp_date_starts[4] & Date <= tempresp_dates[4]) %>%
  summarize(avg_temp = mean(Tavg, na.rm=TRUE))


time_frame_patched <- patched_data %>%
  dplyr::filter(date>='2024-05-21' & date <= '2024-10-05')

global_Tmean_min <- min((time_frame_patched$Tmin + time_frame_patched$Tmax)/2, na.rm = TRUE)
global_Tmean_max <- max((time_frame_patched$Tmin + time_frame_patched$Tmax)/2, na.rm = TRUE)


fig_s3 <- patched_data %>%
  filter(date>='2024-05-21' & date <= '2024-10-05') %>%
  ggplot(aes(x=date, y=Tavg))+
  geom_line()+
  geom_line(aes(y=Tmax))+
  geom_line(aes(y=Tmin))+
  geom_ribbon(aes(ymin = Tmin, ymax = Tmax, fill = Tavg), alpha = 1) +
  scale_fill_gradientn(
    colours = hcl.colors(20, "Spectral", rev = TRUE),
    limits = c(global_Tmean_min, global_Tmean_max),
    na.value = "transparent"
  ) +
  # sample dates
  geom_vline(xintercept=as.Date('2024-06-08'))+
  geom_vline(xintercept=as.Date('2024-06-28'))+
  geom_vline(xintercept=as.Date('2024-08-01'))+
  geom_vline(xintercept=as.Date('2024-09-28'))+
  # axes labels
  ylab(expression(T[air]~plain('(°C)'))) +
  xlab('')+
  labs(fill=expression(plain("Mean")~T[air]~plain('(°C)')))+
  # theme specs
  theme_light(base_family = "Times", base_size = 13) 

fig_s3
