# Plotting Figure 2: site meteorological data and sampling dates
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# Data prep ----

# data: patched_data (created in 02_read_wrangle_meteorology_data.R)
# includes site meteorological data (Tmax, Tmin, Tavg, VPDmax, VPDmin, VPDavg) with temperature data 
# gap-filled using Boonville weather station data


# marking data collection dates
dates_G <- ymd(unique(Li600_all_entire$Date[Li600_all_entire$Site == 'Garcia']))
dates_G <- c(dates_G, '2024-09-27')
dates_df_G <- data.frame(date = dates_G) %>%
  mutate(date = ymd(date))

## prepping plot data ----
plot_data <- patched_data %>%
  filter(site == 'Garcia') %>%
  pivot_longer(cols = c(Tmax, Tmin, VPDavg, VPDmax, VPDmin), names_to = "variable", values_to = "values") %>%
  mutate(date = ymd(date)) %>%
  filter(date < '2024-10-10')


# year boundaries (Jan 1 of each year)
year_lines <- plot_data %>%
  mutate(date = ymd(date)) %>%
  distinct(year = year(date)) %>%
  filter(year!=2022) %>%
  mutate(date = ymd(paste0(year, "-01-01")))

# year label positions (midpoint of each year)
year_labels <- plot_data %>%
  mutate(date = ymd(date),
         year = year(date)) %>%
  group_by(year) %>%
  summarize(
    mid_date = as.Date(mean(as.numeric(date), na.rm = TRUE), origin = "1970-01-01"),
    .groups = "drop"
  )



panel1 <- plot_data %>%
  mutate(date = ymd(date)) %>%
  ggplot(aes(x = date, y = values, group = site)) +
  # dashed vertical lines for year boundaries
  geom_vline(
    data = year_lines,
    aes(xintercept = as.numeric(date)),
    linetype = "dashed",
    color = "black",
    alpha = 0.8,
    linewidth = 0.6
  ) +
  geom_vline(data = dates_df_G, 
             aes(xintercept = as.numeric(date)), 
             color="gray60",
             #linetype="dotted",
             lwd = 0.5, 
             alpha = 0.4, 
             show.legend=FALSE) +
  # --- temperature lines ---
  geom_line(
    data = ~filter(.x, variable %in% c("Tmax", "Tmin")),
    aes(color = variable)
  ) +
  # --- year labels near top of plot ---
  geom_text(
    data = year_labels,
    aes(x = mid_date, y = 44, label = year),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  scale_x_date(
    date_breaks = "1 month",  # ticks every month
    labels = function(x) {
      ifelse(lubridate::month(x) %% 2 == 1,  # label every other month
             format(x, "%b"),
             "")
    },
    expand = c(0.01, 0.01)
  )+
  scale_color_manual(
    values = c(
      "Tmax" = "#BB5566",
      "Tmin" = "#004488",
      "VPDmax" = "#DDAA33"
    ),
    breaks = c("Tmax", "Tmin", "VPDmax"),
    labels = c(expression(T[max]),
               expression(T[min]),
               expression(VPD[max]))
  ) +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    limits = c(-2, 45),
    breaks = seq(0, 45, by = 5)
  )+
  
  labs(x = "", y = "Air Temperature (°C)") +
  theme_light(base_family = "Times", base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(3, 3, 3, 3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.length.y = unit(0.2, "cm")
  )


panel2 <- plot_data %>%
  mutate(date = ymd(date)) %>%
  ggplot(aes(x = date, y = values, group = site)) +
  # dashed vertical lines for year boundaries 
  geom_vline(
    data = year_lines,
    aes(xintercept = as.numeric(date)),
    linetype = "dashed",
    color = "black",
    alpha = 0.8,
    linewidth = 0.6
  ) +
  # gray lines for sampling days
  geom_vline(data = dates_df_G, 
             aes(xintercept = as.numeric(date)), 
             color="gray60",
             lwd = 0.5, 
             alpha = 0.4, 
             show.legend=FALSE) +
  # VPD
  geom_line(
    data = ~filter(.x, variable == "VPDmax"),
    aes(y = values, color = variable),
    lwd=0.8
  ) +
  # year labels
  geom_text(
    data = year_labels,
    aes(x = mid_date, y = 7.9, label = year),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  scale_x_date(
    date_breaks = "1 month",  # ticks every month
    labels = function(x) {
      ifelse(lubridate::month(x) %% 2 == 1,  # label every other month
             format(x, "%b"),
             "")
    },
    expand = c(0.01, 0.01)
  )+
  scale_color_manual(
    values = c(
      "Tmax" = "#BB5566",
      "Tmin" = "#004488",
      "VPDmax" = "#DDAA33"
    ),
    breaks = c("Tmax", "Tmin", "VPDmax"),
    labels = c(expression(T[max]),
               expression(T[min]),
               expression(VPD[max]))
  ) +
  scale_y_continuous(
    limits = c(0, 8),
    breaks = seq(0, 8, by = 1)
  )+
  labs(x = "", y = "VPD (kPa)") +
  theme_light(base_family = "Times", base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = ggplot2::margin(2, 2, 2, 2),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.length.y = unit(0.2, "cm"),
    panel.grid.minor = element_blank()
  )



twopanel <- panel1 /
  panel2 +
  plot_layout(
    ncol = 1
  )+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

twopanel
ggsave(twopanel, filename = "/Users/lklinek/Desktop/Dissertation/Chapter_One/REVISIONS/fig2_met_alt.png",
       width=8, height=8, dpi=1200)

