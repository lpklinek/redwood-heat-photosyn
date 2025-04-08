# Plotting Figure 2: site meteorological data and sampling dates
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# Data prep ----

# data: patched_data (created in 02_read_wrangle_meteorology_data.R)
# includes site meteorological data (Tmax, Tmin, Tavg, VPDmax, VPDmin, VPDavg) with temperature data 
# gap-filled using Boonville weather station data

## heatwave window classification ----

# identify heatwave days (days where Tmax > 30)
heatwave_days <- patched_data %>%
  mutate(Site == 'Garcia') %>%
  filter(Tmax >= 30) %>%
  select(date)

# expanding heatwave window to plus/minus 3 days
heatwave_window <- heatwave_days %>%
  mutate(expanded_window = map(date, ~seq(.x - days(3), .x + days(3), by = "days"))) %>%
  unnest(expanded_window) %>%
  distinct(expanded_window) %>%
  rename(date = expanded_window)

# marking my data collection dates
dates <- ymd(unique(Li600_all_entire$Date[Li600_all_entire$Site == 'Garcia']))

# creating binary 'heatwave' column to mark days within heatwave windows
dates_df <- data.frame(date = dates) %>%
  mutate(heatwave = ifelse(date %in% heatwave_window$date, "Y", "N"))

## prepping plot data ----
plot_data <- patched_data %>%
  filter(Site == 'Garcia') %>%
  pivot_longer(cols = c(Tmax, Tmin, VPDavg, VPDmax, VPDmin), names_to = "variable", values_to = "values") %>%
  mutate(heatwave = ifelse(date %in% dates_df$date[dates_df$heatwave == "Y"], "Y", "N"))


# Figure 2 -----

## setting necessary plot parameters ----
vpd_scaling_factor <- 4.5
lineT <- c("solid", "dashed")
names(lineT) <- c("N","Y")


## plot code----
plot_data %>%
  mutate(date = ymd(date)) %>%
  mutate(Site == 'Garcia') %>%
  ggplot(aes(x = date, y = values, group=Site)) +
  # plotting specifically Tmax and Tmin on one axis
  geom_line(data = plot_data %>% 
              filter(variable== 'Tmax' | variable == 'Tmin'),
            aes(color = variable)) +
  # plotting VPDmax on second axis with scaling factor (specified above plot)
  geom_line(data = plot_data %>% 
              filter(variable == "VPDmax"),
            aes(x = date, y = values * vpd_scaling_factor, color = variable)) +
  # vertical lines to mark sample dates
  geom_vline(data = dates_df, 
             aes(xintercept = as.numeric(date), 
                 color = heatwave, # coloring lines by heatwave variable
                 linetype=factor(heatwave)), # setting linetype to heatwave variable
             lwd = 1, 
             alpha = 0.8, 
             show.legend=FALSE) +
  scale_linetype_manual(values = lineT)+ # linetype: either solid or dashed
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.01, 0.01)) +
  # setting colors, legend labels
  scale_color_manual(
    values = c("Tmax" = "#F8766D", "Tmin" = "#00BFC4", 'VPDmax' = 'purple', "Y" = "gold", "N" = "gray65"),
    breaks = c("Tmax", "Tmin", "VPDmax"),
    labels = c(expression(T[max]), expression(T[min]), expression(VPD[max]))) +
 # y axes
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(-3, 45),
    sec.axis = sec_axis(~./vpd_scaling_factor, name = "VPD (kPa)")) +
  # axes labels
  ylab('Temperature (°C)') +
  xlab('')+
  # theme specs
  theme_light(base_family = "Times", base_size = 13) +
  # legend specs
  theme(legend.position = "top",
        legend.direction = "horizontal", 
        legend.justification = "left",
        legend.title=element_blank())
