# Plotting Figure 3: Tair vs VPD curve from Li600 leaf-level measurements
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

month_labels <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", 
                  "Jul.", "Aug.", "Sep.", "Oct.", "Nov.")


Li600_all_entire %>%
  filter(Site == 'Garcia') %>%
  dplyr::filter(LightDark=='L') %>% # only selecting light-adapted measurements
  ggplot(aes(x=Tref, y=VPDleaf, 
             color=factor(month(Date), levels=1:11, labels=month_labels), # color by month, label with month names
             group=Site)) +
  geom_point(size=1)+
  stat_poly_line(formula = y~poly(x, 3, raw=TRUE), se=F, lwd=0.6, color='black')+ # 3rd degree polynomial
  theme_light(base_family = "Times", base_size=13) +
  labs(color = expression(underline("  "~Month~"   ")), # formatting legend title
       x = "Air Temperature (°C)", # axes titles
       y = "Leaf VPD (kPa)")+
  theme(legend.position = "inside", # legend position and formatting
        legend.position.inside = c(.15, .65), 
        legend.background = element_rect(fill="white", color="black", linewidth=0.2), # box outline
        legend.key = element_blank(), 
        legend.margin = margin(c(4, 15, 4, 15)), # margin around legend text
        legend.text=element_text(hjust=.5)) +
  guides(color = guide_legend(override.aes = list(size = 2), # increasing size of points inside legend
                              theme=theme(legend.title = element_text(hjust=0.1))))
