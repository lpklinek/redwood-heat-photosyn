# Figure S8
# 


ymin <- 0
ymax <- 1

fit_lines <- Li600_all_entire %>%
  filter(Site == "Garcia", LightDark %in% c("L", "D")) %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(Date)) %>%
  group_by(month) %>%
  do({
    model <- lm(PhiPS2 ~ VPDleaf, data = .)
    rng <- c(-0.5, 9)
    new_x <- seq(rng[1], rng[2], length.out = 200)
    new_y <- predict(model, newdata = data.frame(VPDleaf = new_x))

    keep <- which(new_y >= ymin & new_y <= ymax)
    data.frame(month = unique(.$month), VPDleaf = new_x[keep], PhiPS2 = new_y[keep])
  }) %>%
  ungroup()


reg_tablevpd <- Li600_all_entire %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(Date)) %>%
  filter(Site == "Garcia",
         LightDark %in% c("L", "D"),
         PhiPS2 <= 0.83, PhiPS2 > 0) %>%
  ungroup() %>%
  group_by(month) %>%
  do(tidy(lm(PhiPS2 ~ VPDleaf, data = .))) %>%
  ungroup() %>%
  filter(term == "VPDleaf") %>%
  transmute(
    month,
    slope = estimate,
    p_value = p.value,
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

sig_months <- reg_tablevpd %>%
  filter(p_value < 0.05) %>%
  pull(month)

fit_lines_sig <- fit_lines %>%
  filter(month %in% sig_months)

supp_fig8 <- Li600_all_entire %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(Date)) %>%
  group_by(month) %>%
  dplyr::filter(Site == 'Garcia') %>%
  dplyr::filter(LightDark == 'L' | LightDark == 'D') %>%
  
  ggplot(aes(x = VPDleaf, y = PhiPS2)) + 
  
  # light-adapted points
  geom_point(
    data = . %>% filter(LightDark == 'L'),
    aes(color = Qamb, group = Site),
    size = 1
  ) + 
  
  # dark-adapted points
  geom_point(
    data = . %>% filter(LightDark == 'D'),
    color = "red",
    size = 1,
    aes(group = Site)
  ) + 
  
  # regression line ONLY for significant months
  geom_line(data = fit_lines_sig,
            aes(x = VPDleaf, y = PhiPS2),
            color = "black",
            alpha = 0.6,
            linewidth = 0.8,
            inherit.aes = FALSE) +
  
  # slope annotation ONLY for significant months
  stat_poly_eq(
    data = . %>% filter(month %in% sig_months),
    aes(label = after_stat(paste("slope == ", round(b_1, 2)))),
    formula = y ~ x,
    output.type = "numeric",
    parse = TRUE,
    size = 3.5,
    family = "Times",
    label.x = "right", 
    hjust = 1,
    label.y = 0.99
  ) +
  
  # correlation annotation ONLY for significant months
  stat_correlation(
    data = . %>% filter(month %in% sig_months),
    size = 3.5,
    label.x = "right", 
    hjust = 1,
    label.y = 0.94,
    family = "Times"
  ) +
  
  scale_color_viridis(
    discrete = FALSE
  ) +
  
  scale_y_continuous(
    limits = c(0, 0.9),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8),
    expand = expansion(mult = 0, add = 0)
  ) +
  
  scale_x_continuous(
    limits = c(-1, 7),
    breaks = c(0, 2, 4, 6),
    expand = expansion(mult = 0, add = 0)
  ) +
  
  xlab(expression(VPD[leaf]~plain("(kPa)"))) + 
  ylab(expression(phi[PSII])) + 
  theme_light(base_family = "Times", base_size = 17) + 
  theme(axis.title.y = element_text(size = 22))+
  theme(legend.position="none")+
  
  facet_wrap(
    ~month,
    nrow = 2,
    labeller = labeller(
      month = function(x) month.abb[as.numeric(x)]
    )
  )

supp_fig8