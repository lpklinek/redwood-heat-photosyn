# Plotting Figure S9: leaf temperature response curves from Li6800
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# uses data output from 04_read_wrangle_li6800_data.R



plot_data <- combined_data %>% 
  filter(!is.na(ETR),
         Site == 'Garcia') %>%
  ungroup()

# removing error points
plot_data <- plot_data[-c(26, 24),]


plot_data$curve <- interaction(plot_data$Date, plot_data$TreeID)
plot_data$TreeID <- as.factor(plot_data$TreeID)
plot_data$Date   <- as.factor(plot_data$Date)
plot_data$curve   <- as.factor(plot_data$curve)



# Model fitting and comparison ----
high_Ci <- plot_data %>%
  filter(Ci >= 250)

model_null_highCi <- lmer(A ~ 1 + (1|TreeID:Date), data = high_Ci)
model_linear_highCi <- lmer(A ~ Tleaf + (1|TreeID:Date), data = high_Ci)
model_linear_highCi2 <- lmer(A ~ Tleaf + Date + (1|TreeID:Date), data = high_Ci)
model_inter_highCi <- lmer(A ~ Tleaf * Date + (1|TreeID:Date), data = high_Ci)
model_quadratic_highCi <- lmer(A ~ Tleaf + I(Tleaf^2) + Date + (1|TreeID:Date), data = high_Ci)
model_full_highCi <- lmer(A ~ (Tleaf + I(Tleaf^2)) * Date + (1|TreeID:Date), data = high_Ci)

summary(model_linear_highCi)
summary(model_inter_highCi)
summary(model_linear_highCi2)
anova(model_linear_highCi)
anova(model_inter_highCi)
anova(model_full_highCi)

high_Ci_models <- list(
  Null = model_null_highCi,
  Linear = model_linear_highCi2,
  Interaction = model_inter_highCi,
  Quadratic = model_quadratic_highCi,
  Full = model_full_highCi
)

aic_vals <- sapply(high_Ci_models, AIC)
delta_aic <- aic_vals - min(aic_vals)

# R2
r2_vals <- map_dbl(high_Ci_models, ~performance::r2(.x)$R2_marginal)

# Model formulas (cleaned)
formulas <- map_chr(high_Ci_models, ~{
  f <- deparse(formula(.x))
  
  f
})


model_Ci_T <- lmer(Ci ~ Tleaf + (1|Date/TreeID), data = plot_data)
summary(model_Ci_T)
anova(model_Ci_T)

model_A_Ci <- lmer(A ~ Ci + (1|Date/TreeID), data = plot_data)
summary(model_A_Ci)

model_full <- lmer(A ~ Tleaf + Ci + (1|Date/TreeID), data = plot_data)
summary(model_full)

model_full2 <- lmer(A ~ Tleaf * Ci + (1|Date/TreeID), data = plot_data)
summary(model_full2)

m1 <- lmer(A ~ Tleaf + (1|Date/TreeID), data = plot_data)
m2 <- lmer(A ~ Tleaf + Ci + (1|Date/TreeID), data = plot_data)

anova(m1, m2)
date_levels <- levels(plot_data$Date)
tree_ref <- levels(plot_data$TreeID)[1]
curve_ref <- levels(plot_data$curve)[1]

newdata <- expand.grid(
  Tleaf = seq(min(high_Ci$Tleaf),
                 max(high_Ci$Tleaf),
                 length.out = 100),
  Date = factor(high_Ci$Date, levels = date_levels),
  TreeID = factor(tree_ref, levels = levels(high_Ci$TreeID))
)


preds <- predictInterval(
  model_quadratic_highCi,
  #model_linear_highCi,
  newdata = newdata,
  level = 0.95,
  n.sims = 1000,
  which = "fixed",
  include.resid.var = FALSE
)



preds_df <- cbind(newdata, preds)

# average across dates at each temperature
overall_df <- preds_df %>%
  group_by(Tleaf) %>%
  summarize(
    fit = mean(fit),
    lower = mean(lwr),
    upper = mean(upr),
    .groups = "drop"
  )

summary_df <- high_Ci %>%
  ungroup() %>%
  group_by(Tleaf) %>%
  summarize(
    A_mean = mean(A, na.rm = TRUE),
    A_sd   = sd(A, na.rm = TRUE),
    N      = n(),
    se     = A_sd / sqrt(N),
    lower  = A_mean - se,
    upper  = A_mean + se,
    .groups = "drop"
  )

binned_df <- high_Ci %>%
  ungroup() %>%
  mutate(bin = cut(Tleaf, breaks=8)) %>%
  group_by(bin) %>%
  summarise(
    Tleaf_mid = mean(Tleaf),
    mean_Anet = mean(A, na.rm=TRUE),
    sd_Anet = sd(A, na.rm=TRUE),
    n = n(),
    se_Anet = sd_Anet / sqrt(n)
  )



# PLOTS with stats ----
# 
# note: plot panels were printed and saved separately for formatting reasons



# P1
p1 <- ggplot() +
  # overall ribbon
  geom_ribbon(
    data = overall_df,
    aes(x = Tleaf, ymin = lower, ymax = upper),
    fill = "black",
    alpha = 0.19
  ) +
  # overall line
  geom_line(
    data = overall_df,
    aes(x = Tleaf, y = fit),
    color = "black",
    linewidth = 1.2
  ) +
  # empirical points 
  geom_point(
    data = binned_df,
    aes(x = Tleaf_mid, y = mean_Anet),
    color = "black",
    size = 1.5,
    alpha = 0.9
  ) +
  # error bars
  geom_errorbar(
    data = binned_df,
    aes(x = Tleaf_mid, ymin = mean_Anet - se_Anet, ymax = mean_Anet + se_Anet),
    width = 0.2,
    color = "black",
    alpha = 0.8
  ) +
  
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  ylab(expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")")) +
  xlab(expression(T[leaf]~plain("(°C)"))) +
  
  theme_bw(base_family = "Times", base_size = 16) +
  theme(legend.position = "none") +
  theme(legend.title=element_text(size=13),
        plot.tag.position = c(0, 1), 
        plot.tag = element_text(size = 15),
        plot.margin = ggplot2::margin(3, 3, 3, 3))



## P2 ----
p2 <- plot_data %>% 
  filter(!is.na(ETR), Site == 'Garcia', Ci>0) %>%
  ggplot(aes(x=Ci, y=A, color=Tleaf)) +
  geom_smooth(method='lm', color='black')+
  geom_point(aes()) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label))), size=5, formula = y ~ x, parse = TRUE, family="Times", label.y=0.98)+
  stat_correlation(size=5, label.y = 0.93, family="Times")+
  theme_bw(base_family='Times', base_size=16.5)+
  theme(legend.title = element_text(size = 15))+
  labs(y=expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")"), x=expression(C[i]~plain("(µmol / mol)")))+
  scale_color_viridis(name=expression(T[leaf]~plain("(°C)")))+
  theme(
    plot.tag.position = c(0, 1), 
    plot.tag = element_text(size = 15),
    plot.margin = ggplot2::margin(3, 3, 3, 3) )


## P3 ----
p3 <- plot_data %>% 
  filter(!is.na(ETR), Site == 'Garcia', Ci>50) %>%
  ggplot(aes(x=Tleaf, y=Ci, color=A)) +
  geom_smooth(method='lm', color='black')+
  stat_poly_eq(aes(label = paste(after_stat(eq.label))), size=5, formula = y ~ x, parse = TRUE, family="Times", label.y=0.98)+
  stat_correlation(size=5, label.y = 0.93, family="Times")+
  geom_point(aes()) +
  theme_bw(base_family='Times', base_size=16.5)+
  theme(legend.title = element_text(size = 14))+
  labs(x=expression(T[leaf]~plain("(°C)")), y=expression(C[i]~plain("(µmol / mol)")))+
  scale_color_viridis(option='inferno', name = expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")"))+
  theme(
    plot.tag.position = c(0, 1), 
    plot.tag = element_text(size = 15),
    plot.margin = ggplot2::margin(4, 4, 4, 4) )

p1
p2
p3
