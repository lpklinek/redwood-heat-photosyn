# Plotting Figure 6: temperature response curves from Li6800
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# uses data output from 04_read_wrangle_li6800_data.R



plot_data <- combined_data %>% 
  filter(!is.na(ETR),
         Site == 'Garcia') %>%
  ungroup()

# removing error points
plot_data <- plot_data[-c(26, 24),]

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

plot_data$curve <- interaction(plot_data$Date, plot_data$TreeID)
plot_data$TreeID <- as.factor(plot_data$TreeID)
plot_data$Date   <- as.factor(plot_data$Date)
plot_data$curve   <- as.factor(plot_data$curve)


plot_data %>%
  filter(Ci>0) %>%
  ggplot(aes(x=Tair, y=Ci, color=as.factor(Date)))+
  geom_point()

# Model fitting and comparison ----
high_Ci <- plot_data %>%
  filter(Ci >= 250)

model_null_highCi <- lmer(A ~ 1 + (1|TreeID:Date), data = high_Ci)
model_linear_highCi <- lmer(A ~ air_temp + (1|TreeID:Date), data = high_Ci)
model_linear_highCi2 <- lmer(A ~ air_temp + Date + (1|TreeID:Date), data = high_Ci)
model_inter_highCi <- lmer(A ~ air_temp * Date + (1|TreeID:Date), data = high_Ci)
model_quadratic_highCi <- lmer(A ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date), data = high_Ci)
model_full_highCi <- lmer(A ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date), data = high_Ci)

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


model_Ci_T <- lmer(Ci ~ air_temp + (1|Date/TreeID), data = plot_data)
summary(model_Ci_T)
anova(model_Ci_T)

model_A_Ci <- lmer(A ~ Ci + (1|Date/TreeID), data = plot_data)
summary(model_A_Ci)

model_full <- lmer(A ~ air_temp + Ci + (1|Date/TreeID), data = plot_data)
summary(model_full)

model_full2 <- lmer(A ~ air_temp * Ci + (1|Date/TreeID), data = plot_data)
summary(model_full2)

m1 <- lmer(A ~ air_temp + (1|Date/TreeID), data = plot_data)
m2 <- lmer(A ~ air_temp + Ci + (1|Date/TreeID), data = plot_data)

anova(m1, m2)



newdata <- expand.grid(
  air_temp = seq(min(high_Ci$air_temp),
                 max(high_Ci$air_temp),
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
  group_by(air_temp) %>%
  summarize(
    fit = mean(fit),
    lower = mean(lwr),
    upper = mean(upr),
    .groups = "drop"
  )

summary_df <- high_Ci %>%
  ungroup() %>%
  group_by(air_temp) %>%
  summarize(
    A_mean = mean(A, na.rm = TRUE),
    A_sd   = sd(A, na.rm = TRUE),
    N      = n(),
    se     = A_sd / sqrt(N),
    lower  = A_mean - se,
    upper  = A_mean + se,
    .groups = "drop"
  )






# PLOTS with stats ----
# 
# note: plot panels were printed and saved separately for formatting reasons

## P1
p1 <-  ggplot() +
  # overall ribbon
  geom_ribbon(
    data = overall_df,
    aes(x = air_temp, ymin = lower, ymax = upper),
    fill = "black",
    alpha = 0.19
  ) +
  # overall line
  geom_line(
    data = overall_df,
    aes(x = air_temp, y = fit),
    color = "black",
    linewidth = 1.2
  ) +
  # empirical points 
  geom_point(
    data = summary_df,
    aes(x = air_temp, y = A_mean),
    color = "black",
    size = 1.5,
    alpha = 0.9
  ) +
  # error bars
  geom_errorbar(
    data = summary_df,
    aes(x = air_temp, ymin = lower, ymax = upper),
    width = 0.2,
    color = "black",
    alpha = 0.8
  ) +
  
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  ylab(expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")")) +
  xlab(expression(T[air]~plain("(°C)"))) +
  
  theme_bw(base_family = "Times", base_size = 16) +
  theme(legend.position = "none") +
  theme(legend.title=element_text(size=13),
    plot.tag.position = c(0, 1), 
    plot.tag = element_text(size = 15),
    plot.margin = ggplot2::margin(3, 3, 3, 3))

## P2 ----
p2 <- plot_data %>% 
  filter(!is.na(ETR), Site == 'Garcia', Ci>0) %>%
  ggplot(aes(x=Ci, y=A, color=Tair)) +
  geom_smooth(method='lm', color='black')+
  geom_point(aes()) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label))), size=5, formula = y ~ x, parse = TRUE, family="Times", label.y=0.98)+
  stat_correlation(size=5, label.y = 0.93, family="Times")+
  theme_bw(base_family='Times', base_size=16.5)+
  theme(legend.title = element_text(size = 15))+
  labs(y=expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")"), x=expression(C[i]~plain("(µmol / mol)")))+
  scale_color_viridis(name=expression(T[air]~plain("(°C)")))+
  theme(
    plot.tag.position = c(0, 1), 
    plot.tag = element_text(size = 15),
    plot.margin = ggplot2::margin(3, 3, 3, 3) )


## P3 ----
p3 <- plot_data %>% 
  filter(!is.na(ETR), Site == 'Garcia', Ci>50) %>%
  ggplot(aes(x=Tair, y=Ci, color=A)) +
  geom_smooth(method='lm', color='black')+
  stat_poly_eq(aes(label = paste(after_stat(eq.label))), size=5, formula = y ~ x, parse = TRUE, family="Times", label.y=0.98)+
  stat_correlation(size=5, label.y = 0.93, family="Times")+
  geom_point(aes()) +
  theme_bw(base_family='Times', base_size=16.5)+
  theme(legend.title = element_text(size = 14))+
  labs(x=expression(T[air]~plain("(°C)")), y=expression(C[i]~plain("(µmol / mol)")))+
  scale_color_viridis(option='inferno', name = expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")"))+
  theme(
    plot.tag.position = c(0, 1), 
    plot.tag = element_text(size = 15),
    plot.margin = ggplot2::margin(4, 4, 4, 4) )

p1
p2
p3

