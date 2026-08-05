# Plotting Figure 8: temperature response curves from Li6800
# Written by Lily Klinek (lpklinek@ucdavis.edu)
# 

# uses data output from 04_read_wrangle_li6800_data.R


# Prepping df for model fitting ----
plot_data <- combined_data %>% 
  filter(!is.na(ETR),
         Site == 'Garcia') %>%
  ungroup()

## removing error points ----
plot_data <- plot_data[-c(26, 24),]

## binning each step on the temperature response curves ----
## this is to account for slight differences in temperature between curves, since simple
## rounding sometimes grouped data points into the wrong bins

## creating blank matrix for one curve ----
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



# Model fitting and comparison ----

plot_data$curve <- interaction(plot_data$Date, plot_data$TreeID)
plot_data$TreeID <- as.factor(plot_data$TreeID)
plot_data$Date   <- as.factor(plot_data$Date)
plot_data$curve   <- as.factor(plot_data$curve)


## A models ----
m0 <- lmer(A ~ 1 + (1|curve), data =  plot_data)                        # null
m1 <- lmer(A ~ air_temp + Date + (1|curve), data =  plot_data)          # additive linear
m2 <- lmer(A ~ air_temp * Date + (1|curve), data =  plot_data)          # interaction
m3 <- lmer(A ~ air_temp + I(air_temp^2) + Date + (1|curve), data =  plot_data)  # quadratic
m4 <- lmer(A ~ (air_temp + I(air_temp^2)) * Date + (1|curve), data =  plot_data) # full

model_list <-  list(m0=m0, m1 = m1, m2 = m2, m3 = m3, m4 = m4)

compare_models <- function(model_list) {
  AICtab <- AIC(
    model_list$m0,
    model_list$m1,
    model_list$m2,
    model_list$m3,
    model_list$m4
  )
  AICtab$delta <- AICtab$AIC - min(AICtab$AIC)
  AICtab
}

compare_models(model_list)


## PhiPS2 models----
m0 <- lmer(`Fv'/Fm'` ~ 1 + (1|curve), data =  plot_data)                        # null
m1 <- lmer(`Fv'/Fm'` ~ air_temp + Date + (1|curve), data =  plot_data)          # additive linear
m2 <- lmer(`Fv'/Fm'` ~ air_temp * Date + (1|curve), data =  plot_data)          # interaction
m3 <- lmer(`Fv'/Fm'` ~ air_temp + I(air_temp^2) + Date + (1|curve), data =  plot_data)  # quadratic
m4 <- lmer(`Fv'/Fm'` ~ (air_temp + I(air_temp^2)) * Date + (1|curve), data =  plot_data) # full

model_list <-  list(m0=m0, m1 = m1, m2 = m2, m3 = m3, m4 = m4)
compare_models(model_list)

## NPQ models----
m0 <- lmer(NPQ ~ 1 + (1|curve), data =  plot_data)                        # null
m1 <- lmer(NPQ ~ air_temp + Date + (1|curve), data =  plot_data)          # additive linear
m2 <- lmer(NPQ ~ air_temp * Date + (1|curve), data =  plot_data)          # interaction
m3 <- lmer(NPQ ~ air_temp + I(air_temp^2) + Date + (1|curve), data =  plot_data)  # quadratic
m4 <- lmer(NPQ ~ (air_temp + I(air_temp^2)) * Date + (1|curve), data =  plot_data) # full

summary(m3)
model_list <-  list(m0=m0, m1 = m1, m2 = m2, m3 = m3, m4 = m4)
compare_models(model_list)




# Plotting best model for each variable ----

## setting themes ----
theme_top <- theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

theme_bottom <- theme(
  plot.margin = ggplot2::margin(1, 1, 1, 1)
)



## A model + data prep ----
  model_A_quad <- lmer(
    A ~ air_temp + I(air_temp^2) + Date + (1|curve),
    data = plot_data
  )
  
  newdata <- expand.grid(
    air_temp = seq(min(plot_data$air_temp),
                   max(plot_data$air_temp),
                   length.out = 100),
    Date = unique(plot_data$Date)
  )
 
  
  date_levels <- levels(plot_data$Date)
  tree_ref <- levels(plot_data$TreeID)[1]
  curve_ref <- levels(plot_data$curve)[1]
  
  pred_df <- plot_data %>%
    group_by(Date) %>%
    group_modify(~{
      
      x_seq <- seq(min(plot_data$air_temp),
                   max(plot_data$air_temp),
                   length.out = 100)
      
      newdata <- data.frame(
        air_temp = x_seq,
        Date = factor(.y$Date, levels = date_levels),
        curve = factor(curve_ref, levels = levels(plot_data$curve))
      )
      
      preds <- predictInterval(
        model_A_quad,
        newdata = newdata,
        level = 0.95,
        n.sims = 1000,
        which = "fixed",
        include.resid.var = FALSE
      )
      
      tibble(
        air_temp = x_seq,
        #Date = .y$Date,
        fit = preds$fit,
        lower = preds$lwr,
        upper = preds$upr
      )
      
    }) %>%
    ungroup()
  
  x_seq_all <- seq(min(plot_data$air_temp),
                   max(plot_data$air_temp),
                   length.out = 100)
  
  # create predictions for ALL dates
  newdata_all <- expand.grid(
    air_temp = x_seq_all,
    Date = date_levels,
    curve = levels(plot_data$curve)[1]
  )
  
  preds_all <- predictInterval(
    model_A_quad,
    newdata = newdata_all,
    level = 0.95,
    n.sims = 1000,
    which = "fixed",
    include.resid.var = FALSE
  )
  
  preds_all_df <- cbind(newdata_all, preds_all)
  
  # average across dates at each temperature
  overall_df <- preds_all_df %>%
    group_by(air_temp) %>%
    summarize(
      fit = mean(fit),
      lower = mean(lwr),
      upper = mean(upr),
      .groups = "drop"
    )
  
  summary_df <- plot_data %>%
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
  
  
  date_means <- plot_data %>%
    ungroup() %>%
    group_by(Date, air_temp) %>%
    summarize(
      A_mean = mean(A, na.rm = TRUE),
      A_sd   = sd(A, na.rm = TRUE),
      N      = n(),
      se     = A_sd / sqrt(N),
      lower  = A_mean - se,
      upper  = A_mean + se,
      .groups = "drop"
    )
    
  
  
  ## A plot code ----
  a_new <- ggplot() +
    
    # per-Date ribbons 
    geom_ribbon(
      data = pred_df,
      aes(x = air_temp, ymin = lower, ymax = upper, fill = Date),
      alpha = 0.15
    ) +
    
    # per-Date lines 
    geom_line(
      data = pred_df,
      aes(x = air_temp, y = fit, color = Date),
      linewidth = 0.7,
      alpha = 0.5
    ) +
    
    # original points
    geom_point(
      data=date_means,
      aes(x=air_temp, y=A_mean, color=Date, group=Date, fill=Date),
      alpha=0.5,
      size=2, stroke=0
    )+
    
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
    # error barss
    geom_errorbar(
      data = summary_df,
      aes(x = air_temp, ymin = lower, ymax = upper),
      width = 0.2,
      color = "black",
      alpha = 0.8
    ) +
    # palette and theme
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    ylab(expression(A[net]~plain("(µmol")~ CO[2]~m^{-2}~s^{-1}*")")) +
    xlab(expression(T[air]~plain("(°C)"))) +
    theme_bw(base_family = "Times", base_size = 14.5) +
    theme(legend.position = "none") +
    theme_top
  
 
  
  
  ## Phips2 model + data prep ----
 # model_phi <- lmer(`Fv'/Fm'` ~ air_temp + Date + (1|TreeID), data =  plot_data)          # additive linear
  model_phi <- lmer(`Fv'/Fm'` ~ air_temp + I(air_temp^2) + Date + (1|curve), data =  plot_data)  # quadratic
  
  
  # predicting per-date
  
  phi_pred_df <- plot_data %>%
    group_by(Date) %>%
    group_modify(~{
      
      x_seq <- seq(min(plot_data$air_temp),
                   max(plot_data$air_temp),
                   length.out = 100)
      
      newdata <- data.frame(
        air_temp = x_seq,
        Date = factor(.y$Date, levels = date_levels),
        curve = factor(curve_ref, levels = levels(plot_data$curve))
      )
      
      preds <- predictInterval(
        model_phi,
        newdata = newdata,
        level = 0.95,
        n.sims = 1000,
        which = "fixed",
        include.resid.var = FALSE
      )
      
      tibble(
        air_temp = x_seq,
        fit = preds$fit,
        lower = preds$lwr,
        upper = preds$upr
      )
      
    }) %>%
    ungroup()
  
  # for all dates
  phi_preds_all <- predictInterval(
    model_phi,
    newdata = newdata_all,
    level = 0.95,
    n.sims = 1000,
    which = "fixed",
    include.resid.var = FALSE
  )
  
  phi_preds_all_df <- cbind(newdata_all, phi_preds_all)
  
  # average across dates at each temperature
  phi_overall_df <- phi_preds_all_df %>%
    group_by(air_temp) %>%
    summarize(
      fit = mean(fit),
      lower = mean(lwr),
      upper = mean(upr),
      .groups = "drop"
    )
  
  phi_summary_df <- plot_data %>%
    ungroup() %>%
    group_by(air_temp) %>%
    summarize(
      y = mean(`Fv'/Fm'`, na.rm = TRUE),
      sd = sd(`Fv'/Fm'`, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      lower = y - se,
      upper = y + se,
      .groups = "drop"
    )
  
  phi_date_means <- plot_data %>%
    ungroup() %>%
    group_by(Date, air_temp) %>%
    summarize(
      y = mean(`Fv'/Fm'`, na.rm = TRUE),
      sd = sd(`Fv'/Fm'`, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      lower = y - se,
      upper = y + se,
      .groups = "drop"
    )
  
  
  ## Phips2 plot code ----
  b_new <- ggplot() +
    
    geom_ribbon(
      data = phi_pred_df,
      aes(x = air_temp, ymin = lower, ymax = upper, fill = Date),
      alpha = 0.12, color = NA
    ) +
    
    geom_line(
      data = phi_pred_df,
      aes(x = air_temp, y = fit, color = Date),
      alpha = 0.5, linewidth = 0.7
    ) +
    
    # original points
    geom_point(
      data=phi_date_means,
      aes(x=air_temp, y=y, color=Date, group=Date, fill=Date),
      alpha=0.5,
      size=2, stroke=0
    )+
    geom_line(
      data = phi_overall_df,
      aes(x = air_temp, y = fit),
      color = "black", linewidth = 1
    ) +
    
    geom_point(
      data = phi_summary_df,
      aes(x = air_temp, y = y),
      color = "black", alpha = 0.9, size = 1.5
    ) +
    
    geom_errorbar(
      data = phi_summary_df,
      aes(x = air_temp, ymin = lower, ymax = upper),
      color = "black", width = 0.2, alpha = 0.8
    ) +
    
    ylab(expression(phi[PSII])) +
    xlab("")+
    labs(color="Measurement Date")+
    
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2", guide="none") +
    
    theme_bw(base_family = "Times", base_size = 14.5)+
    theme(legend.position = "right",
          axis.title = element_text(size = 16)) +
    theme_top
  
  
  
  
  
## NPQ model + data prep ----
model_NPQ <- lmer(NPQ ~ air_temp + Date + (1|curve), data =  plot_data)

# predicting per-date
npq_pred_df <- plot_data %>%
  group_by(Date) %>%
  group_modify(~{
    
    x_seq <- seq(min(plot_data$air_temp),
                 max(plot_data$air_temp),
                 length.out = 100)
    
    newdata <- data.frame(
      air_temp = x_seq,
      Date = factor(.y$Date, levels = date_levels),
      curve = factor(curve_ref, levels = levels(plot_data$curve))
    )
    
    preds <- predictInterval(
      model_NPQ,
      newdata = newdata,
      level = 0.95,
      n.sims = 1000,
      which = "fixed",
      include.resid.var = FALSE
    )
    
    tibble(
      air_temp = x_seq,
      fit = preds$fit,
      lower = preds$lwr,
      upper = preds$upr
    )
    
  }) %>%
  ungroup()

# for all dates
NPQ_preds_all <- predictInterval(
  model_NPQ,
  newdata = newdata_all,
  level = 0.95,
  n.sims = 1000,
  which = "fixed",
  include.resid.var = FALSE
)

npq_preds_all_df <- cbind(newdata_all, NPQ_preds_all)

# average across dates at each temperature
npq_overall_df <- npq_preds_all_df %>%
  group_by(air_temp) %>%
  summarize(
    fit = mean(fit),
    lower = mean(lwr),
    upper = mean(upr),
    .groups = "drop"
  )

npq_summary_df <- plot_data %>%
  ungroup() %>%
  group_by(air_temp) %>%
  summarize(
    NPQ_mean = mean(NPQ, na.rm = TRUE),
    NPQ_sd   = sd(NPQ, na.rm = TRUE),
    N      = n(),
    se     = NPQ_sd / sqrt(N),
    lower  = NPQ_mean - se,
    upper  = NPQ_mean + se,
    .groups = "drop"
  )

npq_date_means <- plot_data %>%
  ungroup() %>%
  group_by(Date, air_temp) %>%
  summarize(
    NPQ_mean = mean(NPQ, na.rm = TRUE),
    NPQ_sd   = sd(NPQ, na.rm = TRUE),
    N      = n(),
    se     = NPQ_sd / sqrt(N),
    lower  = NPQ_mean - se,
    upper  = NPQ_mean + se,
    .groups = "drop"
  )


## NPQ plot code ----
c_new <- ggplot() +
  
  geom_ribbon(
    data = npq_pred_df,
    aes(x = air_temp, ymin = lower, ymax = upper, fill = Date),
    alpha = 0.12, color = NA
  ) +

  geom_line(
    data = npq_pred_df,
    aes(x = air_temp, y = fit, color = Date),
    alpha = 0.5, linewidth = 0.7
  ) +
  
  # original points
  geom_point(
    data=npq_date_means,
    aes(x=air_temp, y=NPQ_mean, color=Date, group=Date, fill=Date),
    alpha=0.5,
    size=2, stroke=0
  )+
  geom_line(
    data = npq_overall_df,
    aes(x = air_temp, y = fit),
    color = "black", linewidth = 1
  ) +
  
  geom_point(
    data = npq_summary_df,
    aes(x = air_temp, y = NPQ_mean),
    color = "black", alpha = 0.9, size = 1.5
  ) +
  
  geom_errorbar(
    data = npq_summary_df,
    aes(x = air_temp, ymin = lower, ymax = upper),
    color = "black", width = 0.2, alpha = 0.8
  ) +
  
  ylab("NPQ") +
  xlab(expression(T[air]~plain("(°C)"))) +
  
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  theme_bw(base_family = "Times", base_size = 14.5) +
  theme(legend.position = "none") +
  theme_bottom




# Patchwork plot ----

a_new <- a_new+theme(
  # move tag inside panel
  plot.tag.position = c(0, 1), 
  plot.tag = element_text(size = 15),
  # reduce margins
  plot.margin = ggplot2::margin(3, 3, 3, 3) 
)


b_new <- b_new+theme(
  plot.tag.position = c(0, 1), 
  plot.tag = element_text(size = 15),
  plot.margin = ggplot2::margin(3, 3, 3, 3) 
)

c_new<- c_new+theme(
  plot.tag.position = c(0, 1), 
  plot.tag = element_text(size = 15),
  plot.margin = ggplot2::margin(3, 3, 3, 3) 
)

patchworkplot <- 
  a_new /
  b_new /
  c_new +
  plot_layout(
    ncol = 1,
    heights = c(1.2, 1.2, 1.2),
    guides = "collect"
  )+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

patchworkplot
