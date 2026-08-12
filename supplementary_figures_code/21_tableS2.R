# Table S2
# 

highCi_table <- tibble(
  Response = "A",
  Model = names(high_Ci_models),
  Formula = formulas,
  AIC = round(aic_vals, 2),
  Delta_AIC = round(delta_aic, 2),
  R2_marginal = round(r2_vals, 3)
) 


# anova ----
highCi_anova <- map_df(names(high_Ci_models), function(mname) {
  
  mod <- high_Ci_models[[mname]]
  
  # ANOVA table
  aov_tbl <- anova(mod) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Term")
  
  # clean term names (optional but nicer)
  aov_tbl$Term <- gsub("I\\(air_temp\\^2\\)", "Tair²", aov_tbl$Term)
  aov_tbl$Term <- gsub("air_temp", "Tair", aov_tbl$Term)
  
  aov_tbl %>%
    mutate(
      Response = "A",
      Model = mname,
      AIC = round(aic_vals[mname], 2),
      Delta_AIC = round(delta_aic[mname], 2),
      R2_marginal = round(r2_vals[mname], 3),
      Signif = case_when(
        `Pr(>F)` < 0.001 ~ "***",
        `Pr(>F)` < 0.01  ~ "**",
        `Pr(>F)` < 0.05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(Response, Model, Term, `F value`, `Pr(>F)`, Signif,
                  AIC, Delta_AIC, R2_marginal)
  
})

highCi_combined <- highCi_anova %>%
  dplyr::select(-AIC, -Delta_AIC, -R2_marginal) %>%   # remove duplicates
  full_join(
    highCi_table,
    by = c("Response", "Model")
  )

highCi_combined <- highCi_combined %>%
  dplyr::select(
    Response,
    Model,
    Formula,
    AIC,
    Delta_AIC,
    R2_marginal,
    Term,
    `F value`,
    `Pr(>F)`,
    Signif
  )

highCi_combined <- supp_combined %>%
  arrange(Response, Model)

write_csv(highCi_combined, '/Users/lklinek/Desktop/highCi_table.csv')


AIC(model_linear_highCi)


AIC(model_inter_highCi)
AIC(model_linear_highCi2)
anova(model_linear_highCi)
r2(model_linear_highCi)

AIC(model_null_highCi)
AIC(model_quadratic_highCi)

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

high_temp <- plot_data %>%
  filter(Ci >= 250, Ci <= 300, air_temp >= 34)

high_temp_model <- lm(A ~ air_temp, data = high_temp)
summary(high_temp_model)

# stuck here
newdata <- expand.grid(
  air_temp = seq(min(high_Ci$air_temp),
                 max(high_Ci$air_temp),
                 length.out = 100),
  Date = factor(high_Ci$Date, levels = date_levels),
  TreeID = factor(tree_ref, levels = levels(high_Ci$TreeID))
)
# store original factor levels


library(merTools)

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


# linear regression tables ----

linear_table_aci <- plot_data %>%
  filter(!is.na(ETR), Site == 'Garcia', Ci>0) %>%
  do({
    mod <- lm(A ~ Ci, data = .)
    
    tidy(mod) %>%
      filter(term == "Ci") %>%
      mutate(r.squared = summary(mod)$r.squared)
  }) %>%
  ungroup() %>%
  transmute(
    formula = "A ~ Ci",
    slope = estimate,
    p_value = p.value,
    r       = sign(estimate) * sqrt(r.squared),
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    n=92
  )

linear_table_ci_tair <- plot_data %>%
  filter(!is.na(ETR), Site == 'Garcia', Ci>50) %>%
  do({
    mod <- lm(Ci ~ air_temp, data = .)
    
    tidy(mod) %>%
      filter(term == "air_temp") %>%
      mutate(r.squared = summary(mod)$r.squared)
  }) %>%
  ungroup() %>%
  transmute(
    formula = "Ci ~ Tair",
    slope = estimate,
    p_value = p.value,
    r       = sign(estimate) * sqrt(r.squared),
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    n=92
  )

ci_linear_table <- bind_rows(linear_table_aci, linear_table_ci_tair)


ci_linear_table  <- ci_linear_table  %>%
  mutate(
    p_value = signif(p_value, 3),
    slope = round(slope, 3),
    r = round(r, 2)
  )

ci_linear_table

