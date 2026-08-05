# Table 2


sig_label <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE      ~ ""
  )
}


fit_and_extract <- function(response, data) {
  
  # model formulas
  f0 <- as.formula(paste(response, "~ 1 + (1|curve)"))
  f1 <- as.formula(paste(response, "~ air_temp + Date + (1|curve)"))
  f2 <- as.formula(paste(response, "~ air_temp * Date + (1|curve)"))
  f3 <- as.formula(paste(response, "~ air_temp + I(air_temp^2) + Date + (1|curve)"))
  f4 <- as.formula(paste(response, "~ (air_temp + I(air_temp^2)) * Date + (1|curve)"))
  
  # fit models
  m0 <- lmer(f0, data = data)
  m1 <- lmer(f1, data = data)
  m2 <- lmer(f2, data = data)
  m3 <- lmer(f3, data = data)
  m4 <- lmer(f4, data = data)
  
  model_list <- list(m0=m0, m1=m1, m2=m2, m3=m3, m4=m4)
  
  # AIC comparison
  aic_tab <- AIC(m0, m1, m2, m3, m4)
  aic_tab$delta <- aic_tab$AIC - min(aic_tab$AIC)
  
  best_name <- rownames(aic_tab)[which.min(aic_tab$AIC)]
  best_model <- model_list[[gsub(".*\\$", "", best_name)]]
  
  # model label (clean)
  model_label <- case_when(
    best_name == "m0" ~ "Null",
    best_name == "m1" ~ "Linear (additive)",
    best_name == "m2" ~ "Linear (interaction)",
    best_name == "m3" ~ "Quadratic (additive)",
    best_name == "m4" ~ "Quadratic (interaction)"
  )
  
  model_eq <- case_when(
    best_name == "m0" ~ "Null",
    best_name == "m1" ~ "Linear (additive)",
    best_name == "m2" ~ "Linear (interaction)",
    best_name == "m3" ~ "Quadratic (additive)",
    best_name == "m4" ~ "Quadratic (interaction)"
  )
  
  # compute marginal r2
  r2_val <- performance::r2(best_model)$R2_marginal
  
  # ANOVA
  an <- anova(best_model)
  
  out <- as.data.frame(an) %>%
    mutate(
      term = rownames(an),
      Response = response,
      Model = model_label,
      AIC = min(aic_tab$AIC),
      sig = sig_label(`Pr(>F)`),
      R2_marginal = round(r2_val, 3)
    ) %>%
    dplyr::select(Response, Model, AIC, R2_marginal, term, `F value`, `Pr(>F)`, sig)
  
  return(out)
}

model_labels <- function(resp_var) {
  list(
    m0 = paste0(resp_var, " ~ 1 + (1|TreeID)"),
    m1 = paste0(resp_var, " ~ air_temp + Date + (1|TreeID)"),
    m2 = paste0(resp_var, " ~ air_temp * Date + (1|TreeID)"),
    m3 = paste0(resp_var, " ~ air_temp + I(air_temp^2) + Date + (1|TreeID)"),
    m4 = paste0(resp_var, " ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID)")
  )
}

results_table <- bind_rows(
  fit_and_extract("A", plot_data),
  fit_and_extract("`Fv'/Fm'`", plot_data),
  fit_and_extract("NPQ", plot_data)
)

results_table <- results_table %>%
  mutate(
    term = recode(term,
                  "air_temp" = "Temperature",
                  "I(air_temp^2)" = "Temperature²",
                  "Date" = "Date",
                  "air_temp:Date" = "Temperature × Date",
                  "I(air_temp^2):Date" = "Temperature² × Date"
    )
  )

results_table <- results_table %>%
  mutate(
    AIC = round(AIC, 2),
    F.value = round(`F value`, 2),
    `Pr(>F)` = signif(`Pr(>F)`, 3)
  ) %>%
  dplyr::select(-c(`F value`)) %>%
  mutate(Response = recode(Response,
                           "`Fv'/Fm'`" = "ΦPSII"))





# candidate models with TreeID:Date random effect 
candidate_models <- function(response) {
  list(
    Null        = as.formula(paste0(response, " ~ 1 + (1|TreeID:Date)")),
    Linear      = as.formula(paste0(response, " ~ air_temp + Date + (1|TreeID:Date)")),
    Interaction = as.formula(paste0(response, " ~ air_temp * Date + (1|TreeID:Date)")),
    Quadratic   = as.formula(paste0(response, " ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date)")),
    Full        = as.formula(paste0(response, " ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date)"))
  )
}

# map model names to formulas
model_labels <- function(resp_var) {
  lab_resp <- ifelse(resp_var == "`Fv'/Fm'`", "ΦPSII", resp_var)
  list(
    Null        = paste0(lab_resp, " ~ 1 + (1|TreeID:Date)"),
    Linear      = paste0(lab_resp, " ~ air_temp + Date + (1|TreeID:Date)"),
    Interaction = paste0(lab_resp, " ~ air_temp * Date + (1|TreeID:Date)"),
    Quadratic   = paste0(lab_resp, " ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date)"),
    Full        = paste0(lab_resp, " ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date)")
  )
}

# fxn to fit models, select best, extract stats 
make_model_table <- function(resp_var, data) {
  
  safe_var <- make.names(resp_var)
  
  # fit all candidate models
  model_list <- candidate_models(resp_var) %>%
    map(~lmer(.x, data = data, REML = FALSE))
  
  # compute AIC for each model
  aic_vals <- sapply(model_list, AIC)
  best_model_name <- names(aic_vals)[which.min(aic_vals)]
  best_model <- model_list[[best_model_name]]
  best_aic <- aic_vals[best_model_name]
  
  # compute marginal r2
  r2_val <- performance::r2(best_model)$R2_marginal
  
  # extract per-term F and p-values
  anova_tbl <- anova(best_model) %>%
    as.data.frame() %>%
    rownames_to_column("Term") %>%
    mutate(Signif = case_when(
      `Pr(>F)` < 0.001 ~ "***",
      `Pr(>F)` < 0.01  ~ "**",
      `Pr(>F)` < 0.05  ~ "*",
      TRUE ~ ""
    ))
  
  # assemble final table
  final_tbl <- anova_tbl %>%
    dplyr::select(Term, `F value`, `Pr(>F)`, Signif) %>%
    mutate(
      Response       = ifelse(resp_var == "`Fv'/Fm'`", "ΦPSII", resp_var),
      Model_Name     = best_model_name,
      Selected_Model = model_labels(resp_var)[[best_model_name]],
      AIC            = round(best_aic, 2),
      R2_marginal    = round(r2_val, 3)
    ) %>%
    dplyr::select(Response, Model_Name, Selected_Model, AIC, R2_marginal, Term, `F value`, `Pr(>F)`, Signif)
  
  return(final_tbl)
}

# apply to all response variables 
responses <- c("A", "`Fv'/Fm'`", "NPQ") 
all_tables <- map_df(responses, ~make_model_table(.x, plot_data))







# table v2 ----

# A
mA_null      <- lmer(A ~ 1 + (1|TreeID:Date), data = plot_data)
mA_linear    <- lmer(A ~ air_temp + Date + (1|TreeID:Date), data = plot_data)
mA_inter     <- lmer(A ~ air_temp * Date + (1|TreeID:Date), data = plot_data)
mA_quad      <- lmer(A ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date), data = plot_data)
mA_full      <- lmer(A ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date), data = plot_data)

summary(mA_inter)
anova(mA_inter)
# select best A model by AIC
aics_A <- c(Null = AIC(mA_null), Linear = AIC(mA_linear), Interaction = AIC(mA_inter), Quadratic = AIC(mA_quad), Full = AIC(mA_full))
best_A_name <- names(aics_A)[which.min(aics_A)]
best_A <- switch(best_A_name, Null = mA_null, Linear = mA_linear, Quadratic = mA_quad)
r2_A <- performance::r2(best_A)$R2_marginal
anova_A <- anova(best_A) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(Signif = case_when(
    `Pr(>F)` < 0.001 ~ "***",
    `Pr(>F)` < 0.01  ~ "**",
    `Pr(>F)` < 0.05  ~ "*",
    TRUE ~ ""
  ))

tbl_A <- anova_A %>%
  dplyr::select(Term, `F value`, `Pr(>F)`, Signif) %>%
  mutate(Response = "A",
         Model_Name = best_A_name,
         Selected_Model = paste0("A ~ ", paste(all.vars(formula(best_A))[2:length(all.vars(formula(best_A)))], collapse = " + ")),
         AIC = round(aics_A[best_A_name],2),
         R2_marginal = round(r2_A,3))

# --- ΦPSII ---
mP_null      <- lmer(`Fv'/Fm'` ~ 1 + (1|TreeID:Date), data = plot_data)
mP_linear    <- lmer(`Fv'/Fm'` ~ air_temp + Date + (1|TreeID:Date), data = plot_data)
mP_inter     <- lmer(`Fv'/Fm'` ~ air_temp * Date + (1|TreeID:Date), data = plot_data)
mP_quad      <- lmer(`Fv'/Fm'` ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date), data = plot_data)
mP_full      <- lmer(`Fv'/Fm'` ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date), data = plot_data)

summary(mP_inter)
anova(mP_inter)
anova(mP_full)

aics_P <- c(Null = AIC(mP_null), Linear = AIC(mP_linear), Interaction = AIC(mP_inter),
            Quadratic = AIC(mP_quad), Full = AIC(mP_full))
best_P_name <- names(aics_P)[which.min(aics_P)]
best_P <- switch(best_P_name, Null = mP_null, Linear = mP_linear, Interaction = mP_inter,
                 Quadratic = mP_quad, Full = mP_full)
r2_P <- performance::r2(best_P)$R2_marginal
anova_P <- anova(best_P) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(Signif = case_when(
    `Pr(>F)` < 0.001 ~ "***",
    `Pr(>F)` < 0.01  ~ "**",
    `Pr(>F)` < 0.05  ~ "*",
    TRUE ~ ""
  ))
tbl_P <- anova_P %>%
  dplyr::select(Term, `F value`, `Pr(>F)`, Signif) %>%
  mutate(Response = "ΦPSII",
         Model_Name = best_P_name,
         Selected_Model = paste0("ΦPSII ~ ", paste(all.vars(formula(best_P))[2:length(all.vars(formula(best_P)))], collapse = " + ")),
         AIC = round(aics_P[best_P_name],2),
         R2_marginal = round(r2_P,3))

# --- NPQ ---
mN_null      <- lmer(NPQ ~ 1 + (1|TreeID:Date), data = plot_data)
mN_linear    <- lmer(NPQ ~ air_temp + Date + (1|TreeID:Date), data = plot_data)
mN_inter     <- lmer(NPQ ~ air_temp * Date + (1|TreeID:Date), data = plot_data)
mN_quad      <- lmer(NPQ ~ air_temp + I(air_temp^2) + Date + (1|TreeID:Date), data = plot_data)
mN_full      <- lmer(NPQ ~ (air_temp + I(air_temp^2)) * Date + (1|TreeID:Date), data = plot_data)

summary(mN_inter)
anova(mN_inter)

aics_N <- c(Null = AIC(mN_null), Linear = AIC(mN_linear), Interaction = AIC(mN_inter),
            Quadratic = AIC(mN_quad), Full = AIC(mN_full))
best_N_name <- names(aics_N)[which.min(aics_N)]
best_N <- switch(best_N_name, Null = mN_null, Linear = mN_linear, Interaction = mN_inter,
                 Quadratic = mN_quad, Full = mN_full)
r2_N <- performance::r2(best_N)$R2_marginal
anova_N <- anova(best_N) %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(Signif = case_when(
    `Pr(>F)` < 0.001 ~ "***",
    `Pr(>F)` < 0.01  ~ "**",
    `Pr(>F)` < 0.05  ~ "*",
    TRUE ~ ""
  ))
tbl_N <- anova_N %>%
  dplyr::select(Term, `F value`, `Pr(>F)`, Signif) %>%
  mutate(Response = "NPQ",
         Model_Name = best_N_name,
         Selected_Model = paste0("NPQ ~ ", paste(all.vars(formula(best_N))[2:length(all.vars(formula(best_N)))], collapse = " + ")),
         AIC = round(aics_N[best_N_name],2),
         R2_marginal = round(r2_N,3))

# combine tables
final_tbl <- bind_rows(tbl_A, tbl_P, tbl_N) %>%
  dplyr::select(Response, Model_Name, Selected_Model, AIC, R2_marginal, Term, `F value`, `Pr(>F)`, Signif)

final_tbl

write_csv(final_tbl, '/Users/lklinek/Desktop/final_table.csv')

