# Table S1

models_A <- list(
  Null = mA_null,
  Linear = mA_linear,
  Interaction = mA_inter,
  Quadratic = mA_quad,
  Full = mA_full
)

models_P <- list(
  Null = mP_null,
  Linear = mP_linear,
  Interaction = mP_inter,
  Quadratic = mP_quad,
  Full = mP_full
)

models_N <- list(
  Null = mN_null,
  Linear = mN_linear,
  Interaction = mN_inter,
  Quadratic = mN_quad,
  Full = mN_full
)

make_supp_table <- function(model_list, response_label) {
  
  # AIC
  aic_vals <- sapply(model_list, AIC)
  delta_aic <- aic_vals - min(aic_vals)
  
  # R2
  r2_vals <- map_dbl(model_list, ~performance::r2(.x)$R2_marginal)
  
  # model formulas (cleaned)
  formulas <- map_chr(model_list, ~{
    f <- deparse(formula(.x))
    
    if (response_label == "ΦPSII") {
      f <- gsub("`Fv'/Fm'`", "ΦPSII", f)
    }
    f
  })
  
  tibble(
    Response = response_label,
    Model = names(model_list),
    Formula = formulas,
    AIC = round(aic_vals, 2),
    Delta_AIC = round(delta_aic, 2),
    R2_marginal = round(r2_vals, 3)
  ) %>%
    arrange(AIC)
}

supp_A <- make_supp_table(models_A, "A")
supp_P <- make_supp_table(models_P, "ΦPSII")
supp_N <- make_supp_table(models_N, "NPQ")

supp_table <- bind_rows(supp_A, supp_P, supp_N)

# full version with model terms
make_supp_table_full <- function(model_list, response_label) {
  
  # model metrics
  aic_vals <- sapply(model_list, AIC)
  delta_aic <- aic_vals - min(aic_vals)
  r2_vals <- map_dbl(model_list, ~performance::r2(.x)$R2_marginal)
  
  map_df(names(model_list), function(mname) {
    
    mod <- model_list[[mname]]
    
    # fixed effects only
    coefs <- broom.mixed::tidy(mod, effects = "fixed")
    
    # clean response name
    resp_clean <- ifelse(response_label == "ΦPSII", "ΦPSII", response_label)
    
    # clean term names
    coefs$term <- gsub("I\\(air_temp\\^2\\)", "Tair²", coefs$term)
    coefs$term <- gsub("air_temp", "Tair", coefs$term)
    
    coefs %>%
      mutate(
        Response = resp_clean,
        Model = mname,
        AIC = round(aic_vals[mname], 2),
        Delta_AIC = round(delta_aic[mname], 2),
        R2_marginal = round(r2_vals[mname], 3),
        Signif = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        )
      ) %>%
      dplyr::select(Response, Model, term, estimate, std.error, statistic, p.value,
                    Signif, AIC, Delta_AIC, R2_marginal)
    
  })
}

supp_A_full <- make_supp_table_full(models_A, "A")
supp_P_full <- make_supp_table_full(models_P, "ΦPSII")
supp_N_full <- make_supp_table_full(models_N, "NPQ")

supp_table_full <- bind_rows(supp_A_full, supp_P_full, supp_N_full)

supp_table_full <- supp_table_full %>%
  group_by(Response) %>%
  mutate(Best = ifelse(Delta_AIC == 0, "✓", "")) %>%
  ungroup()

supp_table_full <- supp_table_full %>%
  rename(
    Term = term,
    Estimate = estimate,
    SE = std.error,
    t_value = statistic,
    p_value = p.value
  )



# version with just anova

make_supp_anova_table <- function(model_list, response_label) {
  
  # model-level metrics 
  aic_vals <- sapply(model_list, AIC)
  delta_aic <- aic_vals - min(aic_vals)
  r2_vals <- map_dbl(model_list, ~performance::r2(.x)$R2_marginal)
  
  # loop through models 
  map_df(names(model_list), function(mname) {
    
    mod <- model_list[[mname]]
    
    # ANOVA table
    aov_tbl <- anova(mod) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Term")
    
    # clean term names 
    aov_tbl$Term <- gsub("I\\(air_temp\\^2\\)", "Tair²", aov_tbl$Term)
    aov_tbl$Term <- gsub("air_temp", "Tair", aov_tbl$Term)
    
    aov_tbl %>%
      mutate(
        Response = response_label,
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
}

supp_A <- make_supp_anova_table(models_A, "A")
supp_P <- make_supp_anova_table(models_P, "ΦPSII")
supp_N <- make_supp_anova_table(models_N, "NPQ")

supp_table_anova <- bind_rows(supp_A, supp_P, supp_N)

supp_table_anova <- supp_table_anova %>%
  group_by(Response) %>%
  mutate(Best = ifelse(Delta_AIC == 0, "✓", "")) %>%
  filter(!Term %in% c("(Intercept)")) %>%
  ungroup()

# combining
supp_combined <- supp_table_anova %>%
  dplyr::select(-AIC, -Delta_AIC, -R2_marginal) %>%   # remove duplicates
  full_join(
    supp_table,
    by = c("Response", "Model")
  )

supp_combined <- supp_combined %>%
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

supp_combined <- supp_combined %>%
  arrange(Response, Model)

supp_combined