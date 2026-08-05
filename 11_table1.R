# Table 1



reg_table <- Li600_all_entire %>%
  mutate(Tdepr = Tleaf - Tref,
         month = month(date)) %>%
  filter(Site == "Garcia",
         LightDark %in% c("L", "D"),
         PhiPS2 <= 0.83, PhiPS2 > 0) %>%
  group_by(month) %>%
  do({
    mod <- lm(PhiPS2 ~ Tdepr, data = .)
    
    tidy(mod) %>%
      filter(term == "Tdepr") %>%
      mutate(r.squared = summary(mod)$r.squared)
  }) %>%
  ungroup() %>%
  transmute(
    month,
    slope = estimate,
    p_value = p.value,
    r       = sign(estimate) * sqrt(r.squared),
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )



reg_table <- reg_table %>%
  left_join(n_table, by = "month")

reg_table_pretty <- reg_table %>%
  arrange(month) %>%  
  mutate(
    month = month.abb[month], 
    p_value = signif(p_value, 3),
    slope = round(slope, 3),
    r = round(r, 2)
  )

reg_table_pretty