# Leaf temperature experiment -- comparing Li600 and Li6800
# written by Lily Klinek (lpklinek@ucdavis.edu)



# read li600
leaftemp600 <- read.csv('./data/raw/05_leaf_temp_raw/Manual gsw+F/2026-03-03/Manual gsw+F_LI-COR Default_2026_03_04_11_21_26_1.csv', skip=1)
leaftemp600 <- leaftemp600[-1,]

leaftemp600 <- leaftemp600 %>%
  select(leaf_lk, lightdark, Obs., Time, Date, gsw, VPcham, VPleaf, VPDleaf, Fs, Fm., PhiPS2, ETR,
         rh_s, rh_r, Tref, Tmeas, Tleaf, P_atm, Qamb)

# read li6800
leaftemp6800 <- read.csv('./data/raw/05_leaf_temp_raw/2026-03-03-0654_logdata.csv', skip=13)
leaftemp6800 <- leaftemp6800[-1,]

leaftemp6800 <- leaftemp6800 %>% 
  select(leaf_lk, lightdark, obs, date, hhmmss, E, A, Ca, Ci, gsw, SVPleaf, RHcham, VPcham, VPDleaf,
         Qin, Tair, Tleaf)



leaftemp600_join <- leaftemp600 %>%
  select(leaf_lk, lightdark, Tref, Tleaf, Qamb) %>%
  rename(Leaf = leaf_lk, 
         Tair_600 = Tref,
         Tleaf_600 = Tleaf)

leaftemp6800_join <- leaftemp6800 %>%
  select(leaf_lk, lightdark, hhmmss, A, gsw, Qin, Tair, Tleaf) %>%
  rename(Leaf = leaf_lk,
         Tair_6800 = Tair,
         Tleaf_6800 = Tleaf)

leaftemp_join <- left_join(leaftemp600_join, leaftemp6800_join, by=join_by(Leaf, lightdark)) %>%
  mutate(across(Tair_600:Tleaf_6800, as.numeric)) %>%
  mutate(deltaT_600 = Tleaf_600-Tair_600,
         deltaT_6800 = Tleaf_6800-Tair_6800)


fig_S2 <- leaftemp_join %>%
  filter(lightdark=='L') %>%
  ggplot(aes(x=Tleaf_6800, y=Tleaf_600, color=Qamb))+
  ylim(c(10,35))+
  xlim(c(10,35))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed")+
  scale_color_viridis_c()+
  labs(x=expression(T[leaf]~plain("(LI-6800)")~plain("(°C)")),
       y=expression(T[leaf]~plain("(LI-600)")~plain("(°C)")),
       color=expression(Q[amb]~plain("(µmol")~m^-2~s^-1~plain(")")))+
  theme_bw(base_family = "Times", base_size=14)+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10))+
  stat_cor(method = "pearson", label.x = 10, label.y = 33)

fig_S2



