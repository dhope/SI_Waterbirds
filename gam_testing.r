require(tidyverse)
require(mgcv)
source("./R/RunClimateModels.r")
dat_preped <- climateModelRun(non_id_limit = 0.8, return_type = 'models',prepdata = T)

gam_W_J <- gam(formula = WESA.juv.est ~ s(Year, bs='tp', k = 24) + 
                 DOY_rescaled + I(DOY_rescaled^2),# + 
                 # s(DOY_rescaled, bs='re') + 
                 # s(d2, bs='re'),# +
                 # # s(Year.factor, bs='re'),
               family = nb, select=T,
               data = dat_preped$dat_j %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                  Year = arm::rescale(Year),
                                                  d2 = DOY_rescaled^2))
gam_W_J2 <- gam(formula = WESA.juv.est ~ s(Year, bs='tp', k = 24) + 
                 s(DOY_rescaled, bs='cr', k=24),# + 
               # s(DOY_rescaled, bs='re') + 
               # s(d2, bs='re'),# +
               # # s(Year.factor, bs='re'),
               family = nb, select=T,
               data = dat_preped$dat_j %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                  Year = arm::rescale(Year),
                                                  d2 = DOY_rescaled^2))
bbmle::AICctab(gam_W_J, gam_W_J2)
gam.check(gam_W_J)
rsd <- residuals(gam_W_J)
gam(rsd~s(Year, bs='tp', k=20), gamma = 1.4, data =dat_preped$dat_j %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                                               Year = arm::rescale(Year),
                                                                               d2 = DOY_rescaled^2)) 

da <- dat_preped$dat_a %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                  Year = arm::rescale(Year),
                                  d2 = DOY_rescaled^2)

gam_W_A <- gam(formula = WESA.ad.est ~ s(Year, bs='tp', k = 10) + 
                 DOY_rescaled + d2,# +
               family = nb,
               data = da)

bbmle::AICtab(gam_W_A, gam_W_A2,gam_W_A3)
gam.check(gam_W_A)
rsd <- residuals(gam_W_A)
gam(rsd~s(Year, bs='cs', k=19), gamma = 1.4, data = da)

mods_all <- read_rds( "output/model_dist_comparisons.rds")$NB2_ZI_wesa_adults
mods_all$WESA.ad.est$gamA <- gam_W_A
mods_all$WESA.ad.est %>% bbmle::AICtab(., mnames = names(.))

mods_all$WESA.juv.est$gamJ <- gam_W_J
mods_all$WESA.juv.est %>% bbmle::AICtab(., mnames = names(.))


aplot <- sjPlot::plot_model(gam_W_A, type = 'pred', terms = c( "Year") ) +
    coord_trans(y = scales::log1p_trans()) +
  stat_summary(data = da, fun.data = 'mean_cl_boot',
               aes(x=arm::rescale(Year), y=WESA.juv.est))
sjPlot::plot_model(mods_[[24]], type = 'pred', terms = 'Year') +

  
  coord_trans(y = scales::log1p_trans()) +
  stat_summary(data = dat_preped$dat_j, fun.data = 'mean_cl_boot',
               aes(x=arm::rescale(Year), y=WESA.juv.est))

aplot + coord_trans(y = scales::log1p_trans()) +
  stat_summary(data = dat_preped$dat_a, fun.data = 'mean_cl_boot',
               aes(x=arm::rescale(Year), y=WESA.ad.est))


AIC(gam_W_J, gam_W_J1, gam_W_J3, gam_W_J4)

gam_L_J <- gam(formula = LESA.juv.est ~ s(Year, bs='cs', k = 6) + 
                 DOY_rescaled + d2,
               family = nb,
               data = dat_preped$dat_j_lesa %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                  Year = arm::rescale(Year),
                                                  d2 = DOY_rescaled^2))

gam_L_A <- gam(formula = LESA.ad.est ~  s(Year, bs='cs', k = 6) + 
                 DOY_rescaled + d2,
               family = nb,#method = "GCV.Cp" ,
               data = dat_preped$dat_a_lesa %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                  Year = arm::rescale(Year),
                                                  d2 = DOY_rescaled^2))

mods_all$LESA.ad.est$gamA <- gam_L_A
mods_all$LESA.ad.est %>% bbmle::AICctab(., mnames = names(.))

mods_all$LESA.juv.est$gamJ <- gam_L_J
mods_all$LESA.juv.est %>% bbmle::AICctab(., mnames = names(.))

sjPlot::plot_model(gam_L_A, type = 'pred', terms = 'Year') 
sjPlot::plot_model(gam_L_J, type = 'pred', terms = 'Year') 


mods_ <- vector('list', length = 24)

for (k in 1:24){
  mods_[[k]] <- gam(formula = WESA.juv.est ~ s(Year, bs='tp', k = k) + 
                   # DOY_rescaled + d2,# + 
                     s(DOY_rescaled, bs='cr', k=k),# + 
                 family = nb, select=F,
                 data = dat_preped$dat_j %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                    Year = arm::rescale(Year),
                                                    d2 = DOY_rescaled^2))
  
}
bbmle::AICtab(mods_,weights =TRUE, base=T, logLik=T)

mods_ <- vector('list', length = 24)

for (k in 1:24){
  mods_[[k]] <- gam(formula = WESA.juv.est ~ s(Year, bs='tp', k = k) + 
                      s(DOY_rescaled, bs='cr', k=k),# + 
                      # DOY_rescaled + d2,# + 
                    family = nb, select=T,
                    data = dat_preped$dat_j %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                       Year = arm::rescale(Year),
                                                       d2 = DOY_rescaled^2))
  names(mods_)[[k]] <- paste0("k=",k)
  
}
bbmle::AICtab(mods_)
calcmodAIC(mods_, name_ = "WESA.juv.est")


mods_ab <- vector('list', length = 19**2)

for (k in 1:19){
  for(k2 in 1:19){
  mods_ab[[k]] <- gam(formula = WESA.ad.est ~ s(Year, bs='tp', k = k) + 
                      # DOY_rescaled + d2,# +
                     s(DOY_rescaled, bs='cr', k=k2),# +
                    family = nb, select=F,
                    data = dat_preped$dat_a %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                       Year = arm::rescale(Year),
                                                       d2 = DOY_rescaled^2))
  names(mods_ab)[[k+(k-1)*(k2-1)]] <- paste0("k=",k,";dk=",k2)
  
}}
# bbmle::AICtab(mods_a)
calcmodAIC(c(mods_ab,mods_a), name_ = "WESA.ad.est")

mods_jl <- vector('list', length = 24)

for (k in 1:24){
  mods_jl[[k]] <- gam(formula = LESA.juv.est ~ s(Year, bs='tp', k = k) + 
                      # DOY_rescaled + d2,# +
                      s(DOY_rescaled, bs='cr', k=k),# + 
                    family = nb, select=F,
                    data = dat_preped$dat_j_lesa %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                       Year = arm::rescale(Year),
                                                       d2 = DOY_rescaled^2))
  names(mods_jl)[[k]] <- paste0("k=",k)
}
# bbmle::AICtab(mods_jl)
calcmodAIC(mods_jl, name_ = "LESA.juv.est")

mods_al <- vector('list', length = 19)

for (k in 1:19){
  mods_al[[k]] <- gam(formula = LESA.ad.est ~ s(Year, bs='tp', k = k) + 
                       # DOY_rescaled + d2,# + 
                        s(DOY_rescaled, bs='cr', k=k),# + 
                     family = nb, select=T,
                     data = dat_preped$dat_a_lesa %>% mutate(DOY_rescaled = arm::rescale(DOY),
                                                        Year = arm::rescale(Year),
                                                        d2 = DOY_rescaled^2))
  names(mods_al)[[k]] <- paste0("k=",k)
}
bbmle::AICtab(mods_al)
calcmodAIC(mods_al, name_ = "LESA.ad.est")