### Generate predictions for plots ----
# David Hope
# 2018
require(tidyverse)
require(glmmTMB)
source("R/RunClimateModels.r")
source("R/miscFunctions.r")
source("R/CV.r")
models_for_ms <- read_rds("output/models_for_ms.rds")

spID <- tibble(var =unlist(y_vars), Species = c(rep("Western Sandpipers", 2), rep("Least Sandpipers", 2)), 
               Age = rep(c("Adults", "Juveniles"),2))

require(lubridate)
## bring in the bird count
non.id_limit <- 0.8
sidney <- read_rds("output/Sidney_1990_2018.rds") %>%
  mutate(
    Yr = arm::rescale(Year),
    Year.factor = as.factor(Year),

    non.id = replace_na(non.id, 0)
  ) %>%
  filter(non.id <= non.id_limit)
quantlims <- c(0.005, 0.995)
# quantlims <- c(0.05, 0.95)
quant_a <- Hmisc::wtd.quantile(sidney$DOY, sidney$WESA.ad.est, probs = quantlims)
quant_j <- Hmisc::wtd.quantile(sidney$DOY, sidney$WESA.juv.est, probs = quantlims)

quant_a_lesa <- Hmisc::wtd.quantile(sidney$DOY, sidney$LESA.ad.est, probs = quantlims)
quant_j_lesa <- Hmisc::wtd.quantile(sidney$DOY, sidney$LESA.juv.est, probs = quantlims)
write_rds(list(quant_a, quant_j, quant_a_lesa, quant_j_lesa), "output/clipping_dates.rds")

yrsinAnalysis <- read_rds("output/yearsInAnalysis.rds")

sidney_adults <- sidney %>%
  filter(DOY <= quant_a[[2]] & Year %in% yrsinAnalysis$adults) %>% # <231
  mutate(DOY_rescaled = arm::rescale(DOY))
sidney_juveniles <- sidney %>%
  filter(DOY > quant_j[[1]] & Year %in% yrsinAnalysis$juv) %>%
  mutate(DOY_rescaled = arm::rescale(DOY))

sidney_adults_lesa <- sidney %>%
  filter(DOY <= quant_a_lesa[[2]] & Year %in% yrsinAnalysis$adults) %>% # <231
  mutate(DOY_rescaled = arm::rescale(DOY))
sidney_juveniles_lesa <- sidney %>%
  filter(DOY > quant_j_lesa[[1]]& Year %in% yrsinAnalysis$juv ) %>%
  mutate(DOY_rescaled = arm::rescale(DOY))

# Exclude years with insufficient survey effort.
# effortby_age <-
#   bind_rows(list(
#     sidney_adults %>%
#       group_by(Year) %>%
#       summarize(n = n()) %>%
#       mutate(Age = "Adults"),

#     sidney_juveniles %>%
#       group_by(Year) %>%
#       summarize(n = n()) %>%
#       mutate(Age = "Juveniles")
#   ))

# adult_yrs <- effortby_age$Year[effortby_age$Age=="Adults" & effortby_age$n>2]
# juv_yrs <- effortby_age$Year[effortby_age$Age=="Juveniles" & effortby_age$n>2]
# write_rds(list(adults = adult_yrs, juv = juv_yrs), "output/yearsInAnalysis.rds")




sd_doy_a <- sd(sidney_adults$DOY)
mean_doy_a <- mean(sidney_adults$DOY)
sd_doy_j <- sd(sidney_juveniles$DOY)
mean_doy_j <- mean(sidney_juveniles$DOY)

sd_doy_a_lesa <- sd(sidney_adults_lesa$DOY)
mean_doy_a_lesa <- mean(sidney_adults_lesa$DOY)
sd_doy_j_lesa <- sd(sidney_juveniles_lesa$DOY)
mean_doy_j_lesa <- mean(sidney_juveniles_lesa$DOY)




mnyr_A <- mean(sidney_adults$Year)
mnyr_J <- mean(sidney_juveniles$Year)
sd_yr_A <- sd(sidney_adults$Year)
sd_yr_J <- sd(sidney_juveniles$Year)

## Import models
# if(!exists("fullresults_all_dist")) fullresults_all_dist <- read_rds("output/model_dist_comparisons.rds")
random_slope_models <- read_rds("output/random_slope_models.rds")

## CALCULATE PREDICTED VALUES FOR EACH YEAR DATE COMBINATION
# construct data frame over which to predict median values and trends

yrs.doys <- expand.grid(
  Year.factor = as.factor(unique(sidney$Year)),
  DOY = seq(range(sidney$DOY)[1], range(sidney$DOY)[2])
) %>%
  mutate(
    Year = as.numeric(as.character(Year.factor)),
    DOY_rescaled_a = (DOY - mean_doy_a) / (2 * sd_doy_a),
    DOY_rescaled_j = (DOY - mean_doy_j) / (2 * sd_doy_j),
    DOY_rescaled_a_lesa = (DOY - mean_doy_a_lesa) / (2 * sd_doy_a_lesa),
    DOY_rescaled_j_lesa = (DOY - mean_doy_j_lesa) / (2 * sd_doy_j_lesa),
    Yr_a = (Year - mnyr_A) / (2 * sd_yr_A),
    Yr_j = (Year - mnyr_J) / (2 * sd_yr_J)
  ) %>%
  mutate(
    pred.wesa.ad = predict(models_for_ms$WESA.ad.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a)
    ),
    pred.wesa.ad_rs = predict(random_slope_models$WESA.ad.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a)
    ),
    pred.wesa.juv = predict(models_for_ms$WESA.juv.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_j, Yr = Yr_j)
    ),
    pred.wesa.juv_rs = predict(random_slope_models$WESA.juv.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_j, Yr = Yr_j)
    ),
    pred.lesa.ad = predict(models_for_ms$LESA.ad.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_a_lesa, Yr = Yr_a)
    ),
    pred.lesa.ad_rs = predict(random_slope_models$LESA.ad.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_a_lesa, Yr = Yr_a)
    ),
    pred.lesa.juv = predict(models_for_ms$LESA.juv.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_j_lesa, Yr = Yr_j)
    ),
    pred.lesa.juv_rs = predict(random_slope_models$LESA.juv.est$null,
      type = "response",allow.new.levels=T, newdata =
        dplyr::rename(., DOY_rescaled = DOY_rescaled_j_lesa, Yr = Yr_j)
    ),
    se.a.w  = predict(models_for_ms$WESA.ad.est$null,
            type = "response",allow.new.levels=T, newdata =
              dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a), se.fit = T
    )$se.fit
  )
# rm(fullresults_all_dist)

ranefbyYr <- tibble(Year = models_for_ms$WESA.ad.est$null$frame$Year %>% unique() %>% as.character) %>%
  mutate(
    ad.wesa = ranef(models_for_ms$WESA.ad.est$null)$cond$`Year.factor` %>% .[, "(Intercept)"],
    
    ad.lesa = ranef(models_for_ms$LESA.ad.est$null)$cond$`Year.factor` %>% .[, 1]) %>% 
  full_join(tibble(Year = models_for_ms$WESA.juv.est$null$frame$Year %>% unique() %>% as.character) %>%
              mutate(
    juv.wesa = ranef(models_for_ms$WESA.juv.est$null)$cond$`Year.factor` %>% .[, 1],
    juv.lesa = ranef(models_for_ms$LESA.juv.est$null)$cond$`Year.factor` %>% .[, 1]
  ), by = "Year") %>% mutate(Year = as.numeric(Year))




doy.models <- yrs.doys %>%
  group_by(DOY) %>%
  summarize_at(vars(pred.wesa.ad, pred.wesa.juv, pred.lesa.ad, pred.lesa.juv), .funs = quantile, probs = 0.5)



pred.doys_dat <- tibble(
  Year.factor = as.factor(2041),
  DOY = seq(range(sidney$DOY)[1], range(sidney$DOY)[2])) %>%
  mutate(   Year = 2041,
    DOY_rescaled_a = (DOY - mean_doy_a) / (2 * sd_doy_a),
    DOY_rescaled_j = (DOY - mean_doy_j) / (2 * sd_doy_j),
    DOY_rescaled_a_lesa = (DOY - mean_doy_a_lesa) / (2 * sd_doy_a_lesa),
    DOY_rescaled_j_lesa = (DOY - mean_doy_j_lesa) / (2 * sd_doy_j_lesa),
    Yr_a = 0, Yr_j = 0  ) 
pred.doys <- pred.doys_dat %>%
  mutate(
    pred.wesa.ad = splititmod(models_for_ms$WESA.ad.est$null,
                           T, dat =
                             dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a)#resp=NULL
    ),
    pred.wesa.juv = splititmod(models_for_ms$WESA.juv.est$null,
                           F, dat =
                              dplyr::rename(., DOY_rescaled = DOY_rescaled_j, Yr = Yr_j)
    ),
    pred.lesa.ad = splititmod(models_for_ms$LESA.ad.est$null,
                          F, dat =
                             dplyr::rename(., DOY_rescaled = DOY_rescaled_a_lesa, Yr = Yr_a)
    ),
    pred.lesa.juv = splititmod(models_for_ms$LESA.juv.est$null,
                            F, dat =
                              dplyr::rename(., DOY_rescaled = DOY_rescaled_j_lesa, Yr = Yr_j)
    ),
    se.a.w  = splititmod(models_for_ms$WESA.ad.est$null,
                     T, dat =
                        dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a), "se.fit" ),
    se.j.w  = splititmod(models_for_ms$WESA.juv.est$null,
                         T, dat =
                           dplyr::rename(., DOY_rescaled = DOY_rescaled_j, Yr = Yr_j), "se.fit" ),
    se.a.l  = splititmod(models_for_ms$LESA.ad.est$null,
                         T, dat =
                           dplyr::rename(., DOY_rescaled = DOY_rescaled_a, Yr = Yr_a), "se.fit" ),
    se.j.l  = splititmod(models_for_ms$LESA.juv.est$null,
                         T, dat =
                           dplyr::rename(., DOY_rescaled = DOY_rescaled_j, Yr = Yr_j), "se.fit" )
  )



# GAM predictions ---------------------------------------------------------


# require(splines)
require(cowplot)

adult_gam_predict <- tibble(Year_all = 1990:2018, DOY_rescaled = 0, Year.factor = as.factor(1990)) %>% 
  mutate(Year = (Year_all - mnyr_A) /(2*sd_yr_A)) 
juv_gam_predict <- tibble(Year_all = 1990:2018, DOY_rescaled = 0, Year.factor = as.factor(1990)) %>% 
  mutate(Year = (Year_all - mnyr_J) /(2*sd_yr_J)) 


pred.full_dat <- as_tibble(expand.grid(
  Year_all = 1990:2018, 
  DOY = seq(range(sidney$DOY)[1], range(sidney$DOY)[2])) ) %>%
  mutate(   Year.factor = as.factor(1990),
    Year = (Year_all - mnyr_A) /(2*sd_yr_A),
            DOY_rescaled_a = (DOY - mean_doy_a) / (2 * sd_doy_a),
            DOY_rescaled_j = (DOY - mean_doy_j) / (2 * sd_doy_j),
            DOY_rescaled_a_lesa = (DOY - mean_doy_a_lesa) / (2 * sd_doy_a_lesa),
            DOY_rescaled_j_lesa = (DOY - mean_doy_j_lesa) / (2 * sd_doy_j_lesa) ) 





pred_a_gam_wesa <-   predict(models_for_ms$WESA.ad.est$null,#gam, 
                             newdata = pred.full_dat %>%
                               mutate(DOY_rescaled = DOY_rescaled_a), 
                             allow.new.levels = T, se=T)
pred.full_dat$.pred_wesa <- pred_a_gam_wesa$fit
pred.full_dat$.se_wesa <- pred_a_gam_wesa$se.fit


pred_j_gam_wesa <-   predict(models_for_ms$WESA.juv.est$null,#gam, 
                             newdata = pred.full_dat %>%
                               mutate(DOY_rescaled = DOY_rescaled_j), 
                             allow.new.levels = T, se=T)
pred.full_dat$.pred_wesa_j <- pred_j_gam_wesa$fit
pred.full_dat$.se_wesa_j <- pred_j_gam_wesa$se.fit

pred_a_gam_lesa <-   predict(models_for_ms$LESA.ad.est$yr2,#gam, 
                             newdata = pred.full_dat %>%
                               mutate(DOY_rescaled = DOY_rescaled_a_lesa), 
                             allow.new.levels = T, se=T)
pred.full_dat$.pred_lesa <- pred_a_gam_lesa$fit
pred.full_dat$.se_lesa <- pred_a_gam_lesa$se.fit

pred_j_gam_lesa <-   predict(models_for_ms$LESA.juv.est$gam, 
                             newdata = pred.full_dat %>%
                               mutate(DOY_rescaled = DOY_rescaled_j_lesa), 
                             allow.new.levels = T, se=T)
pred.full_dat$.pred_lesa_j <- pred_j_gam_lesa$fit
pred.full_dat$.se_lesa_j <- pred_j_gam_lesa$se.fit

sum_pred_gam <- pred.full_dat %>% 
  filter(DOY <= quant_a[[2]] ) %>% 
  dplyr::select(Year_all, .pred_wesa,.se_wesa, .pred_lesa, .se_lesa) %>% 
  group_by(Year_all) %>% 
  summarize_all(mean)

sum_pred_gam_j <- pred.full_dat %>% 
  filter(DOY > quant_j[[1]] ) %>% 
  dplyr::select(Year_all, .pred_wesa_j,.se_wesa_j, .pred_lesa_j, .se_lesa_j) %>% 
  group_by(Year_all) %>% 
  summarize_all(mean)




