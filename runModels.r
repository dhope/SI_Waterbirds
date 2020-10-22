### Run models Sidney Island Analysis 2018
# David Hope, based on scripts from
# Anna Drake and Mark Drever
###########################


require(tidyverse)
# require(doBy)
# require(MASS)
require(glmmTMB)



source("R/RunClimateModels.r")

# Run all model variants   -------------------------------------------------------------


fullModel_run_ZN_A <- climateModelRun(non_id_limit = 0.8, return_type = 'models')
write_rds(fullModel_run_ZN_A, "output/models_for_ms.rds")
fullModel_run_90quant_doy <- climateModelRun(non_id_limit = 0.8, return_type = 'models')
fullModel_run_noZI <- climateModelRun(non_id_limit = 0.8, return_type = 'models',
                                      zigrep = "ThereWillbeNoZI")
fullModel_run_nbin1 <- climateModelRun(non_id_limit = 0.8, return_type = 'models',
                                       zigrep = "ThereWillbeNoZI",
                                       fam = nbinom1)
fullModel_run_poisson <- climateModelRun(non_id_limit = 0.8, return_type = 'models',
                                       zigrep = "ThereWillbeNoZI",
                                       fam = poisson)
fullModel_run_ran_slope <- climateModelRun(non_id_limit = 0.8, return_type = 'models',
                                           ran_var = "(DOY_rescaled + I(DOY_rescaled^2) | Year.factor)")
write_rds(fullModel_run_ran_slope, "output/random_slope_models.rds")
write_rds(list(
  ranslope = fullModel_run_ran_slope,
  quant90doy = fullModel_run_90quant_doy,
  NB2_ZI_wesa_adults = fullModel_run_ZN_A,
               NB2=fullModel_run_noZI,
               NB1=fullModel_run_nbin1,
               poisson = fullModel_run_poisson), "output/model_dist_comparisons.rds")