### Analyse models Sidney Island Analysis 2018
# David Hope, based on scripts from
# Anna Drake and Mark Drever
###########################
## ---- AIC
require(tidyverse)
require(glmmTMB)
source("R/RunClimateModels.r")
source("R/miscFunctions.r")
source("R/CV.r")

fullresults_all_dist <- read_rds("output/model_dist_comparisons.rds")
# models_for_ms <- fullresults_all_dist$NB2_ZI_wesa_adults
models_for_ms <- read_rds("output/models_for_ms.rds")

# AIC tables  ------------

# aicTables_gams <- map_df(y_vars, calcmodAIC, modlist=models_for_ms, gamonly=T) %>% left_join(hyp, by = c("mnames"="model_name"))
aicTables <- map_df(y_vars, calcmodAIC, modlist=models_for_ms) %>% left_join(hyp, by = c("mnames"="model_name"))
write_rds(aicTables, "output/aicTables.rds")
write_excel_csv(aicTables,"output/Appendix3.csv")

aicTables %>% 
  mutate(hypothesis = ifelse(hypothesis=="Baseline", "Interannual", hypothesis)) %>% 
  group_by(group, hypothesis) %>%
  summarize(w = sum(w), .groups = 'drop') %>% 
  separate(group, sep = "\\.", into = c("spp", "age", "est")) %>% 
  pivot_wider(names_from = c(spp, age), values_from = w) %>% 
  dplyr::select(-est) 
  

## ---- CV
# Cross validation --------------------------------------------------------


# aicTables$mnames[aicTables$dAICc<2]
dat_all <- climateModelRun(non_id_limit = 0.8, prepdata = T)
names(dat_all) <- y_vars

##  Baseline models

runtibble <- tibble(Species = c(rep("WESA", 2), rep("LESA", 2)),
                    Age = rep(c("Adults", "Juveniles"),2),
                    model_name = "null",
                    varname = unlist(y_vars)
                    ) %>%
  mutate(mod = map2(.x = varname, .y = model_name, ~models_for_ms[[.x]][[.y]]),
         dat = map(varname, function(x) dat_all[[x]])) %>%
  mutate(
    cvres = pmap(.l = ., .f= cv_function, return_df=T))
cv_null <- runtibble %>% select(-mod, -dat) %>% unnest %>% select(-Species1, -Age1,-model_name1) %>%
  group_by(Species, Age, model_name, varname) %>% summarize_all(mean)
 write_excel_csv(cv_null, "output/Table1.csv")

## Top models

topmodruntibble <- tibble(Species = c(rep("WESA", 4), rep("LESA", 2)),
                    Age = c(rep("Adults",3), "Juveniles",rep("Adults", 1), "Juveniles"),
                            model_name = aicTables$mnames[aicTables$dAICc<2],
                    varname = aicTables$group[aicTables$dAICc<2]) %>% 
  # filter(varname == "LESA.juv.est") %>% 
  mutate(mod = map2(.x = varname, .y = model_name, ~models_for_ms[[.x]][[.y]]),
         dat = map(varname, function(x) dat_all[[x]])) %>% 
  # select(-model_name, -varname) %>% 
  mutate(
    cvres = pmap(.l = ., .f= cv_function, return_df=T))
write_rds(topmodruntibble %>% select(-dat, -mod), "output/topmodel_cv_revisions.rds")
scv_top_models <- topmodruntibble %>% select(-mod, -dat) %>% unnest %>% select(-Species1, -Age1, -model_name1) %>% 
  group_by(Species, Age, model_name, varname) %>% summarize_all(mean)
write_excel_csv(scv_top_models, "output/Table2.csv")

## ---- Assumptions
### Check model assumptions  ----- 
## See miscFunctiuons for function details

a_check <- checkassumptions(models_for_ms$WESA.ad.est$PNA)

a_check$nb2_simres %>% plot

testTemporalAutocorrelation(simulationOutput = a_check$nb2_simres, time = sidney_adults$DOY_rescaled)

summary(a_check$g1)


j_check <- checkassumptions(fullresults_all_dist$NB2$WESA.juv.est$TMIN_May)
j_check$nb2_simres %>% plot
summary(j_check$g1)
j_check$dr_res
j_check$plotout2


L_a_check <- checkassumptions(fullModel_run$LESA.ad.est$ALPI)
L_a_check$nb2_simres %>% plot
summary(L_a_check$g1)



L_j_check <- checkassumptions(models_for_ms$LESA.juv.est$max_snowmelt)
L_j_check$nb2_simres %>% plot
summary(L_j_check$g1)



