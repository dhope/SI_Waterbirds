## Run and return the models or just their coefficients

## Model to run all the climate variable models for all species ---------
climateModelRun <- function(non_id_limit, return_type = "both",ran_var = "(1|Year.factor)",
                            fam=nbinom2,
                            zigrep = "WESA.ad", prepdata = F) {
  require(glmmTMB)
  require(tidyverse)
  source("R/SI_Climate_Models.r")

  ## Import data ----------------
  sidney <- read_rds("output/Sidney_1990_2018.rds") %>% 
    #read_csv("output/Sidney_1990_2018_Final_Nov6_2018.csv", col_types = cols()) %>%
    mutate(
      Yr = arm::rescale(Year),
      Year.factor = as.factor(Year),

      non.id = replace_na(non.id, 0)
    ) %>%
    filter(non.id <= non_id_limit)

  quants <- read_rds("output/clipping_dates.rds")
  yrsinAnalysis <- read_rds("output/yearsInAnalysis.rds")
  sidney_adults <- sidney %>%
    filter(DOY <= quants[[1]][[2]]& Year %in% yrsinAnalysis$adults) %>% # <231
    mutate(DOY_rescaled = arm::rescale(DOY)) #%>% filter(WESA.ad.est < 4000)
  sidney_juveniles <- sidney %>%
    filter(DOY > quants[[2]][[1]]& Year %in% yrsinAnalysis$juv) %>%
    mutate(DOY_rescaled = arm::rescale(DOY))

  sidney_adults_lesa <- sidney %>%
    filter(DOY <= quants[[3]][[2]]& Year %in% yrsinAnalysis$adults) %>% # <231
    mutate(DOY_rescaled = arm::rescale(DOY))
  sidney_juveniles_lesa <- sidney %>%
    filter(DOY > quants[[4]][[1]]& Year %in% yrsinAnalysis$juv) %>%
    mutate(DOY_rescaled = arm::rescale(DOY))

  
  climate_covariates <- read_rds("output/modelcovariates_all.rds")

  ### Manipulate data for analysis ---------------
  vars_ <- names(climate_covariates)[!names(climate_covariates) %in% c(
    "Month",
    "days_above_zero_tmin_june",
    "days_above_zero_june",
    "TMAX_June",
    "days_above_zero_may",
    "days_above_zero_tmin_may",
    "TMAX_May",
    "min_snowmelt",
    "NPI",
    "EPNP"
  )]
  dat_a <- left_join(sidney_adults, climate_covariates[climate_covariates$Month == 7, ], by = "Year")  
  dat_j <- left_join(sidney_juveniles, climate_covariates[climate_covariates$Month == 8, ], by = "Year") %>%
    mutate(adults = runif(nrow(.), 1, 34))
  dat_a_lesa <- left_join(sidney_adults_lesa, climate_covariates[climate_covariates$Month == 7, ], by = "Year") 
  dat_j_lesa <- left_join(sidney_juveniles_lesa, climate_covariates[climate_covariates$Month == 8, ], by = "Year") %>%
    mutate(adults = runif(nrow(.), 1, 34))  
  if(isTRUE(prepdata)) {return(list(dat_a = dat_a, dat_j = dat_j, dat_a_lesa= dat_a_lesa, dat_j_lesa=dat_j_lesa))}
  ### Run Models -----
  ##### Run all the models described in SI_Climate_Models.r  ---------

  base_f <- "~ DOY_rescaled + I(DOY_rescaled^2)" #+ (1|Year.factor)"
  y_vars <- list("WESA.ad.est", "WESA.juv.est", "LESA.ad.est", "LESA.juv.est")
  all_spp <- tibble(dat_ = list(dat_a, dat_j, dat_a_lesa, dat_j_lesa), age_var = y_vars)

  fullModel_run <- pmap(all_spp, run_full_model_set, base_form = base_f, vars_ = vars_, ranvar = ran_var,
                        fam=fam, zigrep=zigrep)
  names(fullModel_run) <- y_vars
  
  # write_rds(names(fullModel_run$WESA.ad.est), "output/modelNames.rds")
  if(!grepl("DOY_rescaled", ran_var)) {
  ## Extract adult null random intercepts
  ranneffe_adults_W <- ranef(fullModel_run$WESA.ad.est$null) %>%
    .[["cond"]] %>%
    .[["Year.factor"]] %>%
    rownames_to_column("Year") %>%
    mutate_all(as.numeric) %>%
    rename(adults = `(Intercept)`)

  # Pull out annual random intercept effects from adult null model
  ranneffe_adults_l <- ranef(fullModel_run$LESA.ad.est$null) %>%
    .[["cond"]] %>%
    .[["Year.factor"]] %>%
    rownames_to_column("Year") %>%
    mutate_all(as.numeric) %>%
    rename(adults = `(Intercept)`)


  # Refit juvenile - adult model
  # fam <- fam
  dat_i <- dat_j %>% select(-adults) %>% left_join(ranneffe_adults_W, by = "Year")
  fullModel_run$WESA.juv.est$a <- update(object = fullModel_run$WESA.juv.est$a, data = dat_i)
  dat_i <- dat_j_lesa %>%
    select(-adults) %>%
    left_join(ranneffe_adults_l, by = "Year")
  fullModel_run$LESA.juv.est$a <- update(object = fullModel_run$LESA.juv.est$a, data = dat_i)
  # rm(fam)
  rm(dat_i)
  }
  if (return_type %in% c("both", "coef")) {
    model_coefficients_all <- map_df(y_vars, calc_group, fullModel_run) %>% mutate(non_id = non_id_limit)
  }
  if (return_type == "both") return(list(models = fullModel_run, coef = model_coefficients_all))
  if (return_type == "coef") return(model_coefficients_all)
  if (return_type == "models") return(fullModel_run)
}
