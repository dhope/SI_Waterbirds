##########################
# David Hope
# October 2018
# Models analyzing SI count data
#############################




# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

runModel <- function(dat, base_f, v, fam = nbinom2, zi_f = "~1", ranvars = " (1 | Year.factor)") {
  require(tidyverse)
  require(glmmTMB)
  assign("last.warning", NULL, envir = baseenv())
  op <- options("warn")
  on.exit(options(op))
  options(warn = 1)
  f <- ifelse(is.na(v), base_f, paste(base_f, "+", v))
  f <- paste(f, "+ ", ranvars, collapse = "")
  cat("Model - ", v, "--- ", f, "\n\r")
  
  if (is.na(v) | !v %in% names(dat)) {
    dat_i <- dat
  } else {
    dat_i <- dat %>% mutate_at(v, .funs = arm::rescale)
  }
  
  
  if (!is.na(v) & v == "u*v") {
    dat_i <- dat %>% mutate_at(vars(u, v), .funs = arm::rescale)
  }
  # i}
  # print(dat_i[,v])
  if (is.na(zi_f)) {
    mod <- glmmTMB(as.formula(f),
                   data = dat_i,
                   REML = F,
                   family = fam
    )
  } else {
    mod <- glmmTMB(as.formula(f),
                   data = dat_i,
                   ziformula = as.formula(zi_f), REML = F,
                   family = fam
    )
  }
  # print(overdisp_fun(mod))
  cat("Converged:", mod$fit$convergence, " - Iterations: ", mod$fit$iterations, "\n\r")
  print(warnings())
  print("--------------------------")
  
  return(mod)
}




run_full_model_set <- function(dat_, base_form, vars_, age_var,
                               ranvar = " (1 | Year.factor)",
                               fam=nbinom2,
                               zigrep = "WESA.ad") {
  require(tidyverse)
  require(glmmTMB)
  # dat_ <- group_by(dat_, Year.factor) %>% mutate(nrows=n()) %>% filter(nrows>1) %>% ungroup
  base_f <- paste(age_var, base_form, collapse = "")
  # if (grepl("LESA", age_var)) {
    fam_ <- fam
  zi_f <- NA#}
  if (grepl(zigrep, age_var)) {fam_ <- fam
  zi_f <- "~1"}
  models_output <- map(vars_, runModel, base_f = base_f, dat = dat_, fam = fam_, zi_f = zi_f, ranvars = ranvar)
  names(models_output) <- vars_
  models_output[["null"]] <- runModel(dat_, base_f, v = NA, fam = fam_, zi_f = zi_f, ranvars = ranvar) # glmmTMB(as.formula(base_f), data = dat_a, family=nbinom2)
  models_output[["u*v"]] <- runModel(dat_ %>% mutate_at(vars(u, v), arm::rescale), base_f = base_f, v = "u*v", fam = fam_, zi_f = zi_f,ranvars = ranvar )
  models_output[["yr2"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ Year + I(Year**2)"), v = NA, fam = fam_, zi_f = zi_f, ranvars = ranvar)
  # require(splines)
  # models_output[["gam3"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=3)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam4"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=4)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam5"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=5)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam6"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=6)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam7"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=7)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam8"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=8)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam9"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=9)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam10"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=10)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam11"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=11)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  # models_output[["gam12"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year)), paste(base_f, "+ ns(Year, df=11)"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  require(mgcv)
  k_df <- tibble(a = c("WESA.ad.est", "WESA.juv.est", "LESA.ad.est", "LESA.juv.est"), k= c(6,24,19,7))
  k_ <- k_df$k[k_df$a == age_var]
  models_output[["gam"]] <- gam(as.formula(paste(age_var, "~ DOY_rescaled + I(DOY_rescaled^2) +  s(Year, bs='tp', k = ",k_,") ", collapse = "")),
                                  family = nb,#random = list(Year.factor=~1),
                                  data =  dat_ %>% mutate(Year = arm::rescale(Year)) )
  # models_output[["break"]] <- runModel(dat_ %>% mutate(Year = arm::rescale(Year), 
  #                                                      time_group = ifelse(as.numeric(as.character(Year.factor)) < 2002, "Early", "Late")
  #                                                      ), 
  #                                      paste(base_f, "+ Year*time_group"), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  
  models_output[["global"]] <- runModel(dat_ %>% mutate_at(vars(vars_), arm::rescale), paste(c(base_f, vars_), collapse = " + "), v = NA, fam = fam_, zi_f = zi_f, ranvars = ranvar)
  if (grepl("juv", age_var)) {
    models_output[["a"]] <- runModel(dat_, base_f, v = "adults", fam = fam_, zi_f = zi_f, ranvar)
  }
  v_local <- c("Temp", "u*v", "Total_Precip")
  
  models_output[["global_local"]] <- runModel(dat_ %>% mutate_at(vars(vars_), arm::rescale), paste(c(base_f, v_local), collapse = " + "), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  v_breeding <- c("TMIN_May", "TMIN_June", "max_snowmelt")
  models_output[["global_breeding"]] <- runModel(dat_ %>% mutate_at(vars(vars_), arm::rescale), paste(c(base_f, v_breeding), collapse = " + "), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  v_global <- c("ALPI", "PNA", "PDO", "AO")
  models_output[["global_global"]] <- runModel(dat_ %>% mutate_at(vars(vars_), arm::rescale), paste(c(base_f, v_global), collapse = " + "), v = NA, fam = fam_, zi_f = zi_f, ranvar)
  
  return(models_output)
}




calc_tidy <- function(mod,name, group_ )
{
  broom.mixed::tidy(mod, conf.int = TRUE ) %>% 
    mutate(group = group_, model_name=name)
  
}

calc_group <- function(spp, model_list){
  pmap_df(tibble(mod =model_list[[spp]], name=names(model_list[[spp]]), group_ = spp),
          calc_tidy)
}