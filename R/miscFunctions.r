require(tidyverse)
require(cowplot) # Not required but I used it for the plots
require(lubridate)
require(lme4) # Only for the ageclassModel.r
require(glmmTMB)
library(magrittr)
require(readxl)
require(splines)


y_vars <- list("WESA.ad.est", "WESA.juv.est", "LESA.ad.est", "LESA.juv.est")
spID <- tibble(var =unlist(y_vars), Species = c(rep("Western Sandpipers", 2), rep("Least Sandpipers", 2)), 
               Age = rep(c("Adults", "Juveniles"),2))
hyp <- read_csv("data_/Hyp_key.csv")



# Function to convert from logit to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#here we basically get a merMod object and return the fitted values
predFun<-function(.) mm%*%fixef(.)

calcmodAIC <- function(modlist, name_, gamonly = F){
  if(name_ %in% names(modlist))  mod <- modlist[[name_]]
  else mod <- modlist
  if(isTRUE(gamonly)) {mod <- mod[grepl("gam", names(mod))] } else{
  mod <- mod[!names(mod) %in% c("a","global")]
  # if(grepl("WESA.juv.", name_)) mod <- mod[names(mod) == "gam5" | !grepl("gam", names(mod))]
  # if(grepl("LESA.juv.", name_)) mod <- mod[names(mod) == "gam5" | !grepl("gam", names(mod))]
  # if(grepl("WESA.ad.", name_)) mod <- mod[names(mod) == "gam4" | !grepl("gam", names(mod))]
  # if(grepl("LESA.ad.", name_)) mod <- mod[names(mod) == "gam8" | !grepl("gam", names(mod))]
  mod <- mod[names(mod) == "gam" | !grepl("gam", names(mod))]
  }
aicRes <- 
  cbind(sapply(mod, function(x) bbmle::AICc(x,k = 2,nobs = nobs(x) )) )%>% 
  # sapply(mods, stats::AIC) %>% 
  as.data.frame() %>% 
  rownames_to_column("mnames") %>% 
  rename(AICc=V1) %>% 
  filter(!is.na(AICc))
dfs <-  mod %>% bbmle::AICctab(., mnames = names(.),logLik=T,
                               base=T,
                               weights = T)
aicResTab <- aicRes %>%  
  arrange(AICc) %>% 
  mutate(logL = dfs$logLik[!is.na(dfs$logLik)],
         dAICc = AICc - min(AICc),
         logSup = exp(-0.5*dAICc),
         w = logSup / sum(logSup),
         df =dfs$df[!is.na(dfs$logLik)],
         group = name_) 
return(aicResTab)
}



## Check glmmTMB model assumptions from Ben Bolker
# Some fuctions not yet working (commented out)
# from https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/vignettes/model_evaluation.rmd
checkassumptions <- function(mod){
require(DHARMa)
require(car)
require(effects)
require(multcomp)
require(broom.mixed)
require(dotwhisker)
  glht_glmmTMB <- function (model, ..., component="cond") {
    glht(model, ...,
         coef. = function(x) fixef(x)[[component]],
         vcov. = function(x) vcov(x)[[component]],
         df = NULL)
  }
  modelparm.glmmTMB <- function (model, coef. = function(x) fixef(x)[[component]],
                                 vcov. = function(x) vcov(x)[[component]],
                                 df = NULL, component="cond", ...) {
    multcomp:::modelparm.default(model, coef. = coef., vcov. = vcov.,
                                 df = df, ...)
  }
# mod <- fullModel_run$WESA.ad.est$PNA
nb2_simres <- simulateResiduals(mod)
# plot(nb2_simres)
# Anova(fullModel_run$WESA.ad.est$PNA)  ## default type II
# Anova(fullModel_run$WESA.ad.est$PNA,type="III")
# dat_i <- dat_a
# plot(allEffects(fullModel_run$WESA.ad.est$null))
# emmeans::emmeans(fullModel_run$WESA.ad.est$null )
dr_res <- drop1(mod,test="Chisq")
g1 <- glht_glmmTMB(mod, PNA = mcp(period = "Tukey"))
summary(g1)
select <- dplyr::select
(t1 <- broom.mixed::tidy(mod, conf.int = TRUE))
if (packageVersion("dotwhisker")>"0.4.1") {
  ## to get this version (which fixes various dotwhisker problems)
  ## use devtools::install_github("bbolker/broom.mixed") or
  ## wait for pull request acceptance/submission to CRAN/etc.
  plotout2 <- dwplot(mod)+geom_vline(xintercept=0,lty=2)
} else {dev.new()
  mod$coefficients <- TRUE  ## hack!
  plotout2 <- dwplot(mod,by_2sd=FALSE)+geom_vline(xintercept=0,lty=2) 
}
return(list(nb2_simres = nb2_simres,dr_res=dr_res,g1=g1,t1=t1,plotout2=plotout2))
}


splititmod <- function(mod, allownlev, dat, returnvar='fit'){
  df_ <- attributes(bbmle::logLik(mod))$df
  Qt <- c(-1, 1) * qt((1 - 0.95) / 2, df_, lower.tail = FALSE)
  tmp <- predict(mod, allow.new.levels=allownlev, newdata = dat,
                 se.fit=T)
  CI <- tmp$fit + outer(tmp$se.fit, Qt)
  colnames(CI) <- c("lwr", "upr")
  outdf <- tibble(fit = tmp$fit, se.fit = tmp$se.fit, lwr = CI[,1], upr = CI[,2])
  if(returnvar=='all') return(outdf)
  else return(outdf[[returnvar]])
}